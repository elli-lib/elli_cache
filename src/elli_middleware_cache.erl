%%% ============================================== [ elli_middleware_cache.erl ]
%%% @doc Generic caching middleware.
%%% @end
%%% ============================================================================

-module(elli_middleware_cache).

-export([postprocess/3]).

-spec postprocess(Req, Result, Config) -> Result when
      Req :: elli:req(),
      Result :: elli_handler:result(),
      Config :: [module()].
postprocess(Req, {ResponseCode, Body}, Config)
  when ok =:= ResponseCode orelse 200 =:= ResponseCode ->
    postprocess(Req, {ResponseCode, [], Body}, Config);
postprocess(Req, {ResponseCode, Headers, Body} = Res, Config)
  when ok =:= ResponseCode orelse 200 =:= ResponseCode ->
    case elli_cache:modified(Req, Config) of
        false ->
            Res;
        Mtime ->
            NewHeaders = etag(Mtime, last_modified(Mtime, Headers), Body),
            NewRes = setelement(2, Res, NewHeaders),
            RequestHeaders = elli_request:headers(Req),
            NoneMatch = none_match(RequestHeaders, NewHeaders),
            case NoneMatch andalso modified_since(RequestHeaders, NewHeaders) of
                true ->
                    NewRes;
                false ->
                    {304, prune_headers(NewHeaders), <<>>}
            end
    end;
postprocess(_, Res, _) ->
    Res.

%%% ===================================================== [ Internal functions ]

-spec none_match(elli:headers(), elli:headers()) -> boolean().
none_match(RequestHeaders, ResponseHeaders) ->
    NoneMatch = none_match(RequestHeaders),
    ETag = etag(ResponseHeaders),
    do_none_match(NoneMatch, ETag).

do_none_match(undefined, _) -> true;
do_none_match(_, undefined) -> true;
do_none_match(NoneMatch, ETag) -> NoneMatch /= ETag.

-spec modified_since(elli:headers(), elli:headers()) -> boolean().
modified_since(RequestHeaders, ResponseHeaders) ->
    ModifiedSince = modified_since(RequestHeaders),
    LastModified = last_modified(ResponseHeaders),
    do_modified_since(ModifiedSince, LastModified).

do_modified_since(undefined, _) -> true;
do_modified_since(_, undefined) -> true;
do_modified_since(ModifiedSince, LastModified) ->
    case {convert_date(LastModified), convert_date(ModifiedSince)} of
        {bad_date, _} ->
            true;
        {_, bad_date} ->
            true;
        {Modified, Since} ->
            Modified > Since
    end.

-spec convert_date(binary()) -> calendar:datetime() | bad_date.
convert_date(Bin) ->
    httpd_util:convert_request_date(binary_to_list(Bin)).

prune_headers(Headers) ->
    Fun = case proplists:is_defined(<<"ETag">>, Headers) of
              true  -> fun allowed_header/1;
              false ->
                  fun({<<"Last-Modified">>, _}) ->
                          true;
                     (H) ->
                          allowed_header(H)
                  end
          end,
    lists:filter(Fun, Headers).

allowed_header({<<"Cache-Control">>, _}) -> true;
allowed_header({<<"Content-Location">>, _}) -> true;
allowed_header({<<"Date">>, _}) -> true;
allowed_header({<<"ETag">>, _}) -> true;
allowed_header({<<"Expires">>, _}) -> true;
allowed_header({<<"Vary">>, _}) -> true;
allowed_header(_) -> false.

%%% ================================================================ [ Setters ]

-spec etag(calendar:datetime(), elli:headers(), elli:body()) -> elli:headers().
etag(LastModified, Headers, Body) ->
    ETag = httpd_util:create_etag(LastModified, iolist_size(Body)),
    [{<<"ETag">>, iolist_to_binary(ETag)} | Headers].

-spec last_modified(calendar:datetime(), elli:headers()) -> elli:headers().
last_modified(LastModified, Headers) ->
    LastModBin = list_to_binary(httpd_util:rfc1123_date(LastModified)),
    [{<<"Last-Modified">>, LastModBin} | Headers].

%%% ================================================================ [ Getters ]

-spec etag(elli:headers()) -> undefined | binary().
etag(Headers) ->
    proplists:get_value(<<"ETag">>, Headers).

-spec last_modified(elli:headers()) -> undefined | binary().
last_modified(Headers) ->
    proplists:get_value(<<"Last-Modified">>, Headers).

-spec modified_since(elli:headers()) -> undefined | binary().
modified_since(Headers) ->
    proplists:get_value(<<"If-Modified-Since">>, Headers).

-spec none_match(elli:headers()) -> undefined | binary().
none_match(Headers) ->
    proplists:get_value(<<"If-None-Match">>, Headers).

%%% ==================================================================== [ EOF ]
