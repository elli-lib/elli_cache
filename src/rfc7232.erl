%%% ============================================================ [ rfc7232.erl ]
%%% @doc IETF RFC 7232 implementation.
%%% @author Eric Bailey <eric@ericb.me> [https://github.com/yurrriq]
%%% @copyright 2016, elli-lib team
%%% @reference <a href="https://tools.ietf.org/html/rfc7232">IETF RFC 7232</a>
%%% @end
%%% ==================================================================== [ EOH ]
-module(rfc7232).

%% API.
-export([init/3, init/4]).

-import(proplists, [delete/2]).
-import(elli_cache_util,
        [compare_date/3,
         maybe_get_value/2, get_values/2, ifdef_delete/3, store/3,
         update_element/3]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-include("elli_cache_util.hrl").
-include_lib("elli/include/elli.hrl").

%%% ================================================================== [ Types ]

-export_type([etag/0, result/0]).

%% @type etag() = binary().
%% An <a href="https://tools.ietf.org/html/rfc7232#section-2.3">ETag</a> is a
%% binary.
-type etag() :: binary().

%% @type result() = elli:req() | elli_handler:result().
%% A result is an Elli request or `elli_handler' result, i.e. an HTTP response.
-type result() :: elli:req() | elli_handler:result(). % | no_return().

%% Internal state.
-type state() :: #{req   => elli:req(),
                   mtime => maybe_m:maybe(calendar:datetime()),
                   etag  => maybe_m:maybe(etag()),
                   res   => elli_handler:result()
                  }.

%%% ==================================================================== [ API ]

%% not('GET' | 'HEAD')
-spec init(Req, Mtime, ETag) -> result() | no_return() when
      Req   :: elli:req(),
      Mtime :: calendar:datetime(),
      ETag  :: maybe_m:maybe(etag()).
init(Req, Mtime, ETag) ->
    State = #{req => Req, mtime => Mtime, etag => ETag},
    if_match(State).

-spec init(Req, Mtime, ETag, Res) -> elli_handler:result() when
      Req   :: elli:req(),
      Mtime :: maybe_m:maybe(calendar:datetime()),
      ETag  :: maybe_m:maybe(etag()),
      Res   :: elli_handler:result().
init(Req, Mtime, ETag, Res) ->
    State = #{req => Req, mtime => Mtime, etag => ETag, res => Res},
    if_match(State).

%%% ===================================================== [ 2.3.2.  Comparison ]

%% %% @doc Curried version of {@link compare_strong/2}.
-spec compare_strong(etag()) -> fun((etag()) -> boolean()).
compare_strong(ETag1) -> fun (ETag2) -> compare_strong(ETag1, ETag2) end.

-spec compare_strong(etag(), etag()) -> boolean().
compare_strong(_ETag1, <<"*">>) ->
    true;
compare_strong(ETag1, ETag2)
  when not (?WEAK(ETag1) orelse ?WEAK(ETag2)) ->
    ETag1 =:= ETag2;
compare_strong(_ETag1, _ETag2) ->
    false.

%% @doc Curried version of {@link compare_weak/2}.
-spec compare_weak(etag()) -> fun((etag()) -> boolean()).
compare_weak(ETag1) -> fun(ETag2) -> compare_weak(ETag1, ETag2) end.

-spec compare_weak(etag(), etag()) -> boolean().
compare_weak(_ETag1, <<"*">>) ->
    true;
compare_weak(<<"W/", ETag1/binary>>, <<"W/", ETag2/binary>>) ->
    ETag1 =:= ETag2;
compare_weak(<<"W/", ETag1/binary>>, ETag2) ->
    ETag1 =:= ETag2;
compare_weak(ETag1, <<"W/", ETag2/binary>>) ->
    ETag1 =:= ETag2;
compare_weak(ETag1, ETag2) ->
    ETag1 =:= ETag2.

%%% ========================================================= [ 3.1.  If-Match ]
%%% NOTE: "The If-Match header field can be ignored by caches
%%% and intermediaries because it is not applicable to a stored response."

%% @doc If-Match
-spec if_match(state()) -> result().
if_match(#{req := #req{headers = RequestHeaders}} = State) ->
    do_if_match(State, get_if_match(RequestHeaders)).

-spec do_if_match(state(), [etag()]) -> result() | no_return().
do_if_match(State, []) ->
    unmodified_since(State);
do_if_match(#{etag := nothing}, _Etags) ->
    precondition_failed();
do_if_match(#{etag := _JustETag} = State, [<<"*">>]) ->
    unmodified_since(State);
do_if_match(#{etag := {just, ETag}} = State, ETags) ->
    ?IF(lists:any(compare_strong(ETag), ETags),
        unmodified_since(State),
        %% FIXME: ... the origin server MUST respond with either a) the 412
        %% (Precondition Failed) status code or b) one of the 2xx (Successful)
        %% status codes if the origin server has verified that a state change is
        %% being requested and the final state is already reflected in the
        %% current state of the target resource (i.e., the change requested by
        %% the user agent has already succeeded, but the user agent might not be
        %% aware of it, perhaps because the prior response was lost or a
        %% compatible change was made by some other user agent). In the latter
        %% case, the origin server MUST NOT send a validator header field in the
        %% response unless it can verify that the request is a duplicate of an
        %% immediately prior change made by the same user agent.
        precondition_failed()).

-spec get_if_match(elli:headers()) -> [etag()].
get_if_match(Headers) ->
    get_values(<<"If-Match">>, Headers).

%%% ==================================================== [ 3.2.  If-None-Match ]

%% @doc If-None-Match
-spec none_match(state()) -> result().
none_match(#{req := #req{headers = RequestHeaders}} = State) ->
    do_none_match(State, get_none_match(RequestHeaders)).

-spec do_none_match(state(), [etag()]) -> result().
do_none_match(State, []) ->
    if_modified_since(State);
do_none_match(#{etag := nothing} = State, _ETags) ->
    if_range(State);
do_none_match(#{req := Req, etag := {just, ETag}} = State, ETags) ->
    ?IF(lists:any(compare_weak(ETag), ETags),
        ?IF(?GET_OR_HEAD(Req#req.method),
            not_modified(State),
            precondition_failed()),
        if_range(State)).

-spec get_none_match(elli:headers()) -> [etag()].
get_none_match(Headers) ->
    get_values(<<"If-None-Match">>, Headers).

%%% ================================================ [ 3.3.  If-Modified-Since ]

%% @doc If-Modified-Since
-spec if_modified_since(state()) -> result().
if_modified_since(#{req := Req} = State)
  when ?GET_OR_HEAD(Req#req.method) ->
    do_if_modified_since(State, get_modified_since(Req#req.headers));
if_modified_since(State) ->
    otherwise(State).

-spec do_if_modified_since(state(), maybe_m:maybe(binary())) -> result().
do_if_modified_since(#{mtime := Mtime} = State, Date)
  when nothing =:= Mtime; nothing =:= Date ->
    otherwise(State);
do_if_modified_since(#{req := Req, mtime := {just, Mtime}} = State,
                     {just, Date})
  when ?GET_OR_HEAD(Req#req.method) ->
    case compare_date(fun erlang:'>'/2, Mtime, Date) of
        {just, false} -> otherwise(State);
        _             -> if_range(State)
    end.

-spec get_modified_since(elli:headers()) -> maybe_m:maybe(binary()).
get_modified_since(Headers) ->
    maybe_get_value(<<"If-Modified-Since">>, Headers).

%%% ============================================== [ 3.4.  If-Unmodified-Since ]

%% @doc If-Unmodified-Since
-spec unmodified_since(state()) -> result() | no_return().
unmodified_since(#{req := #req{headers = RequestHeaders}} = State) ->
    do_unmodified_since(State, get_unmodified_since(RequestHeaders)).

-spec do_unmodified_since(state(), nothing) -> result();
                         (state(), {just, binary()}) -> no_return() | result().
do_unmodified_since(State, nothing) ->
    none_match(State);
do_unmodified_since(#{mtime := Mtime} = State, {just, Date}) ->
    %% FIXME: The origin server MUST NOT perform the requested method if the
    %% selected representation's last modification date is more recent than the
    %% date provided in the field-value; instead the origin server MUST respond
    %% with either a) the 412 (Precondition Failed) status code or b) one of the
    %% 2xx (Successful) status codes if the origin server has verified that a
    %% state change is being requested and the final state is already reflected
    %% in the current state of the target resource (i.e., the change requested
    %% by the user agent has already succeeded, but the user agent might not be
    %% aware of that because the prior response message was lost or a compatible
    %% change was made by some other user agent). In the latter case, the origin
    %% server MUST NOT send a validator header field in the response unless it
    %% can verify that the request is a duplicate of an immediately prior change
    %% made by the same user agent.
    case compare_date(fun erlang:'>'/2, Mtime, Date) of
        {just, true} -> precondition_failed();
        _            -> none_match(State)
    end.

-spec get_unmodified_since(elli:headers()) -> maybe_m:maybe(binary()).
get_unmodified_since(Headers) ->
    maybe_get_value(<<"If-Unmodified-Since">>, Headers).

%%% ========================================================= [ 3.5.  If-Range ]

%% @doc If-Range
-spec if_range(state()) -> result().
if_range(#{req := #req{headers = RequestHeaders} = Req} = State)
  when ?GET_OR_HEAD(Req#req.method) ->
    case get_range(RequestHeaders) of
        %% If there's no Range header, don't bother with If-Range.
        []     -> otherwise(State);
        _Range -> do_if_range(State, get_if_range(RequestHeaders))
    end;
if_range(State) ->
    otherwise(State).

-spec do_if_range(state(), maybe_m:maybe(binary())) -> result().
do_if_range(State, nothing) ->
    otherwise(State);
%% NOTE: No ETag implies no last modified date.
do_if_range(#{etag := nothing} = State, _ETagOrDate) ->
    otherwise(State);
do_if_range(#{etag := ETag, mtime := Mtime} = State, ETagOrDate) ->
    NewState =
        ?IF(is_etag(ETagOrDate),
            %% If-Range = entity-tag
            ?IF(compare_strong(ETag, ETagOrDate), State, delete_range(State)),
            %% If-Range = HTTP-date
            case compare_date(fun erlang:'=:='/2, Mtime, ETagOrDate) of
                {just, true} -> State;
                _            -> delete_range(State)
            end),
    otherwise(NewState).

-spec delete_range(state()) -> state().
delete_range(#{req := #req{headers = Headers} = Req} = State) ->
    maps:update(req, Req#req{headers = delete(<<"Range">>, Headers)}, State).

-spec get_if_range(Headers :: elli:headers()) -> maybe_m:maybe(binary()).
get_if_range(Headers) ->
    maybe_get_value(<<"If-Range">>, Headers).

%% @doc Determine with a given binary is an entity-tag or a date.
%% Per IETF RFC 7233 Section 3.2, "A valid entity-tag can be distinguished from
%% a valid HTTP-date by examining the first two characters for a DQUOTE."
-spec is_etag(Value :: binary()) -> boolean().
is_etag(ETag) when ?STRONG(ETag); ?WEAK(ETag) -> true;
is_etag(_Date)                                -> false.

-spec get_range(Headers :: elli:headers()) -> [binary()].
get_range(Headers) ->
    get_values(<<"Range">>, Headers).

%%% ================================================= [ 4.1.  304 Not Modified ]

-spec not_modified(state()) -> {304, elli:headers(), <<>>}.
not_modified(#{res := Res} = State) ->
    {_Code, NewHeaders, _Body} = update_headers(Res, State),
    PrunedHeaders = ifdef_delete(<<"ETag">>, <<"Last-Modified">>, NewHeaders),
    {304, PrunedHeaders, <<>>}.

%%% ========================================== [ 4.2.  412 Precondition Failed ]

-spec precondition_failed() -> no_return().
precondition_failed() ->
    throw({412, [], <<>>}).

%%% ========================================================= [ 6.  Precedence ]

%% @doc Passthrough.
-spec otherwise(state()) -> result().
otherwise(State) ->
    do_otherwise(State, maps:find(res, State)).

-spec do_otherwise(state(), error) -> elli:req();
                  (state(), {ok, elli_handler:result()}) ->
                          elli_handler:result().
do_otherwise(#{req := Req}, error) ->
    Req;
do_otherwise(State, {ok, Res}) ->
    update_headers(Res, State).

%%% ================================================================ [ Helpers ]

-spec update_headers(elli_handler:result(), state()) -> elli_handler:result().
update_headers(Res, State) ->
    NewRes = update_element(2, Res, fun prune_headers/1),
    Fun = fun(Headers) -> maps:fold(fun update_headers/3, Headers, State) end,
    update_element(2, NewRes, Fun).

-spec update_headers(etag, {just, etag()}, elli:headers()) ->
                            elli:headers();
                    (mtime, {just, calendar:datetime()}, elli:headers()) ->
                            elli:headers();
                    (_Key, _Value, elli:headers()) ->
                            elli:headers().
update_headers(etag, {just, ETag}, Headers) ->
    store(<<"ETag">>, ETag, Headers);
update_headers(mtime, {just, Mtime}, Headers) ->
    LastModified = list_to_binary(httpd_util:rfc1123_date(Mtime)),
    store(<<"Last-Modified">>, LastModified, Headers);
update_headers(_Key, _Value, Headers) ->
    Headers.

-spec prune_headers(elli:headers()) -> elli:headers().
prune_headers(Headers) ->
    lists:filter(fun allowed_header/1, Headers).

-spec allowed_header(elli:header()) -> boolean().
allowed_header({<<"Cache-Control">>, _}) -> true;
allowed_header({<<"Content-Location">>, _}) -> true;
allowed_header({<<"Date">>, _}) -> true;
allowed_header({<<"ETag">>, _}) -> true;
allowed_header({<<"Expires">>, _}) -> true;
allowed_header({<<"Last-Modified">>, _}) -> true;
allowed_header({<<"Vary">>, _}) -> true;
allowed_header(_) -> false.


%%% ==================================================================== [ EOF ]
