%%% ============================================================ [ rfc7232.erl ]
%%% @doc RFC 7232 implementation.
%%% @author Eric Bailey <eric@ericb.me> [https://github.com/yurrriq]
%%% @copyright 2016, elli-lib team
%%% @reference <a href="https://tools.ietf.org/html/rfc7232">RFC 7232</a>
%%% @end
%%% ==================================================================== [ EOH ]
-module(rfc7232).

%% API.
-export([init/3, init/4]).

-import(proplists, [delete/2, get_value/2, get_value/3, is_defined/2]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-include("elli_cache_util.hrl").
-include_lib("elli/include/elli.hrl").

%% Macros.
-define(WEAK(ETag), <<"W/">> =:= binary_part(ETag, {0, 2})).

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
                   mtime => calendar:datetime(),
                   etag  => etag(),
                   res   => elli_handler:result()
                  }.

%%% ==================================================================== [ API ]

%% not('GET' | 'HEAD')
-spec init(Req, Mtime, ETag) -> result() | no_return() when
      Req   :: elli:req(),
      Mtime :: calendar:datetime(),
      ETag  :: etag().
init(Req, Mtime, ETag) ->
    State = #{req => Req, mtime => Mtime, etag => ETag},
    if_match(State).

-spec init(Req, Mtime, ETag, Res) -> elli_handler:result() when
      Req   :: elli:req(),
      Mtime :: calendar:datetime(),
      ETag  :: etag(),
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

%% @doc If-Match
-spec if_match(state()) -> result().
if_match(#{req := #req{headers = RequestHeaders}} = State) ->
    do_if_match(State, get_if_match(RequestHeaders)).

-spec do_if_match(state(), [etag()]) -> result() | no_return().
do_if_match(State, []) ->
    unmodified_since(State);
do_if_match(#{etag := ETag} = State, ETags) ->
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
do_none_match(#{req := Req, etag := ETag} = State, ETags) ->
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

-spec do_if_modified_since(state(), undefined | binary()) -> result().
do_if_modified_since(State, undefined) ->
    if_range(State);
do_if_modified_since(#{req := Req, mtime := Mtime} = State, Date)
  when ?GET_OR_HEAD(Req#req.method) ->
    case convert_date(Mtime) of
        bad_date ->
            if_range(State);
        Modified ->
            case convert_date(Date) of
                bad_date ->
                    if_range(State);
                Since ->
                    ?IF(Modified > Since,
                        if_range(State),
                        otherwise(State))
            end
    end.

-spec get_modified_since(elli:headers()) -> undefined | binary().
get_modified_since(Headers) ->
    get_value(<<"If-Modified-Since">>, Headers).

%%% ============================================== [ 3.4.  If-Unmodified-Since ]

%% @doc If-Unmodified-Since
-spec unmodified_since(state()) -> result() | no_return().
unmodified_since(#{req := #req{headers = RequestHeaders}} = State) ->
    do_unmodified_since(State, get_unmodified_since(RequestHeaders)).

-spec do_unmodified_since(state(), undefined) -> result();
                         (state(), binary()) -> no_return().
do_unmodified_since(State, undefined) ->
    none_match(State);
do_unmodified_since(_State, _Date) ->
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
    precondition_failed().

-spec get_unmodified_since(elli:headers()) -> undefined | binary().
get_unmodified_since(Headers) ->
    get_value(<<"If-Unmodified-Since">>, Headers).

%%% ========================================================= [ 3.5.  If-Range ]

%% TODO: See Section 3.2. of RFC 7233
%% https://tools.ietf.org/html/rfc7233#section-3.2

%% @doc Range and If-Range
-spec if_range(state()) -> result().
if_range(State) ->
    %% TODO: Range and If-Range
    otherwise(State).

%% if_range(Headers) -> error(nyi).

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

-spec convert_date(binary()) -> non_neg_integer() | bad_date.
convert_date(Bin) ->
    ReqDate = binary_to_list(Bin),
    case httpd_util:convert_request_date(ReqDate) of
        bad_date ->
            bad_date;
        DateTime ->
            Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
            %% FIXME: use Date header, if present
            Now = calendar:universal_time(),
            %% See 2.2.1
            max(Seconds, calendar:datetime_to_gregorian_seconds(Now))
    end.

-spec update_headers(elli_handler:result(), state()) -> elli_handler:result().
update_headers(Res, State) ->
    NewRes = update_element(2, Res, fun prune_headers/1),
    Fun = fun(Headers) -> maps:fold(fun update_headers/3, Headers, State) end,
    update_element(2, NewRes, Fun).

-spec update_headers(etag, etag(), elli:headers()) ->
                            elli:headers();
                    (mtime, calendar:datetime(), elli:headers()) ->
                            elli:headers();
                    (_Key, _Value, elli:headers()) ->
                            elli:headers().
update_headers(etag, ETag, Headers) ->
    store(<<"ETag">>, ETag, Headers);
update_headers(mtime, Mtime, Headers) ->
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

-spec update_element(Index, Tuple1, Fun) -> Tuple2 when
      Index  :: pos_integer(),                  % 1..tuple_size(Tuple1)
      Tuple1 :: tuple(),
      Fun    :: fun((term()) -> term()),
      Tuple2 :: tuple().
update_element(Index, Tuple, Fun)
  when is_tuple(Tuple), tuple_size(Tuple) >= Index ->
    Value = Fun(element(Index, Tuple)),
    setelement(Index, Tuple, Value).

-spec get_values(binary(), elli:headers()) -> [binary()].
get_values(Key, Headers) ->
    comma_split(get_value(Key, Headers, <<>>)).

-spec store(binary(), binary(), elli:headers()) -> elli:headers().
store(Key, Value, List) ->
    lists:keystore(Key, 1, List, {Key, Value}).

-spec comma_split(binary()) -> [binary()].
comma_split(Subject) ->
    binary:split(Subject, [<<", ">>, <<",">>], [global, trim]).

%% @doc If `List1' contains at least one entry associated with `Key1',
%% delete all entries associated with `Key2'. Otherwise, return `List1'.
-spec ifdef_delete(Key1, Key2, List1) -> List2 when
      Key1  :: term(),
      Key2  :: term(),
      List1 :: [term()],
      List2 :: [term()].
ifdef_delete(Key1, Key2, List) ->
    ?IF(is_defined(Key1, List), delete(Key2, List), List).

%%% ==================================================================== [ EOF ]
