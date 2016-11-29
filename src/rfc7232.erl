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
    step1(State).

-spec init(Req, Mtime, ETag, Res) -> elli_handler:result() when
      Req   :: elli:req(),
      Mtime :: calendar:datetime(),
      ETag  :: etag(),
      Res   :: elli_handler:result().
init(Req, Mtime, ETag, Res) ->
    State = #{req => Req, mtime => Mtime, etag => ETag, res => Res},
    step1(State).

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

-spec if_match(elli:headers()) -> [etag()].
if_match(Headers) ->
    get_values(<<"If-Match">>, Headers).

%%% ==================================================== [ 3.2.  If-None-Match ]

-spec none_match(elli:headers()) -> [etag()].
none_match(Headers) ->
    get_values(<<"If-None-Match">>, Headers).

%%% ================================================ [ 3.3.  If-Modified-Since ]

-spec modified_since(elli:headers()) -> undefined | binary().
modified_since(Headers) ->
    proplists:get_value(<<"If-Modified-Since">>, Headers).

%%% ============================================== [ 3.4.  If-Unmodified-Since ]

-spec unmodified_since(elli:headers()) -> undefined | binary().
unmodified_since(Headers) ->
    proplists:get_value(<<"If-Unmodified-Since">>, Headers).

%%% ========================================================= [ 3.5.  If-Range ]

%% TODO: See Section 3.2. of RFC 7233
%% https://tools.ietf.org/html/rfc7233#section-3.2
%% if_range(Headers) -> error(nyi).

%%% ================================================= [ 4.1.  304 Not Modified ]

-spec not_modified(state()) -> {304, elli:headers(), <<>>}.
not_modified(#{res := Res} = State) ->
    {_Code, NewHeaders, _Body} = update_headers(Res, State),
    PrunedHeaders = ?IF(proplists:is_defined(<<"ETag">>, NewHeaders),
                        proplists:delete(<<"Last-Modified">>, NewHeaders),
                        NewHeaders),
    {304, PrunedHeaders, <<>>}.

%%% ========================================== [ 4.2.  412 Precondition Failed ]

-spec precondition_failed() -> no_return().
precondition_failed() ->
    throw({412, [], <<>>}).

%%% ======================================================== [ 6.  Precendence ]

%% @doc If-Match
-spec step1(state()) -> result().
step1(#{req := #req{headers = RequestHeaders}} = State) ->
    do_step1(State, if_match(RequestHeaders)).

-spec do_step1(state(), [etag()]) -> result() | no_return().
do_step1(State, []) ->
    step2(State);
do_step1(#{etag := ETag} = State, ETags) ->
    ?IF(lists:any(compare_strong(ETag), ETags),
        step3(State),
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

%% @doc If-Unmodified-Since
-spec step2(state()) -> result() | no_return().
step2(#{req := #req{headers = RequestHeaders}} = State) ->
    do_step2(State, unmodified_since(RequestHeaders)).

-spec do_step2(state(), undefined) -> result();
              (state(), binary()) -> no_return().
do_step2(State, undefined) ->
    step3(State);
do_step2(_State, _Date) ->
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

%% @doc If-None-Match
-spec step3(state()) -> result().
step3(#{req := #req{headers = RequestHeaders}} = State) ->
    do_step3(State, none_match(RequestHeaders)).

-spec do_step3(state(), [etag()]) -> result().
do_step3(State, []) ->
    step4(State);
do_step3(#{req := Req, etag := ETag} = State, ETags) ->
    ?IF(lists:any(compare_weak(ETag), ETags),
        ?IF(?GET_OR_HEAD(Req#req.method),
            not_modified(State),
            precondition_failed()),
        step5(State)).

%% @doc If-Modified-Since
-spec step4(state()) -> result().
step4(#{req := Req} = State)
  when ?GET_OR_HEAD(Req#req.method) ->
    do_step4(State, modified_since(Req#req.headers));
step4(State) ->
    step6(State).

-spec do_step4(state(), undefined | binary()) -> result().
do_step4(State, undefined) ->
    step5(State);
do_step4(#{req := Req, mtime := Mtime} = State, Date)
  when ?GET_OR_HEAD(Req#req.method) ->
    case convert_date(Mtime) of
        bad_date ->
            step5(State);
        Modified ->
            case convert_date(Date) of
                bad_date ->
                    step5(State);
                Since ->
                    ?IF(Modified > Since,
                        step5(State),
                        step6(State))
            end
    end.

%% @doc Range and If-Range
-spec step5(state()) -> result().
step5(State) ->
    %% TODO: Range and If-Range
    step6(State).

%% @doc Passthrough.
-spec step6(state()) -> result().
step6(State) ->
    do_step6(State, maps:find(res, State)).

-spec do_step6(state(), error) -> elli:req();
              (state(), {ok, elli_handler:result()}) -> elli_handler:result().
do_step6(#{req := Req}, error) ->
    Req;
do_step6(State, {ok, Res}) ->
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
    comma_split(proplists:get_value(Key, Headers, <<>>)).

-spec store(binary(), binary(), elli:headers()) -> elli:headers().
store(Key, Value, List) ->
    lists:keystore(Key, 1, List, {Key, Value}).

-spec comma_split(binary()) -> [binary()].
comma_split(Subject) ->
    binary:split(Subject, [<<", ">>, <<",">>], [global, trim]).

%%% ==================================================================== [ EOF ]
