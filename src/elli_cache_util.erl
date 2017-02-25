%%% ==================================================== [ elli_cache_util.erl ]
%%% @doc Useful helper functions for elli_cache.
%%% @author Eric Bailey <eric@ericb.me> [https://github.com/yurrriq]
%%% @copyright 2016, elli-lib team
%%% @end
%%% ==================================================================== [ EOH ]
-module(elli_cache_util).

-compile({parse_transform, do}).

%% Binary Utils.
-export([comma_split/1]).

%% Date Utils.
-export([convert_date/1, compare_date/3]).

%% Proplist Utils.
-export([maybe_get_value/2, get_values/2, ifdef_delete/3, store/3]).

%% Tuple Utils.
-export([update_element/3]).

-import(proplists, [delete/2, get_value/2, get_value/3, is_defined/2]).

-include("elli_cache_util.hrl").

%%% =========================================================== [ Binary Utils ]

-spec comma_split(binary()) -> [binary()].
comma_split(Subject) ->
    binary:split(Subject, [<<", ">>, <<",">>], [global, trim]).

%%% ============================================================= [ Date Utils ]

-spec convert_date(Date) -> maybe_m:maybe(Seconds) when
      Date    :: binary() | calendar:datetime(),
      Seconds :: non_neg_integer().
%% NOTE: convert_request_date/1 throws a function clause error
%% for input shorter than four characters.
convert_date(Bin) when is_binary(Bin) andalso size(Bin) >= 4 ->
    ReqDate = binary_to_list(Bin),
    case httpd_util:convert_request_date(ReqDate) of
        bad_date -> maybe_m:fail(Bin);
        DateTime -> convert_date(DateTime)
    end;
convert_date(Bin) when is_binary(Bin) ->
    maybe_m:fail(Bin);
convert_date({_Date, _Time} = DateTime) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    %% FIXME: use Date header, if present
    Now = calendar:universal_time(),
    NowSeconds = calendar:datetime_to_gregorian_seconds(Now),
    %% See 2.2.1
    maybe_m:return(?IF(Seconds > NowSeconds, NowSeconds, Seconds)).

-spec compare_date(Comp, Date1, Date2) -> maybe_m:maybe(boolean()) when
      Comp     :: fun((Seconds1, Seconds2) -> boolean()),
      Seconds1 :: non_neg_integer(),
      Seconds2 :: non_neg_integer(),
      Date1    :: binary() | calendar:datetime(),
      Date2    :: binary() | calendar:datetime().
compare_date(Comp, Date1, Date2) ->
    do([maybe_m || Seconds1 <- convert_date(Date1),
                   Seconds2 <- convert_date(Date2),
                   return(Comp(Seconds1, Seconds2))]).

%%% ========================================================= [ Proplist Utils ]

-spec maybe_get_value(binary(), elli:headers()) -> maybe_m:maybe(binary()).
maybe_get_value(_Key, []) ->
    nothing;
maybe_get_value(Key, Headers) ->
    case get_value(Key, Headers) of
        undefined -> nothing;
        Value     -> {just, Value}
    end.

-spec get_values(binary(), elli:headers()) -> [binary()].
get_values(Key, Headers) ->
    comma_split(get_value(Key, Headers, <<>>)).

%% @doc If `List1' contains at least one entry associated with `Key1',
%% delete all entries associated with `Key2'. Otherwise, return `List1'.
-spec ifdef_delete(Key1, Key2, List1) -> List2 when
      Key1  :: term(),
      Key2  :: term(),
      List1 :: [term()],
      List2 :: [term()].
ifdef_delete(Key1, Key2, List) ->
    ?IF(is_defined(Key1, List), delete(Key2, List), List).

-spec store(binary(), binary(), elli:headers()) -> elli:headers().
store(Key, Value, List) ->
    lists:keystore(Key, 1, List, {Key, Value}).

%%% ============================================================ [ Tuple Utils ]

-spec update_element(Index, Tuple1, Fun) -> Tuple2 when
      Index  :: pos_integer(),                  % 1..tuple_size(Tuple1)
      Tuple1 :: tuple(),
      Fun    :: fun((term()) -> term()),
      Tuple2 :: tuple().
update_element(Index, Tuple, Fun)
  when is_tuple(Tuple), tuple_size(Tuple) >= Index ->
    Value = Fun(element(Index, Tuple)),
    setelement(Index, Tuple, Value).

%%% ==================================================================== [ EOF ]
