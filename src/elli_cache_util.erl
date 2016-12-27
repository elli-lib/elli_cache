%%% ==================================================== [ elli_cache_util.erl ]
%%% @doc Useful helper functions for elli_cache.
%%% @author Eric Bailey <eric@ericb.me> [https://github.com/yurrriq]
%%% @copyright 2016, elli-lib team
%%% @end
%%% ==================================================================== [ EOH ]
-module(elli_cache_util).

%% Binary Utils.
-export([comma_split/1]).

%% Date Utils.
-export([convert_date/1]).

%% Proplist Utils.
-export([get_values/2, ifdef_delete/3, store/3]).

%% Tuple Utils.
-export([update_element/3]).

-import(proplists, [delete/2, get_value/2, get_value/3, is_defined/2]).

-include("elli_cache_util.hrl").

%%% =========================================================== [ Binary Utils ]

-spec comma_split(binary()) -> [binary()].
comma_split(Subject) ->
    binary:split(Subject, [<<", ">>, <<",">>], [global, trim]).

%%% ============================================================= [ Date Utils ]

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

%%% ========================================================= [ Proplist Utils ]

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
