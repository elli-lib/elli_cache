-module(example_cache).
-behaviour(elli_cache).
-export([get_modified/2, get_size/2]).

-spec get_modified(elli:req(), elli_cache:config()) -> calendar:datetime().
get_modified(_Req, _Args) ->
    %% calendar:universal_time().
    {{2016, 11, 15}, {0, 31, 7}}.

-spec get_size(elli:req(), elli_cache:config()) -> non_neg_integer().
get_size(_Req, _Args) ->
    %% rand:uniform(65536).
    42.
