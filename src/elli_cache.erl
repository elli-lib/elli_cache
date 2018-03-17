%%% ========================================================= [ elli_cache.erl ]
%%% @doc elli_cache behaviour.
%%% @author Eric Bailey <eric@ericb.me> [https://github.com/yurrriq]
%%% @copyright 2016, elli-lib team
%%% @end
%%% ==================================================================== [ EOH ]
-module(elli_cache).

-compile({parse_transform, cut}).
-compile({parse_transform, do}).

-export([get_modified/2, get_size/2]).

-include("elli_cache_util.hrl").
-include_lib("elli/include/elli.hrl").


%%% ================================================================== [ Types ]

-export_type([config/0]).

-type config() :: [elli_handler:callback()].


%%% ============================================================== [ Callbacks ]

-callback get_modified(Req, Args) -> Mtime when
      Req   :: elli:req(),
      Args  :: elli_handler:callback_args(),
      Mtime :: maybe_m:maybe(calendar:datetime()).

-callback get_size(Req, Args) -> Size when
      Req  :: elli:req(),
      Args :: elli_handler:callback_args(),
      Size :: maybe_m:maybe(non_neg_integer()).


%%% ================================================================ [ Helpers ]

%% @doc Maybe get the last modified date for a request.
%% If `{mod, Mod}' is present in `Args' and `Mod:get_modified/2' is exported,
%% return `Mod:get_modified(Req, Args)'. Otherwise, return `nothing'.
-spec get_modified(elli:req(), config()) -> maybe_m:maybe(calendar:datetime()).
get_modified(_Req, []) ->
    nothing;
get_modified(Req, [{Mod, Args}|ModArgs]) ->
    case maybe_apply(Mod, get_modified, Req, Args) of
        nothing -> get_modified(Req, ModArgs);
        Just    -> Just
    end.


%% @doc Maybe get the size of the response to a request.
%% If `{mod, Mod}' is present in `Args' and `Mod:get_size/2' is exported,
%% return `Mod:get_size(Req, Args)'. Otherwise, return `nothing'.
-spec get_size(elli:req(), config()) -> maybe_m:maybe(non_neg_integer()).
get_size(_Req, []) ->
    nothing;
get_size(Req, [{Mod, Args}|ModArgs]) ->
    case maybe_apply(Mod, get_size, Req, Args) of
        nothing -> get_modified(Req, ModArgs);
        Just    -> Just
    end.


%%% ===================================================== [ Internal Functions ]

-spec maybe_apply(module(), atom(), elli:req(), elli_handler:callback_args()) -> maybe_m:maybe(_).
maybe_apply(Module, Function, Req, Args) ->
    do([maybe_m ||
        ?IF(erlang:function_exported(Module, Function, 2),
            Module:Function(Req, Args),
            fail("Not exported"))]).


%%% ==================================================================== [ EOF ]
