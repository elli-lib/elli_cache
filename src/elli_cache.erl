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

%%% ================================================================== [ Types ]

-export_type([config/0]).

-type config() :: [{mod, Mod :: module()}].

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
get_modified(Req, Args) ->
    maybe_m:'>>='(get_mod(Args), maybe_apply(_, get_modified, Req, Args)).

%% @doc Maybe get the size of the response to a request.
%% If `{mod, Mod}' is present in `Args' and `Mod:get_size/2' is exported,
%% return `Mod:get_size(Req, Args)'. Otherwise, return `nothing'.
-spec get_size(elli:req(), config()) -> maybe_m:maybe(non_neg_integer()).
get_size(Req, Args) ->
    maybe_m:'>>='(get_mod(Args), maybe_apply(_, get_size, Req, Args)).

%%% ===================================================== [ Internal Functions ]

-spec get_mod(config()) -> maybe_m:maybe(module()).
get_mod(Args) ->
    do([maybe_m ||
        case proplists:get_value(mod, Args) of
            undefined -> fail("mod undefined");
            Mod       -> return(Mod)
        end]).

-spec maybe_apply(module(), atom(), elli:req(), config()) -> maybe_m:maybe(_).
maybe_apply(Module, Function, Req, Args) ->
    do([maybe_m ||
        ?IF(erlang:function_exported(Module, Function, 2),
            Module:Function(Req, Args),
            fail("Not exported"))]).

%%% ==================================================================== [ EOF ]
