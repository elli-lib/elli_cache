%%% ========================================================= [ elli_cache.erl ]
%%% @doc elli_cache behaviour.
%%% @end
%%% ============================================================================

-module(elli_cache).

-export([modified/2]).

%%% ============================================================== [ Callbacks ]

-callback modified(Req, Args) -> Mtime when
      Req :: elli:req(),
      Args :: elli_handler:callback_args(),
      Mtime :: calendar:datetime().

%%% ================================================================ [ Helpers ]

-spec modified(Req, Args) -> false | Mtime when
      Req :: elli:req(),
      Args :: [{mod, Mod :: module()}],
      Mtime :: calendar:datetime().
%% @doc Iff given a `Mod`, return `Mod:modified(Req, Args)' iff exported.
modified(Req, Args) ->
    case proplists:get_value(mod, Args) of
        undefined ->
            false;
        Mod ->
            erlang:function_exported(Mod, modified, 2)
                andalso Mod:modified(Req, Args)
    end.

%%% ==================================================================== [ EOF ]
