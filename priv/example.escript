#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    code:ensure_loaded(example_cache),
    Config = [
              {mods, [
                      {elli_example_callback, []},
                      {elli_middleware_cache, [{mod, example_cache}]}
                     ]}
             ],
    {ok, _Pid} = elli:start_link([{callback, elli_middleware},
                                  {callback_args, Config},
                                  {port, 3000}]).
