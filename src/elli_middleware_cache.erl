%%% ============================================== [ elli_middleware_cache.erl ]
%%% @doc Generic caching middleware.
%%% @author Eric Bailey <eric@ericb.me> [https://github.com/yurrriq]
%%% @copyright 2016, elli-lib team
%%% @end
%%% ==================================================================== [ EOH ]
-module(elli_middleware_cache).

-behaviour(elli_handler).

-compile({parse_transform, do}).


%% Elli Handler
-export([handle/2, handle_event/3, preprocess/2, postprocess/3]).


-include("elli_cache_util.hrl").
-include_lib("elli/include/elli.hrl").


%%% =========================================================== [ Elli Handler ]

handle(_Req, _Args) ->
    ignore.


handle_event(_Event, _Args, _Config) ->
    ok.


-spec preprocess(Req1, Config) -> Req2 when
      Req1   :: elli:req(),
      Config :: elli_handler:callback_args(),
      Req2   :: elli:req().
preprocess(Req, Config)
  when not(?GET_OR_HEAD(Req#req.method)) ->
    {MaybeMtime, MaybeETag} = maybe_params(Req, Config),
    rfc7232:init(Req, MaybeMtime, MaybeETag);
preprocess(Req, _Config) ->
    Req.


-spec postprocess(Req, Res1, Config) -> Res2 when
      Req    :: elli:req(),
      Res1   :: elli_handler:result(),
      Config :: elli_handler:callback_args(),
      Res2   :: elli_handler:result().
postprocess(Req, {ResponseCode, Body}, Config)
  when ?OK_GET_OR_HEAD(ResponseCode, Req#req.method) ->
    postprocess(Req, {ResponseCode, [], Body}, Config);
postprocess(Req, {ResponseCode, _Headers, _Body} = Res, Config)
  when ?OK_GET_OR_HEAD(ResponseCode, Req#req.method) ->
    {MaybeMtime, MaybeETag} = maybe_params(Req, Config),
    rfc7232:init(Req, MaybeMtime, MaybeETag, Res);
postprocess(_, Res, _) ->
    Res.


%%% ===================================================== [ Internal Functions ]

maybe_params(Req, Config) ->
    MaybeMtime = elli_cache:get_modified(Req, Config),
    MaybeETag  = do([maybe_m ||
                     Mtime <- MaybeMtime,
                     Size  <- elli_cache:get_size(Req, Config),
                     return(create_etag(Mtime, Size))]),
    {MaybeMtime, MaybeETag}.


%% @doc Create an {@link rfc7232:etag/0. ETag} from a resource's last modified
%% date and its size.
%% @see rfc7232:etag/0
%% @see elli_cache
-spec create_etag(calendar:datetime(), non_neg_integer()) -> rfc7232:etag().
create_etag(Mtime, Size) ->
    ETag = list_to_binary(httpd_util:create_etag(Mtime, Size)),
    <<"\"", ETag/binary, "\"">>.


%%% ==================================================================== [ EOF ]
