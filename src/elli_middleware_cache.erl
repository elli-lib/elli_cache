%%% ============================================== [ elli_middleware_cache.erl ]
%%% @doc Generic caching middleware.
%%% @author Eric Bailey <eric@ericb.me> [https://github.com/yurrriq]
%%% @copyright 2016, elli-lib team
%%% @end
%%% ==================================================================== [ EOH ]
-module(elli_middleware_cache).

-compile({parse_transform, do}).

%% Elli Middleware
-export([preprocess/2, postprocess/3]).

-include("elli_cache_util.hrl").
-include_lib("elli/include/elli.hrl").

%%% ======================================================== [ Elli Middleware ]

-spec preprocess(Req1, Config) -> Req2 when
      Req1   :: elli:req(),
      Config :: elli_handler:callback_args(),
      Req2   :: elli:req().
preprocess(Req, Config)
  when not(?GET_OR_HEAD(Req#req.method)) ->
    from_maybe(Req,
               do([maybe_m ||
                   Mtime <- elli_cache:get_modified(Req, Config),
                   Size  <- elli_cache:get_size(Req, Config),
                   ETag  <- return(create_etag(Mtime, Size)),
                   return(rfc7232:init(Req, Mtime, ETag))]));
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
postprocess(Req, {ResponseCode, _Headers, Body} = Res, Config)
  when ?OK_GET_OR_HEAD(ResponseCode, Req#req.method) ->
    from_maybe(Res,
               do([maybe_m ||
                   Mtime <- elli_cache:get_modified(Req, Config),
                   Size  <- return(iolist_size(Body)),
                   ETag  <- return(create_etag(Mtime, Size)),
                   return(rfc7232:init(Req, Mtime, ETag, Res))]));
postprocess(_, Res, _) ->
    Res.

%%% ===================================================== [ Internal Functions ]

%% @doc Create an {@link rfc7232:etag/0. ETag} from a resource's last modified
%% date and its size.
%% @see rfc7232:etag/0
%% @see elli_cache
-spec create_etag(calendar:datetime(), non_neg_integer()) -> rfc7232:etag().
create_etag(Mtime, Size) ->
    ETag = list_to_binary(httpd_util:create_etag(Mtime, Size)),
    <<"\"", ETag/binary, "\"">>.

%% @doc Like Haskell's `Data.Maybe.fromMaybe'.
%% Given a default value and a maybe (or nullary function that returns one), if
%% the maybe is `nothing', return the default value; otherwise, return the value
%% contained in the maybe.
-spec from_maybe(A, maybe_m:maybe(A) | fun(() -> maybe_m:maybe(A))) -> A.
from_maybe(D, F) when is_function(F, 0) -> from_maybe(D, F());
from_maybe(D, X) -> case X of nothing -> D; {just, V} -> V end.

%%% ==================================================================== [ EOF ]
