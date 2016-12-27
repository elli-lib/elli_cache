%%% ============================================================ [ rfc7233.erl ]
%%% @doc Partial IETF RFC 7233 implementation.
%%% @author Eric Bailey <eric@ericb.me> [https://github.com/yurrriq]
%%% @copyright 2016, elli-lib team
%%% @reference <a href="https://tools.ietf.org/html/rfc7233">IETF RFC 7233</a>
%%% @end
%%% ==================================================================== [ EOH ]
-module(rfc7233).

%% If-Range.
-export([get_if_range/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("elli_cache_util.hrl").

%%% ========================================================= [ 3.2.  If-Range ]

-spec get_if_range(Headers :: elli:headers()) -> no_return().
get_if_range(_Headers) -> error(nyi).

%%% ===================================================== [ Internal Functions ]

%% @doc Determine with a given binary is an entity-tag or a date.
%% Per IETF RFC 7233 Section 3.2, "A valid entity-tag can be distinguished from
%% a valid HTTP-date by examining the first two characters for a DQUOTE."
-spec is_etag(Value :: binary()) -> boolean().
is_etag(ETag) when ?STRONG(ETag); ?WEAK(ETag) -> true;
is_etag(_Date)                                -> false.

%%% ==================================================================== [ EOF ]
