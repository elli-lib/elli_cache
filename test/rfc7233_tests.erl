%%% ====================================================== [ rfc7233_tests.erl ]
%%% @doc Tests for the partial IETF RFC 7233 implementation.
%%% @author Eric Bailey <eric@ericb.me> [https://github.com/yurrriq]
%%% @copyright 2016, elli-lib team
%%% @reference <a href="https://tools.ietf.org/html/rfc7233">IETF RFC 7233</a>
%%% @end
%%% ==================================================================== [ EOH ]
-module(rfc7233_tests).

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================= [ Unit Tests ]

%% FIXME: obviously
get_if_range_test() ->
    ?assertError(nyi, rfc7233:get_if_range([{<<"If-Range">>, <<>>}])).

is_etag_test_() ->
    Data = [ {<<"Fri, 26 Mar 2010 00:05:00 GMT">>, false}
             | [ {ETag, true} || ETag <- [<<"W/\"1\"">>, <<"W/\"2\"">>] ]
             ++ [ {ETag, true} || ETag <- [<<"\"1\"">>, <<"\"2\"">>] ]
           ],
    [ {<<Bin/binary, (is(IsETag))/binary, "an ETag">>,
       ?_assertMatch(IsETag, rfc7233:is_etag(Bin))}
      || {Bin, IsETag} <- Data ].

%%% ===================================================== [ Internal Functions ]

is(true)  -> <<" is ">>;
is(false) -> <<" is not ">>.

%%% ==================================================================== [ EOF ]
