%%% ====================================================== [ rfc7232_tests.erl ]
%%% @doc IETF RFC 7232 tests.
%%% @author Eric Bailey <eric@ericb.me> [https://github.com/yurrriq]
%%% @copyright 2016, elli-lib team
%%% @reference <a href="https://tools.ietf.org/html/rfc7232">IETF RFC 7232</a>
%%% @end
%%% ==================================================================== [ EOH ]
-module(rfc7232_tests).

-include_lib("eunit/include/eunit.hrl").

%%% ============================================================= [ Unit Tests ]

comparison_test_() ->
    [
     compare(<<"W/\"1\"">>, <<"W/\"1\"">>, no_match, match),
     compare(<<"W/\"1\"">>, <<"W/\"2\"">>, no_match, no_match),
     compare(<<"W/\"1\"">>, <<"\"1\"">>, no_match, match),
     compare(<<"\"1\"">>, <<"\"1\"">>, match, match)
    ].

%% FIXME: obviously
get_if_range_test() ->
    ?assertMatch({just, <<>>}, rfc7232:get_if_range([{<<"If-Range">>, <<>>}])).

is_etag_test_() ->
    Data = [ {<<"Fri, 26 Mar 2010 00:05:00 GMT">>, false}
             | [ {ETag, true} || ETag <- [<<"W/\"1\"">>, <<"W/\"2\"">>] ]
             ++ [ {ETag, true} || ETag <- [<<"\"1\"">>, <<"\"2\"">>] ]
           ],
    [ {<<Bin/binary, (is(IsETag))/binary, "an ETag">>,
       ?_assertMatch(IsETag, rfc7232:is_etag(Bin))}
      || {Bin, IsETag} <- Data ].

%%% ===================================================== [ Internal Functions ]

compare(ETag1, ETag2, Strong, Weak) ->
    Comparisons = [
                   {<<"Strong">>, fun rfc7232:compare_strong/2, Strong},
                   {<<"Weak">>, fun rfc7232:compare_weak/2, Weak}
                  ],
    [ {<<Strength/binary, " comparison of ",
         ETag1/binary, " and ", ETag2/binary,
         " ==> ", (match(Match))/binary>>,
       assertion(Compare, ETag1, ETag2, Match)}
      || {Strength, Compare, Match} <- Comparisons ].

is(true)  -> <<" is ">>;
is(false) -> <<" is not ">>.

match(match) -> <<"match">>;
match(no_match) -> <<"no match">>.

assertion(Compare, ETag1, ETag2, match) ->
    ?_assert(Compare(ETag1, ETag2));
assertion(Compare, ETag1, ETag2, no_match) ->
    ?_assertNot(Compare(ETag1, ETag2)).

%%% ==================================================================== [ EOF ]
