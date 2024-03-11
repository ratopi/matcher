%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019-2024, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 27. Okt 2019 14:31
%%% Updated : 11. Mar 2024 19:57
%%%-------------------------------------------------------------------
-module(matcher_SUITE).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

-include_lib("common_test/include/ct.hrl").

%% API
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, all/0]).
-export([simple_tests/1, test_and_or/1, wrong_operator/1, test_map/1]).

%%%===================================================================
%%% CT callbacks
%%%===================================================================

all() ->
	[
		simple_tests,
		test_and_or,
		wrong_operator,
		test_map
	].


init_per_suite(Config) ->
	Config.


end_per_suite(_Config) ->
	ok.

init_per_testcase(_, Config) ->
	Config.


end_per_testcase(_, _Config) ->
	ok.

%%%===================================================================
%%% test functions
%%%===================================================================

simple_tests(_Config) ->
	true = matcher:eval({'=', <<"Albert">>, <<"Albert">>}),
	false = matcher:eval({'=', <<"Albert">>, <<"albert">>}),

	true = matcher:eval({'=~', <<"Albert">>, <<"Albert">>}),
	true = matcher:eval({'=~', <<"Albert">>, <<"albert">>}),

	true = matcher:eval({'<', <<"Albert">>, <<"albert">>}),
	true = matcher:eval({'>', <<"albert">>, <<"Albert">>}),
	true = matcher:eval({'<=', <<"Albert">>, <<"albert">>}),
	true = matcher:eval({'<=', <<"Albert">>, <<"Albert">>}),
	true = matcher:eval({'>=', <<"albert">>, <<"Albert">>}),
	true = matcher:eval({'>=', <<"albert">>, <<"albert">>}),

	true = matcher:eval({'?', <<"bert">>, <<"Albert">>}),
	true = matcher:eval({'?', <<"Alb">>, <<"Albert">>}),
	false = matcher:eval({'?', <<"bert">>, <<"ALBERT">>}),
	false = matcher:eval({'?', <<"Alb">>, <<"albert">>}),

	true = matcher:eval({'?~', <<"bert">>, <<"Albert">>}),
	true = matcher:eval({'?~', <<"Alb">>, <<"albert">>}),
	true = matcher:eval({'?~', <<"alb">>, <<"albert">>}),
	true = matcher:eval({'?~', <<"b">>, <<"albert">>}),
	false = matcher:eval({'?~', <<"x">>, <<"albert">>}),

	ok.



test_and_or(_Config) ->
	true = matcher:eval({'|', [{'=', <<"Albert">>, <<"Albert">>}, {'=', <<"Berta">>, <<"Albert">>}]}),
	false = matcher:eval({'&', [{'=', <<"Albert">>, <<"Albert">>}, {'=', <<"Berta">>, <<"Albert">>}]}),
	false = matcher:eval({'|', [{'=', <<"Albert">>, <<"Karl">>}, {'=', <<"Berta">>, <<"Karl">>}]}),

	ok.



wrong_operator(_Config) ->
	Fail = {'*', <<"Something">>, <<"Albert">>},
	Ok = {'=', <<"Albert">>, <<"Albert">>},

	Fail = matcher:eval(Fail),

	{error, {unevaluated_expression, Fail}} = matcher:match(Fail),
	{error, {unevaluated_expression, Fail}} = matcher:match({'&', [Ok, Fail]}),
	{error, {unevaluated_expression, Fail}} = matcher:match({'&', [Ok, Fail, Ok]}),

	ok.



test_map(_Config) ->
	M = get_a_doc(),

	true = matcher:eval(M, {'=', sender, <<"WDR">>}),
	true = matcher:eval(M, {'=~', sender, <<"wdr">>}),

	false = matcher:eval(M, {'=', sender, <<"ZDF">>}),

	true = matcher:eval(M, {'>', <<"ZDF">>, sender}),
	true = matcher:eval(M, {'<', sender, <<"ZDF">>}),
	true = matcher:eval(M, {'>', sender, <<"ARD">>}),

	false = matcher:eval(M, {'>', sender, <<"ZDF">>}),
	false = matcher:eval(M, {'<', sender, <<"ARD">>}),
	false = matcher:eval(M, {'>', <<"ARD">>, sender}),

	true = matcher:eval(M, {'?~', <<"weltweit">>, url}),
	true = matcher:eval(M, {'?', <<"Pene">>, titel}),
	true = matcher:eval(M, {'?~', <<"Pene">>, titel}),
	true = matcher:eval(M, {'?~', <<"PENE">>, titel}),
	true = matcher:eval(M, {'?~', <<"E F">>, titel}),
	true = matcher:eval(M, {'?~', <<"THE FAB">>, titel}),

	false = matcher:eval(M, {'?~', <<"xPene">>, titel}),
	false = matcher:eval(M, {'?', <<"THE FAB">>, titel}),

	true = matcher:eval(M, {'?', thema, <<"Der tolle Rockpalast">>}),

	true = matcher:eval(M, {'|', [{'?', <<"THE FAB">>, titel}, {'<', sender, <<"ARD">>}, {'?~', <<"weltweit">>, url}]}),

	ok.

%%%===================================================================
%%% helper functions
%%%===================================================================

get_a_doc() ->
	#{
		beschreibung => <<"Klicke dich hier durch die Sendung">>,
		datum => <<"22.10.2011">>,
		datum_l => <<"1319313600">>,
		dauer => <<"00:58:45">>,
		neu => false,
		sender => <<"WDR">>,
		thema => <<"Rockpalast">>,
		titel => <<"The Fabulous Penetrators">>,
		url => <<"http://wdradaptiv-vh.akamaihd.net/i/medp/ondemand/weltweit/fsk0/28/285747/,285747_2745062,.mp4.csmil/index_0_av.m3u8">>,
		website => <<"https://www1.wdr.de/mediathek/video/sendungen/rockpalast/video-the-fabulous-penetrators-100.html">>,
		zeit => <<"22:00:00">>
	}.
