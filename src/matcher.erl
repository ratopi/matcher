%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2024, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 11. Mär 2024 06:38
%%%-------------------------------------------------------------------
-module(matcher).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([identity_provider/0, map_provider/1]).
-export([eval/1, eval/2]).
-export([match/1, match/2]).

%%%===================================================================
%%% API
%%%===================================================================

identity_provider() ->
	fun(X) -> X end.


map_provider(Map) when is_map(Map) ->
	fun(X) -> maps:get(X, Map, X) end.


eval(Expression) ->
	eval(identity_provider(), Expression).


eval(Provider, Expression) when is_function(Provider) ->
	eval_(Provider, Provider(Expression));

eval(Map, Expression) when is_map(Map) ->
	eval(map_provider(Map), Expression).


match(Expression) ->
	true_or_false(eval(Expression)).


match(Provider, Expression) ->
	true_or_false(eval(Provider, Expression)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

% replacements

eval_(Provider, {'!', A}) -> eval_(Provider, {'not', A});

eval_(Provider, {'&', List}) -> eval_(Provider, {'and', List});

eval_(Provider, {'|', List}) -> eval_(Provider, {'or', List});

eval_(Provider, {'<', A, B}) -> eval_(Provider, {'lt', A, B});
eval_(Provider, {'<~', A, B}) -> eval_(Provider, {{'lt', 'case_insensitive'}, A, B});

eval_(Provider, {'>', A, B}) -> eval_(Provider, {'gt', A, B});
eval_(Provider, {'>~', A, B}) -> eval_(Provider, {{'gt', 'case_insensitive'}, A, B});

eval_(Provider, {'>=', A, B}) -> eval_(Provider, {'ge', A, B});
eval_(Provider, {'=>', A, B}) -> eval_(Provider, {'ge', A, B});

eval_(Provider, {'>=~', A, B}) -> eval_(Provider, {{'ge', 'case_insensitive'}, A, B});
eval_(Provider, {'=>~', A, B}) -> eval_(Provider, {{'ge', 'case_insensitive'}, A, B});

eval_(Provider, {'<=', A, B}) -> eval_(Provider, {'le', A, B});
eval_(Provider, {'=<', A, B}) -> eval_(Provider, {'le', A, B});

eval_(Provider, {'<=~', A, B}) -> eval_(Provider, {{'le', 'case_insensitive'}, A, B});
eval_(Provider, {'=<~', A, B}) -> eval_(Provider, {{'le', 'case_insensitive'}, A, B});

eval_(Provider, {'=', A, B}) -> eval_(Provider, {'eq', A, B});
eval_(Provider, {'==', A, B}) -> eval_(Provider, {'eq', A, B});

eval_(Provider, {'=~', A, B}) -> eval_(Provider, {{'eq', 'case_insensitive'}, A, B});

eval_(Provider, {'?', A, B}) -> eval_(Provider, {'part_of', A, B});
eval_(Provider, {'?~', A, B}) -> eval_(Provider, {{'part_of', 'case_insensitive'}, A, B});


% evaluations
% eval_/2 calls eval/2 for replacement!

% operators with single operands

eval_(Provider, {'not', A}) -> not eval(Provider, A);

% operators with any number of operands

eval_(_Provider, {'and', []}) -> true;

eval_(Provider, {'and', [H | T]}) ->
	case eval(Provider, H) of
		true -> eval(Provider, {'and', T});
		false -> false;
		_ -> H % return the original expression -> eval/2 can return it, match/2 can handle it
	end;


eval_(_Provider, {'or', []}) -> false;

eval_(Provider, {'or', [H | T]}) ->
	case eval(Provider, H) of
		false -> eval(Provider, {'or', T});
		true -> true
	end;

% case insensitive two operands

eval_(Provider, {{Op, 'case_insensitive'}, A, B}) ->
	eval_(Provider, {Op, lowercase(eval(Provider, A)), lowercase(eval(Provider, B))});

% operators with two operands

eval_(Provider, {'lt', A, B}) -> eval(Provider, A) < eval(Provider, B);
eval_(Provider, {'gt', A, B}) -> eval(Provider, A) > eval(Provider, B);
eval_(Provider, {'le', A, B}) -> eval(Provider, A) =< eval(Provider, B);
eval_(Provider, {'ge', A, B}) -> eval(Provider, A) >= eval(Provider, B);
eval_(Provider, {'eq', A, B}) -> eval(Provider, A) == eval(Provider, B);

eval_(Provider, {'part_of', Part, Full}) ->
	P = eval(Provider, Part),
	F = eval(Provider, Full),
	case is_string(P) andalso is_string(F) of
		true ->
			case
				string:find(F, P)
			of
				nomatch -> false;
				_ -> true
			end;
		false ->
			false
	end;

% finally we assume to have a value (not an expression)

eval_(_Provider, Expr) -> Expr.


% --- helpers ---


is_string(S) when is_binary(S); is_list(S) -> true;
is_string(_) -> false.


lowercase(S) when is_binary(S); is_list(S) -> string:lowercase(S);
lowercase(X) -> X.


true_or_false(true) -> true;
true_or_false(false) -> false;
true_or_false(Expr) -> {error, {unevaluated_expression, Expr}}.
