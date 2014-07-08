-module(neo4j_utils).

-export([transform_cypher_result/1, props/2]).

props(Id, NodeProplist) ->
  lists:merge(proplists:get_value(<<"data">>, NodeProplist), [{<<"id">>, Id}]).

transform_cypher_result([{<<"columns">>, Columns}, {<<"data">>, Data}]) ->
  {ok, lists:map(fun(Row) -> transform(Columns, Row) end, Data)};

transform_cypher_result([{columns, Columns}, {data, Data}]) ->
  {ok, lists:map(fun(Row) -> transform(Columns, Row) end, Data)};

transform_cypher_result(_) ->
  false.

transform([Column|Columns], [Item|Items]) ->
  lists:append([{Column, Item}], transform(Columns, Items));

transform([], []) ->
  [];

transform([_H|_T], []) ->
  [];

transform([], [_H|_T]) ->
  [].

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

robustness_test() ->
  ?assert(transform_cypher_result([]) =:= false).

simple_test() ->
  ?assertEqual({ok, [[{1, "foo"}, {2, "bar"}, {3, "baz"}]]},
			   transform_cypher_result([{columns, [1, 2, 3]}, {data, [["foo", "bar", "baz"]]}])).

edge_case_test() ->
  ?assertEqual({ok, [[{1, "foo"}]]},
			   transform_cypher_result([{columns, [1, 2, 3]}, {data, [["foo"]]}])),
  ?assertEqual({ok, [[{1, "foo"}, {2, "bar"}]]},
			   transform_cypher_result([{columns, [1, 2]}, {data, [["foo", "bar", "baz"]]}])).

	-endif.
