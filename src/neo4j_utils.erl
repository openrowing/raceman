-module(neo4j_utils).

-export([transform_cypher_result/1, props/2, query_with_optional_pagination/2]).

props(Id, NodeProplist) ->
  lists:merge(proplists:get_value(<<"data">>, NodeProplist), [{<<"id">>, Id}]).

%% Transforms a Neo4j Result set to a more erlang style list of proplists

transform_cypher_result([{<<"columns">>, Columns}, {<<"data">>, Data}]) ->
  {ok, lists:map(fun(Row) -> lists:zip(Columns, Row) end, Data)};

transform_cypher_result([{columns, Columns}, {data, Data}]) ->
  {ok, lists:map(fun(Row) -> lists:zip(Columns, Row) end, Data)};

transform_cypher_result(_) ->
  false.

%% Adds LIMIT and SKIP clauses to Query if Options contains {page_size, <<integer>>} and {page, <<integer>>}
query_with_optional_pagination(Query, Options) ->
  PageSize = proplists:get_value(page_size, Options, 0),
  Page = proplists:get_value(page, Options, 1),
  if
    PageSize > 0 ->
      <<Query/binary, " SKIP ", (list_to_binary(integer_to_list((Page - 1) * PageSize)))/binary, " LIMIT ", (list_to_binary(integer_to_list(PageSize)))/binary>>;
      true ->
        Query
  end.

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
