-module(regatta_types_service).

-export([load/2, load_all/1, load_all/2, load_boat_classes/2]).

-include_lib("webmachine/include/wm_reqdata.hrl").

load(Neo, ReqData) when is_record(ReqData, wm_reqdata) ->
  case lists:keyfind(regatta_type_id, 1, wrq:path_info(ReqData)) of
	{regatta_type_id, RegattaTypeId} ->
	  load(Neo, list_to_binary(RegattaTypeId));
	_ ->
	  false
  end;

load(Neo, RegattaTypeId) ->
  case neo4j_utils:transform_cypher_result(
		 neo4j:cypher(Neo,
					  <<"MATCH (reg:Regatta) WHERE reg.type={id} RETURN DISTINCT reg.type AS id, reg.type AS name">>,
					  [{<<"id">>, RegattaTypeId}]
					 )) of
	{ok, [RegattaType|_]} ->
	  RegattaType;
	_ ->
	  false
  end.

load_all(Neo) ->
  load_all(Neo, []).

load_all(Neo, Options) ->
  Query = <<"MATCH (reg:Regatta) RETURN DISTINCT reg.type AS id, reg.type AS name">>,
  neo4j_utils:transform_cypher_result(
	neo4j:cypher(Neo, neo4j_utils:query_with_optional_pagination(Query, Options))).

load_boat_classes(Neo, RegattaType) -> 
  neo4j_utils:transform_cypher_result(
	neo4j:cypher(Neo,
				 <<"MATCH (reg:Regatta)--(e:Event) WHERE reg.type = {regatta_type} RETURN DISTINCT e.name as class ORDER BY class">>,
				 [{<<"regatta_type">>, proplists:get_value(<<"id">>, RegattaType)}])).
