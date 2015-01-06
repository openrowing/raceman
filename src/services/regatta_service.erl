-module(regatta_service).

-export([load/2, load_events/2, load_regattas/1, load_regattas/2]).

-include_lib("webmachine/include/wm_reqdata.hrl").

load(Neo, RegattaId) when is_integer(RegattaId) ->
  case neo4j_utils:transform_cypher_result(
		 neo4j:cypher(Neo,
					  <<"START reg=node({id}) MATCH (s:Season)-->reg-->(city:City)-->(country:Country) RETURN ID(reg) AS id, reg.name AS name, reg.from AS from, reg.to AS to, s.year AS year, city.name AS venueCity, country.name AS venueCountry">>,
					  [{<<"id">>, RegattaId}]
					 )) of
	{ok, [Regatta|_]} ->
	  Regatta;
	_ ->
	  false
  end;

load(Neo, ReqData) when is_record(ReqData, wm_reqdata) ->
  case lists:keyfind(regatta_id, 1, wrq:path_info(ReqData)) of
	{regatta_id, RegattaIdStr} ->
	  case string:to_integer(RegattaIdStr) of
		{error, _} ->
		  false;
		{RegattaId, _} ->
		  load(Neo, RegattaId)
	  end;
	_ ->
	  false
  end.

load_regattas(Neo) ->
  load_regattas(Neo, []).

load_regattas(Neo, Options) ->
  Query = <<"MATCH (s:Season)-->(reg:Regatta)-->(city:City)-->(country:Country) RETURN ID(reg) AS id, reg.name AS name, s.year AS year, city.name AS venueCity, country.name AS venueCountry ORDER BY s.year DESC, reg.name">>,
  neo4j_utils:transform_cypher_result(
    neo4j:cypher(Neo, neo4j_utils:query_with_optional_pagination(Query, Options))
  ).

load_events(Neo, Regatta) ->
  {ok, Events} = neo4j_utils:transform_cypher_result(
				   neo4j:cypher(Neo,
								<<"START reg=node({id}) MATCH reg-->(ev:Event) OPTIONAL MATCH ev-->(r:Race) WITH ev, r.type as raceType, collect({name: r.name, date: r.date}) AS races RETURN ID(ev) AS id, ev.name AS name, ev.labelEn AS labelEn, ev.labelDe as labelDe, collect({type: raceType, races: races}) as raceTypes ORDER BY name">>,
                [{<<"id">>, proplists:get_value(<<"id">>, Regatta)}]
							 )),
  Events.
