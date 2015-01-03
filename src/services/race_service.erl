-module(race_service).

-export([load/3, load_results/3]).

-include_lib("webmachine/include/wm_reqdata.hrl").

load(Neo, Regatta, Rest) when is_list(Regatta) ->
  load(Neo, proplists:get_value(<<"id">>, Regatta), Rest);

load(Neo, RegattaId, RaceId) when is_integer(RaceId) ->
  case neo4j_utils:transform_cypher_result(
		 neo4j:cypher(Neo,
					  <<"START reg=node({regattaId}), r=node({raceId}) MATCH (reg:Regatta)-->(ev:Event)-->(r:Race) RETURN ID(r) as id, ID(ev) AS event_id, r.name as name, r.date as date">>,
					  [{<<"regattaId">>, RegattaId}, {<<"raceId">>, RaceId}]
					 )) of
	{ok, [Race|_]} ->
	  Race;
	_ ->
	  false
  end;

load(Neo, RegattaId, ReqData) when is_record(ReqData, wm_reqdata) ->
  case lists:keyfind(race_id, 1, wrq:path_info(ReqData)) of
	{race_id, RaceIdStr} ->
	  case string:to_integer(RaceIdStr) of
		{error, _} ->
		  false;
		{RaceId, _} ->
		  load(Neo, RegattaId, RaceId)
	  end;
	_ ->
	  false
  end.

load_results(Neo, Event, RaceType) ->
  case neo4j_utils:transform_cypher_result(
		 neo4j:cypher(Neo,
					  <<"START ev=node({eventId}) MATCH (ev:Event)-->(r:Race)-->(b:Boat)-->(c:Country) WHERE r.type={raceType} OPTIONAL MATCH b-->(p:Person) WITH r, b, c, collect(p.firstName + ' ' + p.lastName) as names OPTIONAL MATCH b-[m:MEASUREMENT]->(d:Checkpoint) WITH r, b, c, names, CASE WHEN EXISTS(d.distance) THEN collect({distance: d.distance, time: m.time}) ELSE [] END as times ORDER BY r.abbr, b.position=0, b.position RETURN r.name as race, r.abbr as raceAbbr, collect({name: b.name, country: c.name, position: b.position, names: names, times: times}) as boats ORDER BY raceAbbr">>,
    			  [{<<"raceType">>, RaceType}, {<<"eventId">>, proplists:get_value(<<"id">>, Event)}]
					 )) of
	{ok, Results} ->
    BoatsDistExtractor = fun(Boat, DistancesMemo) ->
      Times = proplists:get_value(<<"times">>, Boat),
      Dists = lists:map(fun(Time) -> proplists:get_value(<<"distance">>, Time, []) end, Times),
      sets:union(DistancesMemo, sets:from_list(Dists))
    end,
    RaceDistExtractor = fun(Race, DistancesMemo) ->
      lists:foldl(BoatsDistExtractor, DistancesMemo, proplists:get_value(<<"boats">>, Race, []))
    end,
    Distances = lists:foldl(RaceDistExtractor, sets:new(), Results),
    erlang:display(Distances),
    {ok, Results, lists:sort(sets:to_list(Distances))};
	_ ->
	  false
  end.
