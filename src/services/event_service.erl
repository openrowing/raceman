-module(event_service).

-export([load/3, load_races/2]).

-include_lib("webmachine/include/wm_reqdata.hrl").

load(Neo, Regatta, Rest) when is_list(Regatta) ->
  load(Neo, proplists:get_value(<<"id">>, Regatta), Rest);

load(Neo, RegattaId, EventId) when is_integer(EventId) ->
  case neo4j_utils:transform_cypher_result(
		 neo4j:cypher(Neo,
					  <<"START reg=node({regattaId}), ev=node({eventId}) MATCH reg-->ev RETURN ID(ev) as id, ev.name as name">>,
					  [{<<"regattaId">>, RegattaId}, {<<"eventId">>, EventId}]
					 )) of
	{ok, [Event|_]} ->
	  Event;
	_ ->
	  false
  end;

load(Neo, RegattaId, ReqData) when is_record(ReqData, wm_reqdata) ->
  case lists:keyfind(event_id, 1, wrq:path_info(ReqData)) of
	{event_id, EventIdStr} ->
	  case string:to_integer(EventIdStr) of
		{error, _} ->
		  false;
		{EventId, _} ->
		  load(Neo, RegattaId, EventId)
	  end;
	_ ->
	  false
  end.

load_races(Neo, Event) ->
  {ok, Races} = neo4j_utils:transform_cypher_result(
				  neo4j:cypher(Neo,
							   <<"START ev=node({eventId}) MATCH (ev:Event)-->(r:Race) WITH r.type as type, ID(r) as raceId, r.name as name, r.date as date ORDER BY date DESC RETURN type, collect({raceId: raceId, name: name, date: date}) as races">>,
							   [{<<"eventId">>, proplists:get_value(<<"id">>, Event)}]
							  )),
  Races.
