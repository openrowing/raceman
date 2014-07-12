-module(event_types_boat_classes_resource).

-export([init/1, to_html/2, content_types_provided/2, resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {neo, action, boat_class, event_type}).

init(_Config) ->
  Neo = neo4j:connect([{base_uri, <<"http://localhost:7474/db/data/">>}]),
  {ok, #context{neo = Neo}}.

content_types_provided(ReqData, Context) ->
  {[{"text/html", to_html}], ReqData, Context}.

resource_exists(ReqData, Context) ->
  {event_type, EventType} = lists:keyfind(event_type, 1, wrq:path_info(ReqData)),
  
  case wrq:get_qs_value("class", ReqData) of 
	undefined -> 
	  {true, ReqData, Context#context{action=select, event_type=EventType}};
	BoatClass ->
	  {true, ReqData, Context#context{action=show, boat_class=BoatClass, event_type=EventType}}
  end.

to_html(ReqData, Context) when Context#context.action == select ->
  {Classes} = load_boat_classes(Context#context.neo, Context#context.event_type),
  
  {ok, Content} = event_types_boat_classes_select_dtl:render([{event_type, Context#context.event_type},
															  {classes, Classes}
															 ]),
  {Content, ReqData, Context};

to_html(ReqData, Context) when Context#context.action == show ->
  {Classes} = load_boat_classes(Context#context.neo, Context#context.event_type),
  
  {ok, CountriesAlpha} = neo4j_utils:transform_cypher_result(
						   neo4j:cypher(Context#context.neo,
										<<"MATCH (s:Season)--(e:Event)--(r:Race) where e.type={event_type} and r.name={boat_class} WITH r MATCH r--(:Final)--(:Boat)--(c:Country) RETURN DISTINCT c.name as country ORDER BY country">>,
										[{<<"event_type">>, list_to_binary(Context#context.event_type)}, 
										 {<<"boat_class">>, list_to_binary(Context#context.boat_class)}]
									   )),
  {ok, CountriesRank} = neo4j_utils:transform_cypher_result(
						  neo4j:cypher(Context#context.neo,
									   <<"MATCH (s:Season)--(e:Event)--(r:Race) where e.type={event_type} and r.name={boat_class} WITH r MATCH r--(f:Final)--(b:Boat)--(c:Country) OPTIONAL MATCH f<-[:NEXT_FINAL*1..]-(:Final)-->(prev:Boat) WITH r as race, c.name as country, count(prev) + b.position as rank ORDER BY rank WITH country, race, HEAD(collect(rank)) as bestRank RETURN country, avg(bestRank) as averageRank ORDER BY averageRank">>,
									   [{<<"event_type">>, list_to_binary(Context#context.event_type)},
										{<<"boat_class">>, list_to_binary(Context#context.boat_class)}]
									  )),
  {ok, Events} = neo4j_utils:transform_cypher_result(
				   neo4j:cypher(Context#context.neo,
								<<"MATCH (s:Season)--(e:Event)--(r:Race) where e.type={event_type} and r.name={boat_class} WITH s,e,r MATCH r--(f:Final)--(b:Boat)--(c:Country) OPTIONAL MATCH f<-[:NEXT_FINAL*1..]-(:Final)-->(prev:Boat) WITH {name: e.name, id: ID(e), year: s.year} as event, c.name as country, count(prev) + b.position as rank ORDER BY event.year, event.name, country, rank WITH event, {country: country, ranks: STR(collect(rank))} as country RETURN event, collect(country) as countries ORDER BY event.year DESC, event.name">>,
								[{<<"event_type">>, list_to_binary(Context#context.event_type)},
								 {<<"boat_class">>, list_to_binary(Context#context.boat_class)}]
							   )),
  {ok, Content} = event_types_boat_class_show_dtl:render([{event_type, Context#context.event_type},
														  {countries, CountriesAlpha},
														  {countriesByRank, CountriesRank},
														  {events, Events},
														  {boat_class, Context#context.boat_class},
														  {classes, Classes}
														 ]),
  {Content, ReqData, Context}.

load_boat_classes(Neo, EventType) -> 
  {ok, Classes} = neo4j_utils:transform_cypher_result(
					neo4j:cypher(Neo,
								 <<"MATCH (r:Race)--(e:Event) WHERE e.type = {event_type} RETURN DISTINCT r.name as class ORDER BY class">>,
								 [{<<"event_type">>, list_to_binary(EventType)}]
								)),
  {Classes}.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
