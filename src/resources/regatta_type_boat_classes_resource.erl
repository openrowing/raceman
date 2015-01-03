-module(regatta_type_boat_classes_resource).

-export([init/1, to_html/2, content_types_provided/2, resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {neo, regatta_type}).

init(_Config) ->
  Neo = neo4j:connect([{base_uri, <<"http://localhost:7474/db/data/">>}]),
  {ok, #ctx{neo = Neo}}.

content_types_provided(ReqData, Context) ->
  {[{"text/html", to_html}], ReqData, Context}.

resource_exists(ReqData, Context) ->
  case regatta_types_service:load(Context#ctx.neo, ReqData) of
	false ->
	  {false, ReqData, Context};
	RegattaType ->
	  {true, ReqData, Context#ctx{regatta_type=RegattaType}}
  end.

to_html(ReqData, Context) ->
  Params = case wrq:get_qs_value("boat_class", ReqData) of
			 BoatClass when is_list(BoatClass) ->		   
			   {ok, CountriesAlpha} = neo4j_utils:transform_cypher_result(
										neo4j:cypher(Context#ctx.neo,
													 <<"MATCH (reg:Regatta)--(ev:Event) where reg.type={regatta_type} and ev.name={boat_class} WITH ev MATCH ev-[:FINALS]-(:Race)--(b:Boat)--(c:Country) WHERE b.position > 0 RETURN DISTINCT c.name as country ORDER BY country">>,
													 [{<<"regatta_type">>, proplists:get_value(<<"id">>, Context#ctx.regatta_type)}, 
													  {<<"boat_class">>, list_to_binary(BoatClass)}]
													)),
			   {ok, CountriesRank} = neo4j_utils:transform_cypher_result(
									   neo4j:cypher(Context#ctx.neo,
													<<"MATCH (reg:Regatta)--(ev:Event) where reg.type={regatta_type} and ev.name={boat_class} WITH ev MATCH ev-[:FINALS]-(r:Race)--(b:Boat)--(c:Country) WHERE b.position > 0 OPTIONAL MATCH r<-[:NEXT_FINAL*1..]-(:Race)-->(prev:Boat) WITH ev as event, c.name as country, count(prev) + b.position as rank ORDER BY rank WITH country, event, HEAD(collect(rank)) as bestRank RETURN country, avg(bestRank) as averageRank ORDER BY averageRank">>,
													[{<<"regatta_type">>, proplists:get_value(<<"id">>, Context#ctx.regatta_type)},
													 {<<"boat_class">>, list_to_binary(BoatClass)}]
												   )),
			   {ok, Events} = neo4j_utils:transform_cypher_result(
								neo4j:cypher(Context#ctx.neo,
											 <<"MATCH (s:Season)--(reg:Regatta)--(ev:Event) where reg.type={regatta_type} and ev.name={boat_class} WITH s, reg, ev MATCH ev-[:FINALS]-(r:Race)--(b:Boat)--(c:Country) WHERE b.position > 0 OPTIONAL MATCH r<-[:NEXT_FINAL*1..]-(:Race)-->(prev:Boat) WITH {name: ev.name, id: ID(ev), regattaId: ID(reg), year: s.year} as event, c.name as country, count(prev) + b.position as rank ORDER BY event.year, event.name, country, rank WITH event, {country: country, ranks: STR(collect(rank))} as country RETURN event, collect(country) as countries ORDER BY event.year DESC, event.name">>,
											 [{<<"regatta_type">>, proplists:get_value(<<"id">>, Context#ctx.regatta_type)},
											  {<<"boat_class">>, list_to_binary(BoatClass)}]
											)),
			   [{countries, CountriesAlpha}, {countriesByRank, CountriesRank}, {events, Events}, {boat_class, BoatClass}];
			 _ ->
			   []
		   end,
  
  {ok, BoatClasses} = regatta_types_service:load_boat_classes(Context#ctx.neo, Context#ctx.regatta_type), 
  
  {ok, Content} = regatta_type_boat_classes_dtl:render(Params ++ [{regatta_type, Context#ctx.regatta_type},
																 {boat_classes, BoatClasses}
																]),
  {Content, ReqData, Context}.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
