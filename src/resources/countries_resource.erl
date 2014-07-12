-module(countries_resource).

-export([init/1, to_html/2, content_types_provided/2, resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {neo, action, event_type, country, boatClass}).

init(_Config) ->
	Neo = neo4j:connect([{base_uri, <<"http://localhost:7474/db/data/">>}]),
    {ok, #context{neo = Neo}}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}], ReqData, Context}.

resource_exists(ReqData, Context) ->
  	{event_type, EventType} = lists:keyfind(event_type, 1, wrq:path_info(ReqData)),

	case lists:keyfind(id, 1, wrq:path_info(ReqData)) of
		{id, Code} ->
			{true, ReqData, Context#context{action=show, event_type=EventType, country=Code}};
		false ->
			case lists:keyfind(id_with_class_index, 1, wrq:path_info(ReqData)) of
				{id_with_class_index, Code} ->
					{true, ReqData, Context#context{action=show_class_index, event_type=EventType, country=Code}};
				false ->
					case lists:keyfind(id_with_class, 1, wrq:path_info(ReqData)) of
						{id_with_class, Code} ->
							{class, BoatClass} = lists:keyfind(class, 1, wrq:path_info(ReqData)),
							{true, ReqData, Context#context{action=show_class, event_type=EventType, country=Code, boatClass=BoatClass}};
						false ->
							{true, ReqData, Context#context{event_type=EventType, action=index}}
					end
			end;
		_Else ->
			{false, ReqData, Context}
	end.

to_html(ReqData, Context) when Context#context.action == index ->
  {ok, Countries} = neo4j_utils:transform_cypher_result(
							neo4j:cypher(Context#context.neo,
						 <<"MATCH (c:Country)--(b:Boat)--(f:Final)--(r:Race)--(e:Event) WHERE e.type = {event_type} RETURN DISTINCT c.name as country ORDER BY country">>,
						 [{<<"event_type">>, list_to_binary(Context#context.event_type)}]
						)),
    {ok, Content} = countries_index_dtl:render([{countries, Countries}, {event_type, Context#context.event_type}]),
    {Content, ReqData, Context};

to_html(ReqData, Context) when Context#context.action == show_class_index ->
	  {ok, Boats} = neo4j_utils:transform_cypher_result(
			neo4j:cypher(Context#context.neo,
				<<"MATCH (c:Country) WHERE c.name = {code} MATCH c--(:Boat)--(:Final)--(r:Race)--(e:Event) WHERE e.type = {event_type} RETURN DISTINCT r.name as boatClass ORDER BY boatClass">>,
						[{<<"code">>, list_to_binary(Context#context.country)},
						 {<<"event_type">>, list_to_binary(Context#context.event_type)}]
				)),
    {ok, Content} = countries_boat_class_index_dtl:render([
	    {boats, Boats},
	    {country, Context#context.country}
	  ]),
    {Content, ReqData, Context};

to_html(ReqData, Context) when Context#context.action == show_class ->
	  {ok, Events} = neo4j_utils:transform_cypher_result(
			neo4j:cypher(Context#context.neo,
				<<"MATCH (c:Country) WHERE c.name = {code} MATCH c--(b:Boat)--(f:Final)--(r:Race) WHERE r.name={boatClass} OPTIONAL MATCH f<-[:NEXT_FINAL*1..]-(:Final)-->(prev:Boat) WITH r, count(prev) + b.position AS rank ORDER BY rank WITH r, STR(collect(rank)) as ranks MATCH r--(e:Event)--(s:Season) RETURN ranks, {name: e.name, id: ID(e), year: s.year} as event ORDER BY s.year desc, e.name">>,
				[{<<"code">>, list_to_binary(Context#context.country)}, {<<"boatClass">>, list_to_binary(Context#context.boatClass)}]
			)),
    {ok, Content} = countries_boat_class_dtl:render([
    	{country, Context#context.country},
    	{events, Events},
    	{boatClass, Context#context.boatClass}
    ]),
    {Content, ReqData, Context};

to_html(ReqData, Context) when Context#context.action == show ->
		{ok, Cities} = neo4j_utils:transform_cypher_result(
			neo4j:cypher(Context#context.neo,
			<<"MATCH (c:Country) WHERE c.name = {code} MATCH c--(ci:City)--(e:Event)--(s:Season) WITH ci.name as city, e.name as event, ID(e) as eventId, s.year as year ORDER BY year RETURN city, collect({name: event, id: eventId, year: year}) as events ORDER BY city">>,
					[{<<"code">>, list_to_binary(Context#context.country)}]
			)),
    {ok, Content} = countries_show_dtl:render([
	    {cities, Cities},
	    {country, Context#context.country}
	  ]),
    {Content, ReqData, Context}.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
