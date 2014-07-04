-module(countries_resource).

-export([init/1, to_html/2, content_types_provided/2, resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {neo, action, cities, country}).

init(_Config) ->
	Neo = neo4j:connect([{base_uri, <<"http://localhost:7474/db/data/">>}]),
    {ok, #context{neo = Neo}}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}], ReqData, Context}.

resource_exists(ReqData, Context) ->
	case lists:keyfind(id, 1, wrq:path_info(ReqData)) of
		{id, Code} ->
		  case neo4j_utils:transform_cypher_result(
				neo4j:cypher(Context#context.neo,
					<<"MATCH (c:Country) WHERE c.name = {code} MATCH c--(ci:City)--(e:Event)--(s:Season) WITH ci.name as city, e.name as event, ID(e) as eventId, s.year as year ORDER BY s.year RETURN city, collect({name: event, id: eventId, year: year}) as events ORDER BY city">>,
							[{<<"code">>, list_to_binary(Code)}]
					)) of 
				{ok, Cities} ->
					{true, ReqData, Context#context{action=show, cities=Cities, country=Code}};
				_ ->
					{false, ReqData, Context}
			end;
		false ->
			{true, ReqData, Context#context{action=index}};
		_Else ->
			{false, ReqData, Context}
	end.

to_html(ReqData, Context) when Context#context.action == index ->
		{ok, Countries} = neo4j_utils:transform_cypher_result(
			neo4j:cypher(Context#context.neo,
				<<"MATCH (c:Country) RETURN c.name as country ORDER BY country">>)),
    {ok, Content} = countries_index_dtl:render([{countries, Countries}]),
    {Content, ReqData, Context};

to_html(ReqData, Context) when Context#context.action == show ->

    {ok, Content} = countries_show_dtl:render([
	    {cities, Context#context.cities},
	    {country, Context#context.country}
	  ]),
    {Content, ReqData, Context}.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
