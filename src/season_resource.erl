-module(season_resource).

-export([init/1, to_html/2, content_types_provided/2, resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {neo, action, year, boatClass}).

init(_Config) ->
	Neo = neo4j:connect([{base_uri, <<"http://localhost:7474/db/data/">>}]),
    {ok, #context{neo = Neo}}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}], ReqData, Context}.

resource_exists(ReqData, Context) ->
  case wrq:get_qs_value("year", ReqData) of 
    undefined -> 
      {true, ReqData, Context#context{action=select}};
    YearStr ->
      case string:to_integer(YearStr) of
        {error, _} ->
          {false, ReqData, Context};
        {Year, _ } ->
          case wrq:get_qs_value("class", ReqData) of 
            undefined -> 
              {false, ReqData, Context};
            BoatClass ->
              {true, ReqData, Context#context{action=show, year=Year, boatClass=BoatClass}}
          end
      end
  end.

to_html(ReqData, Context) when Context#context.action == select ->
  {ok, Years} = neo4j_utils:transform_cypher_result(
    neo4j:cypher(Context#context.neo,
      <<"MATCH (s:Season) RETURN s.year as year ORDER BY year DESC">>
    )),
  {ok, Classes} = neo4j_utils:transform_cypher_result(
    neo4j:cypher(Context#context.neo,
      <<"MATCH (r:Race) RETURN DISTINCT r.name as class ORDER BY class">>
    )),
  {ok, Content} = season_select_dtl:render([
      {years, Years},
      {classes, Classes}
    ]),
  {Content, ReqData, Context};

to_html(ReqData, Context) when Context#context.action == show ->
  {ok, Years} = neo4j_utils:transform_cypher_result(
    neo4j:cypher(Context#context.neo,
      <<"MATCH (s:Season) RETURN s.year as year ORDER BY year DESC">>
    )),
  {ok, Classes} = neo4j_utils:transform_cypher_result(
    neo4j:cypher(Context#context.neo,
      <<"MATCH (r:Race) RETURN DISTINCT r.name as class ORDER BY class">>
    )),
  {ok, CountriesAlpha} = neo4j_utils:transform_cypher_result(
    neo4j:cypher(Context#context.neo,
      <<"MATCH (s:Season)--(e:Event)--(r:Race) where s.year={year} and r.name={boatClass} WITH r MATCH r--(:Final)--(:Boat)--(c:Country) RETURN DISTINCT c.name as country ORDER BY country">>,
      [{<<"year">>, Context#context.year}, {<<"boatClass">>, list_to_binary(Context#context.boatClass)}]
    )),
  {ok, CountriesRank} = neo4j_utils:transform_cypher_result(
    neo4j:cypher(Context#context.neo,
      <<"MATCH (s:Season)--(e:Event)--(r:Race) where s.year={year} and r.name={boatClass} WITH r MATCH r--(f:Final)--(b:Boat)--(c:Country) OPTIONAL MATCH f<-[:NEXT_FINAL*1..]-(:Final)-->(prev:Boat) WITH r as race, c.name as country, count(prev) + b.position as rank ORDER BY rank WITH country, race, HEAD(collect(rank)) as bestRank RETURN country, avg(bestRank) as averageRank ORDER BY averageRank">>,
      [{<<"year">>, Context#context.year}, {<<"boatClass">>, list_to_binary(Context#context.boatClass)}]
    )),
  {ok, Events} = neo4j_utils:transform_cypher_result(
    neo4j:cypher(Context#context.neo,
      <<"MATCH (s:Season)--(e:Event)--(r:Race) where s.year={year} and r.name={boatClass} WITH e,r MATCH r--(f:Final)--(b:Boat)--(c:Country) OPTIONAL MATCH f<-[:NEXT_FINAL*1..]-(:Final)-->(prev:Boat) WITH {name: e.name, id: ID(e)} as event, c.name as country, count(prev) + b.position as rank ORDER BY event.name, country, rank WITH event, {country: country, ranks: STR(collect(rank))} as country RETURN event, collect(country) as countries">>,
      [{<<"year">>, Context#context.year}, {<<"boatClass">>, list_to_binary(Context#context.boatClass)}]
    )),
  {ok, Content} = season_show_dtl:render([
    	{countries, CountriesAlpha},
      {countriesByRank, CountriesRank},
      {events, Events},
    	{boatClass, Context#context.boatClass},
      {year, Context#context.year},
      {years, Years},
      {classes, Classes}
    ]),
  {Content, ReqData, Context}.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
