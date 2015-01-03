-module(regatta_type_countries_resource).

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
  Params = case wrq:get_qs_value("country", ReqData) of
             CountryCode when is_list(CountryCode) ->   
               {ok, Regattas} = neo4j_utils:transform_cypher_result(
                                  neo4j:cypher(Context#ctx.neo,
                                               <<"MATCH (c:Country {name: {code}})--(b:Boat)--(r:Race)--(ev:Event)--(reg:Regatta {type: {regatta_type}})--(s:Season) WITH reg.name as name, ID(reg) as id, s.year as year, {name: ev.name, id: ID(ev)} AS event ORDER BY year RETURN name, id, year, collect(event) as events ORDER BY year DESC, name">>,
                                               [{<<"regatta_type">>, proplists:get_value(<<"id">>, Context#ctx.regatta_type)},
                                                {<<"code">>, list_to_binary(CountryCode)}]
                                              )),
               [{regattas, Regattas}, {country, CountryCode}];
             _ -> 
               []
           end,
  
  {ok, Countries} = neo4j_utils:transform_cypher_result(
                      neo4j:cypher(Context#ctx.neo,
                                   <<"MATCH (c:Country)--(:Boat)--(:Race)--(e:Event)--(reg:Regatta) WHERE reg.type = {regatta_type} RETURN DISTINCT c.name as country ORDER BY country">>,
                                   [{<<"regatta_type">>, proplists:get_value(<<"id">>, Context#ctx.regatta_type)}]
                                  )),
  
  {ok, Content} = regatta_type_countries_dtl:render(Params ++ [{countries, Countries}, 
                                                               {regatta_type, Context#ctx.regatta_type}]),
  {Content, ReqData, Context}.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
