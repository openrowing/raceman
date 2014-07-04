-module(races_resource).

-export([init/1, to_html/2, content_types_provided/2, resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {neo, action, event, eventId, boatClass}).

init(_Config) ->
	Neo = neo4j:connect([{base_uri, <<"http://localhost:7474/db/data/">>}]),
    {ok, #context{neo = Neo}}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}], ReqData, Context}.

resource_exists(ReqData, Context) ->
	case lists:keyfind(event, 1, wrq:path_info(ReqData)) of
		{event, StringId} ->
      case string:to_integer(StringId) of
        {error, _} ->
          {false, ReqData, Context};
        {EventId, _} ->
    			case lists:keyfind(class, 1, wrq:path_info(ReqData)) of
    				{class, BoatClass} ->
    					case neo4j_utils:transform_cypher_result(
    						neo4j:cypher(Context#context.neo,
    							<<"START e=node({event}) MATCH e--(s:Season) RETURN e.name as name, ID(e) as id, s.year as year">>,
    							[{<<"event">>, EventId}, {<<"boatClass">>, list_to_binary(BoatClass)}]
    						)) of
                {ok, [Event]} ->
                  {true, ReqData, Context#context{action=show, event=Event, boatClass=BoatClass, eventId=EventId}};
                _ ->
                  {false, ReqData, Context}
              end;
    				false ->
    					{false, ReqData, Context}
    		  end
      end;
		false ->
			{false, ReqData, Context};
		_Else ->
			{true, ReqData, Context}
	end.

to_html(ReqData, Context) when Context#context.action == show ->
    {ok, Boats} = neo4j_utils:transform_cypher_result(
      neo4j:cypher(Context#context.neo,
        <<"START e=node({event}) MATCH e-->(r:Race)-->(f:Final)-->(b:Boat)-->(c:Country) WHERE r.name={boatClass} OPTIONAL MATCH f<-[:NEXT_FINAL*1..]-(:Final)-->(prev:Boat) RETURN b.name as boat, c.name as country, count(prev) + b.position AS rank, b.time as time, e.name as event ORDER BY rank">>,
        [{<<"event">>, Context#context.eventId}, {<<"boatClass">>, list_to_binary(Context#context.boatClass)}]
      )),
    {ok, Content} = races_show_dtl:render([
    	{boats, Boats},
    	{boatClass, Context#context.boatClass},
      {event, Context#context.event}
    ]),
    {Content, ReqData, Context}.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
