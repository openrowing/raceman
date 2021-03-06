-module(races_resource).

-export([init/1, to_html/2, content_types_provided/2, resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {neo, action, regatta, event, race_type}).

init(_Config) ->
  Neo = neo4j:connect([{base_uri, <<"http://localhost:7474/db/data/">>}]),
  {ok, #context{neo = Neo}}.

content_types_provided(ReqData, Context) ->
  {[{"text/html", to_html}], ReqData, Context}.

resource_exists(ReqData, Context) ->
  case regatta_service:load(Context#context.neo, ReqData) of
	false ->
	  {false, ReqData, Context};
	Regatta ->
	  case event_service:load(Context#context.neo, Regatta, ReqData) of
		false ->
		  {false, ReqData, Context};
		Event ->
     RaceType = list_to_binary(proplists:get_value(race_type, wrq:path_info(ReqData))),
		    {true, ReqData, Context#context{regatta=Regatta,event=Event,race_type=RaceType}}
	  end
  end.

to_html(ReqData, Context) ->
  {ok, Results, Distances} = race_service:load_results(Context#context.neo, Context#context.event, Context#context.race_type),
  {ok, Content} = races_dtl:render([{results, Results},
									{event, Context#context.event},
									{regatta, Context#context.regatta},
                  {distances, Distances}
								   ]),
  {Content, ReqData, Context}.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
