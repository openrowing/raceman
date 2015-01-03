-module(event_resource).

-export([init/1, to_html/2, content_types_provided/2, resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {neo, regatta, event}).

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
		  {true, ReqData, Context#context{regatta=Regatta,event=Event}}
	  end
  end.

to_html(ReqData, Context) ->
  RaceTypes = event_service:load_races(Context#context.neo, Context#context.event),
  {ok, Content} = event_dtl:render([{race_types, RaceTypes},
									{event, Context#context.event},
									{regatta, Context#context.regatta}
								   ]),
  {Content, ReqData, Context}.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
