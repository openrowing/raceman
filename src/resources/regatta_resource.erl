-module(regatta_resource).

-export([init/1, to_html/2, content_types_provided/2, resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {neo, regatta}).

init(_Config) ->
  Neo = neo4j:connect([{base_uri, <<"http://localhost:7474/db/data/">>}]),
  {ok, #ctx{neo = Neo}}.

content_types_provided(ReqData, Context) ->
  {[{"text/html", to_html}], ReqData, Context}.

resource_exists(ReqData, Context) ->
  case regatta_service:load(Context#ctx.neo, ReqData) of
	false ->
	  {false, ReqData, Context};
	Regatta ->
	  {true, ReqData, Context#ctx{regatta=Regatta}}
  end.

to_html(ReqData, Context) ->
  Events = regatta_service:load_events(Context#ctx.neo, Context#ctx.regatta),
  {ok, Content} = regatta_dtl:render([{regatta, Context#ctx.regatta},
									{events, Events}
								   ]),
  {Content, ReqData, Context}.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
