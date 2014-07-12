-module(event_types_resource).

-export([init/1, to_html/2, content_types_provided/2, resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {neo, action}).

init(_Config) ->
  Neo = neo4j:connect([{base_uri, <<"http://localhost:7474/db/data/">>}]),
  {ok, #context{neo = Neo}}.

content_types_provided(ReqData, Context) ->
  {[{"text/html", to_html}], ReqData, Context}.

resource_exists(ReqData, Context) ->
	  {true, ReqData, Context#context{action=index}}.

to_html(ReqData, Context) when Context#context.action == index ->
  
  {Page, PageSize} = wrq_utils:page_and_page_size(ReqData),
  
  {ok, EventTypes} = neo4j_utils:transform_cypher_result(
				   neo4j:cypher(Context#context.neo,
								<<"MATCH (e:Event) RETURN DISTINCT e.type AS id, e.type AS name SKIP {pageSkip} LIMIT {pageSize}">>,
								[{<<"pageSkip">>, (Page - 1) * PageSize},{<<"pageSize">>, PageSize}]
							   )),
  
  {ok, Content} = event_types_index_dtl:render([{event_types, EventTypes},
										   {nextPage, Page + 1},
										   {prevPage, Page - 1}
										  ]),
  {Content, ReqData, Context}.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
