-module(regattas_resource).

-export([init/1, to_html/2, content_types_provided/2, resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {neo, regatta}).

init(_Config) ->
  Neo = neo4j:connect([{base_uri, <<"http://localhost:7474/db/data/">>}]),
  {ok, #context{neo = Neo}}.

content_types_provided(ReqData, Context) ->
  {[{"text/html", to_html}], ReqData, Context}.

resource_exists(ReqData, Context) ->
  {true, ReqData, Context}.

to_html(ReqData, Context) ->

  {Page, PageSize} = wrq_utils:page_and_page_size(ReqData),

  {ok, Regattas} = regatta_service:load_regattas(Context#context.neo, [{page, Page}, {page_size, PageSize}]),

  {ok, Content} = regattas_dtl:render([{regattas, Regattas},
									 {nextPage, Page + 1},
									 {prevPage, Page - 1}
									]),
  {Content, ReqData, Context}.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
