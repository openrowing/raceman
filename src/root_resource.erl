-module(root_resource).

-export([init/1, to_html/2, content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {}).

init(_Config) ->
    {ok, #context{}}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}],ReqData, Context}.

to_html(ReqData, Context) ->
    Neo = neo4j:connect([{base_uri, <<"http://localhost:7474/db/data/">>}]),

    Node = neo4j:get_node(Neo, 0),

    {ok, Content} = dashboard_dtl:render(neo4j:get_node_properties(Node)),
    {Content, ReqData, Context}.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
