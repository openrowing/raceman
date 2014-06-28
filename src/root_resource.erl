-module(root_resource).

-export([init/1, to_html/2, content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {}).

init(_Config) ->
    {ok, #context{}}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}],ReqData, Context}.

to_html(ReqData, Context) ->
    {ok, Content} = dashboard_dtl:render([]),
    {Content, ReqData, Context}.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
