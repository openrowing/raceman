-module(events_resource).

-export([init/1, to_html/2, content_types_provided/2, resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {neo, action, event}).

init(_Config) ->
	Neo = neo4j:connect([{base_uri, <<"http://localhost:7474/db/data/">>}]),
    {ok, #context{neo = Neo}}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}], ReqData, Context}.

resource_exists(ReqData, Context) ->
	case lists:keyfind(action_or_id, 1, wrq:path_info(ReqData)) of
		{action_or_id, "new"} ->
			{true, ReqData, Context#context{action=new}};
		{action_or_id, Str} ->
			case string:to_integer(Str) of
				{error, _Reason} ->
					{false, ReqData, Context};
				{Id, _Rest} ->
					Node = neo4j:get_node(Context#context.neo, Id),
					case Node of
						{error, not_found} ->
							{false, ReqData, Context};
						_Else ->
						  {true, ReqData, Context#context{action=show_or_update, event=Node}}
					end
			end;
		false ->
			{true, ReqData, Context#context{action=index_or_create}};
		_Else ->
			{false, ReqData, Context}
	end.

to_html(ReqData, Context) when Context#context.action == index_or_create ->

    {ok, Events} = neo4j_utils:transform_cypher_result(
			neo4j:cypher(Context#context.neo,
				<<"MATCH ((e:EVENT) <-- (s:SEASON)) RETURN ID(e) AS id, e.name AS name, s.year AS year, e.venueCity AS venueCity, e.venueCountry AS venueCountry ORDER BY s.year DESC, ID(e) DESC LIMIT 100">>
		)),

    {ok, Content} = events_index_dtl:render([
	    {events, Events}
	  ]),
    {Content, ReqData, Context};

to_html(ReqData, Context) when Context#context.action == show_or_update ->
    {erlang:iolist_to_binary([<<"<pre>">>,
				  mochiweb_html:escape(lists:flatten(io_lib:format("~80p", [Context#context.event]))),
				  <<"</pre>">>]),
	 ReqData, Context};

to_html(ReqData, Context) when Context#context.action == new ->
    {<<"TODO">>, ReqData, Context}.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
