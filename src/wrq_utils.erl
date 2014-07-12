-module(wrq_utils).

-export([page_and_page_size/1]).

page_and_page_size(ReqData) ->
  Page = case string:to_integer(wrq:get_qs_value("page", "1", ReqData)) of
		   {error, _} -> 1;
		   {P, _} ->
			 if
			   P < 1 -> 1;
			   true -> P
			 end
		 end,
  {PageSize, _} = string:to_integer(wrq:get_qs_value("size", "30", ReqData)),
  {Page, PageSize}.
