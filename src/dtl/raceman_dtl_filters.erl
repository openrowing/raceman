-module(raceman_dtl_filters).
-export([date_from_ts/2]).

date_from_ts(Timestamp, Format) when is_integer(Timestamp), is_binary(Format) ->
  Time = calendar:now_to_local_time({
    Timestamp div 1000000,
    Timestamp rem 1000000,
    Timestamp rem 1}),
  erlydtl_dateformat:format(Time, Format).
