-module(workorder_root_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([work_order_collection_to_json/2]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, work_order_collection_to_json}
  ], Req, State}.

work_order_collection_to_json(Req, State) ->
  Body = <<"{}">>,
  {Body, Req, State}.