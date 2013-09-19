-module(workorder_root_handler).

-export([init/3]).
-export([rest_init/2]).
-export([service_available/2]).
-export([content_types_provided/2]).
-export([work_order_collection_to_json/2]).

-include("workorder.hrl").

-record(state, {
  conn,
  work_orders
}).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, #state{}}.

service_available(Req, State) ->
  case riakou:do(get_index_eq, [?JOBS_BUCKET, {binary_index, "waiting"}, <<1>>]) of
    {ok, {keys, Keys}} -> {true, Req, State#state{work_orders = Keys}};
    _Error ->
      io:format("~p~n", [_Error]),
      {false, Req, State}
  end.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, work_order_collection_to_json},
    {<<"application/collection+json">>, work_order_collection_to_json}
  ], Req, State}.

work_order_collection_to_json(Req, State=#state{work_orders=WorkOrders}) ->
  Body = to_json(WorkOrders),
  {jsx:encode(Body), Req, State}.

to_json(WorkOrders) ->
  [
    {<<"collection">>, [
      {<<"items">>, [
        [{<<"href">>, <<"/jobs/", WorkOrder/binary>>}] || WorkOrder <- WorkOrders
      ]}
    ]},
    {<<"version">>, <<"1.0">>}
  ].
