-module(workorder_root_handler).

-export([init/3]).
-export([rest_init/2]).
-export([service_available/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([allowed_methods/2]).
-export([work_order_collection_to_json/2]).
-export([create_work_order/2]).

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
  case riakou:do(get_index_eq, [?JOBS_BUCKET, {binary_index, "status"}, <<"Waiting">>]) of
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

content_types_accepted(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, []}, create_work_order},
    {{<<"application">>, <<"collection+json">>, []}, create_work_order}
  ], Req, State}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

work_order_collection_to_json(Req, State=#state{work_orders=WorkOrders}) ->
  Body = to_json(Req, WorkOrders),
  {jsx:encode(Body), Req, State}.

to_json(Req, WorkOrders) ->
  [
    {<<"collection">>, [
      {<<"items">>, [
        [{<<"href">>, cowboy_base:resolve([<<"jobs">>, WorkOrder], Req)}] || WorkOrder <- WorkOrders
      ]}
    ]},
    {<<"version">>, <<"1.0">>}
  ].

create_work_order(Req, State) ->
  {ok, Json, Req2} = cowboy_req:body(Req),
  WorkOrder = jsx:decode(Json),
  RiakObject = riakc_obj:new(?JOBS_BUCKET, undefined),
  RiakObject2 = workorder_riak:set_body(WorkOrder, RiakObject),
  RiakObject3 = workorder_riak:set_binary_index("status", <<"Waiting">>, RiakObject2),
  {ok, SavedRiakObject} = riakou:do(put, [RiakObject3]),
  Id = riakc_obj:key(SavedRiakObject),
  Url = cowboy_base:resolve([<<"jobs">>, Id], Req2),
  Req3 = cowboy_req:set_resp_header(<<"Location">>, Url, Req2),
  {true, Req3, State}.
