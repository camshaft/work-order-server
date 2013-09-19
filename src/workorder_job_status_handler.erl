-module(workorder_job_status_handler).

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([status_work_order/2]).
-export([resource_exists/2]).
-export([service_available/2]).
-export([content_types_provided/2]).
-export([work_order_status_to_json/2]).
-export([rest_init/2]).

-include("workorder.hrl").

-record(state, {
  conn,
  obj,
  id
}).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, #state{}}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, []}, status_work_order},
    {{<<"application">>, <<"status+json">>, []}, status_work_order}
  ], Req, State}.

content_types_provided(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, []}, work_order_status_to_json},
    {{<<"application">>, <<"status+json">>, []}, work_order_status_to_json}
  ], Req, State}.

service_available(Req, State) ->
  case riakou:take() of
    {error, _} ->
      {false, Req, State};
    Pid ->
      {true, Req, State#state{conn = Pid}}
  end.

resource_exists(Req, State = #state{conn = Pid}) ->
  {ID, Req2} = cowboy_req:binding(id, Req, <<>>),
  case riakc_pb_socket:get(Pid, ?STATUS_BUCKET, ID) of
    {error, notfound} ->
      {false, Req2, State#state{id = ID, obj = riakc_obj:new(?STATUS_BUCKET, ID)}};
    {error, _} ->
      {halt, Req2, State};
    {ok, Obj} ->
      {true, Req2, State#state{obj = Obj, id = ID}}
  end.

status_work_order(Req, State) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  UpdatedObj = workorder_riak:set_body(jsx:decode(Body), State#state.obj),
  ok = riakc_pb_socket:put(State#state.conn, UpdatedObj),
  {true, Req2, State}.

work_order_status_to_json(Req, State) ->
  Body = workorder_riak:body(State#state.obj),
  {jsx:encode(Body), Req, State}.
