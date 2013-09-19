-module(workorder_job_fail_handler).

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([fail_work_order/2]).
-export([resource_exists/2]).
-export([service_available/2]).

-include("workorder.hrl").

-record(state, {
  conn,
  obj
}).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {['PUT'], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"*/*">>, []}, fail_work_order}], Req, State}.	

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
    {error, _} ->
      {false, Req2, State};
    {ok, Obj} ->
      {true, Req2, State#state{obj = Obj}}
  end.

fail_work_order(Req, State) ->
	Obj = workorder_riak:body(State#state.obj),
	UpdatedObj = workorder_riak:set_body("Failed", Obj),
	riakc_pb_socket:put(State#state.conn, UpdatedObj),
	{ok, Req, State}.
