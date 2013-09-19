-module(workorder_job_start_handler).

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([start_work_order/2]).
-export([resource_exists/2]).
-export([service_available/2]).
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
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{'*', start_work_order}], Req, State}.

service_available(Req, State) ->
  case riakou:take() of
    {error, _} ->
      {false, Req, State};
    Pid ->
      {true, Req, State#state{conn = Pid}}
  end.

resource_exists(Req, State = #state{conn = Pid}) ->
  {ID, Req2} = cowboy_req:binding(id, Req, <<>>),
  case riakc_pb_socket:get(Pid, ?JOBS_BUCKET, ID) of
    {error, _} ->
      {false, Req2, State};
    {ok, Obj} ->
      {true, Req2, State#state{obj = Obj, id = ID}}
  end.

start_work_order(Req, State = #state{id = ID}) ->
  %set secondary key on jobs object
  Obj = State#state.obj,
  case workorder_riak:has_binary_index("status", <<"Waiting">>, Obj) of
    false ->
      {false, Req, State};
    _ ->
      UpdatedObj = workorder_riak:set_binary_index("status",<<"InProgress">>, State#state.obj),
      ok = riakc_pb_socket:put(State#state.conn, UpdatedObj),
      {ok, Req2} = cowboy_req:reply(303, [{<<"location">>, cowboy_base:resolve([<<"jobs">>, ID], Req)}], Req),
      {true, Req2, State}
  end.

