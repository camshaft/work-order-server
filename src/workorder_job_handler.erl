-module(workorder_job_handler).

-export([init/3]).
-export([rest_init/2]).
-export([service_available/2]).
-export([resource_exists/2]).
-export([content_types_provided/2]).
-export([to_json/2]).

-include("workorder.hrl").

-record(state, {
  conn,
  obj
}).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, #state{}}.

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
      {true, Req2, State#state{obj = Obj}}
  end.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, to_json},
    {<<"application/vnd.mogsie.work-order+json">>, to_json}
  ], Req, State}.

to_json(Req, State = #state{obj = Obj}) ->
  Job = workorder_riak:body(Obj),
  {jsx:encode(Job), Req, State}.

