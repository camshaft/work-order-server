%% @private
-module(workorder_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  ENV = simple_env:get("ERL_ENV", "production"),

  configure(ENV),

  Listeners = simple_env:get_integer("NUM_LISTENERS", 100),
  Port = simple_env:get_integer("PORT", 5000),

  RiakURL = simple_env:get_binary("RIAK_URL", <<"riak://localhost">>),
  ok = riakou:start_link(RiakURL),

  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", workorder_root_handler, []},
      {"/jobs/:id", workorder_jobs_handler, []},
      {"/jobs/:id/start", workorder_job_start_handler, []},
      {"/jobs/:id/status", workorder_job_status_handler, []},
      {"/jobs/:id/complete", workorder_job_complete_handler, []},
      {"/jobs/:id/fail", workorder_job_fail_handler, []}
    ]}
  ]),

  {ok, _} = cowboy:start_http(http, Listeners, [{port, Port}], [
    {compress, true},
    {env, [
      {dispatch, Dispatch}
    ]},
    {middlewares, [
      cowboy_empty_favicon,
      cowboy_base,
      cowboy_router,
      cowboy_handler
    ]}
  ]),

  io:format("Server started on port ~p", [Port]),
  workorder_sup:start_link().

stop(_State) ->
  ok.

configure("development") ->
  sync:go();
configure(_)->
  ok.
