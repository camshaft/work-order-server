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

  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", workorder_root_handler, []}
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
