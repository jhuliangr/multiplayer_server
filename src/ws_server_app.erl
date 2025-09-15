%%%-------------------------------------------------------------------
%% @doc ws_server public API
%% @end
%%%-------------------------------------------------------------------

-module(ws_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  env_loader:load(),
  {Port, _} =
    string:to_integer(
      env_loader:get("PORT")),
  Dispatch = cowboy_router:compile([{'_', [{"/ws", ws_handler, []}]}]),
  {ok, _} =
    cowboy:start_clear(http_listener, [{port, Port}], #{env => #{dispatch => Dispatch}}),
  io:format("Websockets server started on port ~s~n", [env_loader:get("PORT")]),
  global_state:start_link(),
  ws_server_sup:start_link().

stop(_State) ->
  ok.
