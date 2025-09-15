-module(ws_handler).

-behaviour(cowboy_websocket).

%% Callbacks
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

%% starts HTTP connection before upgrading to WS
init(Req, _Opts) ->
  {cowboy_websocket, Req, #{}}.

%% called when upgraded to websocket
websocket_init(State) ->
  global_state:add_client(self()),
  {reply, {text, map_generator:default_map()}, State}.

%% handling messages
websocket_handle({text, <<"player: ", Name/binary>>}, State) ->
  global_state:update_client(self(), [{username, Name}]),
  io:format("Client connected: ~s~n", [Name]),
  {ok, State};
websocket_handle({text, Msg}, State) ->
  io:format("Message received: ~s~n", [Msg]),
  {ok, State};
websocket_handle(_Other, State) ->
  {ok, State}.

%% handling system/OTP messages
websocket_info({broadcast, Msg}, State) ->
  {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, _Req, _State) ->
  global_state:remove_client(self()),
  io:format("Client disconnected~n"),
  ok.
