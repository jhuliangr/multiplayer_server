-module(ws_handler).

-behaviour(cowboy_websocket).

%% Callbacks
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

%% starts HTTP connection before upgrading to WS
init(Req, _Opts) ->
  {cowboy_websocket, Req, #{}}.

%% called when upgraded to websocket
websocket_init(State) ->
  {reply, {text, map_generator:default_map()}, State}.

%% handling messages
websocket_handle({text, <<"player: ", Name/binary>>}, State) ->
  Username = binary_to_list(Name),
  global_state:initialize_client(self(), [{username, Username}]),
  {ok, State};
websocket_handle({text, Msg}, State) ->
  io:format("Message received: ~s~n", [Msg]),
  {ok, State};
websocket_handle(_Other, State) ->
  {ok, State}.

%% handling system/OTP messages
websocket_info({broadcast, Msg}, State) ->
  {reply, {text, Msg}, State};
websocket_info({disconnect, Reason}, State) ->
  CloseCode =
    case Reason of
      <<"username_taken">> ->
        4001;
      <<"invalid_data">> ->
        4002;
      _ ->
        4000
    end,
  {reply, {close, CloseCode, Reason}, State};
websocket_info(Info, State) ->
  io:format("cayo aqui la info esta: ~p~n", [Info]),
  {ok, State}.

terminate(Reason, _Req, _State) ->
  case Reason of
    {remote, 1000, <<"Client disconnected">>} ->
      global_state:remove_client(self());
    {remote, 1001, _Text} ->
      global_state:remove_client(self());
    timeout ->
      global_state:disconnect_client(self());
    _ ->
      io:format("Reason for closing connection not handled: ~p~n", [Reason])
  end,
  io:format("Client disconnected~n"),
  ok.
