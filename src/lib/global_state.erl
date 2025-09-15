-module(global_state).

-behaviour(gen_server).

%% API
-export([start_link/0, add_client/1, config_client/2, remove_client/1, broadcast/1,
         get_client_info/1, update_client/2, disconnect_client/1, initialize_client/2]).
%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(client,
        {pid :: pid() | undefined,
         username = "" :: string(),
         position = {0, 0} :: {integer(), integer()},
         weapon = "None" :: string(),
         health = 100 :: integer(),
         score = 0 :: integer()}).
-record(state, {clients = [] :: [#client{}]}).

%% --- API ---
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_client(Pid) ->
  gen_server:cast(?MODULE, {add, Pid}).

config_client(Pid, ClientInfo) ->
  gen_server:cast(?MODULE, {config, Pid, ClientInfo}).

remove_client(Pid) ->
  gen_server:cast(?MODULE, {remove, Pid}).

disconnect_client(Pid) ->
  gen_server:cast(?MODULE, {disconnect, Pid}).

broadcast(Message) ->
  gen_server:cast(?MODULE, {broadcast, Message}).

get_client_info(Pid) ->
  gen_server:call(?MODULE, {get_info, Pid}).

initialize_client(Pid, Updates) ->
  gen_server:cast(?MODULE, {initialize, Pid, Updates}).

update_client(Pid, Updates) ->
  gen_server:cast(?MODULE, {update, Pid, Updates}).

% ===========================================================================================
%% --- Callbacks ---
% ===========================================================================================

init([]) ->
  {ok, #state{}}.

handle_cast({add, Pid}, State = #state{clients = Clients}) ->
  NewClient = #client{pid = Pid},
  {noreply, State#state{clients = [NewClient | Clients]}};
handle_cast({config, Pid, ClientInfo}, State = #state{clients = Clients}) ->
  UpdatedClients =
    case lists:keyfind(Pid, #client.pid, Clients) of
      false ->
        NewClient = create_client_from_info(Pid, ClientInfo),
        [NewClient | Clients];
      Client ->
        UpdatedClient = update_client_info(Client, ClientInfo),
        lists:keyreplace(Pid, #client.pid, Clients, UpdatedClient)
    end,
  {noreply, State#state{clients = UpdatedClients}};
handle_cast({initialize, Pid, Updates}, State = #state{clients = Clients}) ->
  Username = proplists:get_value(username, Updates),

  case find_client_by_username(Username, Clients) of
    {ok, ExistingClient} when ExistingClient#client.pid =:= undefined ->
      io:format("Reconnection allowed for username: ~s~n", [Username]),
      UpdatedClient = ExistingClient#client{pid = Pid},
      FilteredClients = lists:filter(fun(C) -> C#client.username =/= Username end, Clients),
      UpdatedClients = [UpdatedClient | FilteredClients],
      {noreply, State#state{clients = UpdatedClients}};
    {ok, ExistingClient} when ExistingClient#client.pid =/= undefined ->
      io:format("Cliente with username ~s is already connected. Rejecting connection."
                "..~n",
                [Username]),
      Pid ! {disconnect, <<"username_taken">>},
      {noreply, State};
    {error, not_found} ->
      io:format("New client: ~s~n", [Username]),
      NewClient = create_client_from_info(Pid, Updates),

      UpdatedClients = [NewClient | Clients],

      {noreply, State#state{clients = UpdatedClients}}
  end;
handle_cast({update, Pid, Updates}, State = #state{clients = Clients}) ->
  UpdatedClients =
    case lists:keyfind(Pid, #client.pid, Clients) of
      false ->
        Clients;
      Client ->
        UpdatedClient = apply_updates(Client, Updates),
        lists:keyreplace(Pid, #client.pid, Clients, UpdatedClient)
    end,
  {noreply, State#state{clients = UpdatedClients}};
handle_cast({remove, Pid}, State = #state{clients = Clients}) ->
  UpdatedClients = lists:keydelete(Pid, #client.pid, Clients),
  {noreply, State#state{clients = UpdatedClients}};
handle_cast({disconnect, Pid}, State = #state{clients = Clients}) ->
  UpdatedClients =
    case lists:keyfind(Pid, #client.pid, Clients) of
      false ->
        Clients;
      Client ->
        UpdatedClient = apply_updates(Client, [{pid, undefined}]),
        lists:keyreplace(Pid, #client.pid, Clients, UpdatedClient)
    end,
  {noreply, State#state{clients = UpdatedClients}};
handle_cast({broadcast, Message}, State = #state{clients = Clients}) ->
  lists:foreach(fun(#client{pid = P}) -> P ! {broadcast, Message} end, Clients),
  {noreply, State};
handle_cast(_, State) ->
  {noreply, State}.

handle_call({get_info, Pid}, _From, State = #state{clients = Clients}) ->
  case lists:keyfind(Pid, #client.pid, Clients) of
    false ->
      {reply, {error, client_not_found}, State};
    Client ->
      {reply, {ok, Client}, State}
  end;
handle_call(_, _From, State) ->
  {reply, ok, State}.

handle_info(_, State) ->
  {noreply, State}.

terminate(_, _) ->
  ok.

code_change(_, State, _) ->
  {ok, State}.

% ===========================================================================================
%% --- Callbacks ---
% ===========================================================================================

create_client_from_info(Pid, ClientInfo) ->
  #client{pid = Pid,
          username = proplists:get_value(username, ClientInfo, ""),
          position = proplists:get_value(position, ClientInfo, {0, 0}),
          weapon = proplists:get_value(weapon, ClientInfo, "none"),
          health = proplists:get_value(health, ClientInfo, 100),
          score = proplists:get_value(score, ClientInfo, 0)}.

update_client_info(Client, ClientInfo) ->
  Client#client{username =
                  proplists:get_value(username, ClientInfo, Client#client.username),
                position = proplists:get_value(position, ClientInfo, Client#client.position),
                weapon = proplists:get_value(weapon, ClientInfo, Client#client.weapon),
                health = proplists:get_value(health, ClientInfo, Client#client.health),
                score = proplists:get_value(score, ClientInfo, Client#client.score)}.

apply_updates(Client, Updates) ->
  lists:foldl(fun ({username, Value}, Acc) ->
                    Acc#client{username = Value};
                  ({position, Value}, Acc) ->
                    Acc#client{position = Value};
                  ({weapon, Value}, Acc) ->
                    Acc#client{weapon = Value};
                  ({health, Value}, Acc) ->
                    Acc#client{health = Value};
                  ({score, Value}, Acc) ->
                    Acc#client{score = Value};
                  ({pid, Value}, Acc) ->
                    Acc#client{pid = Value};
                  (Aa, Acc) ->
                    io:format("Ignoring update: ~p~n", [Aa]),
                    Acc
              end,
              Client,
              Updates).

find_client_by_username(Username, Clients) ->
  case lists:filter(fun(Client) -> Client#client.username =:= Username end, Clients) of
    [] ->
      {error, not_found};
    [Client] ->
      {ok, Client};
    [Client | _] ->
      {ok, Client}
  end.
