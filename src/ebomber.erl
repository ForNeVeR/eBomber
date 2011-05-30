%% This file is part of eBomber.
%% Copyright (C) 2011 ForNeVeR
%%
%% eBomber is free software: you can redistribute it and/or modify it under the
%% terms of the GNU General Public License as published by the Free Software
%% Foundation, either version 3 of the License, or (at your option) any later
%% version.
%%
%% eBomber is distributed in the hope that it will be useful, but WITHOUT ANY
%% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
%% A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License along with
%% eBomber.  If not, see <http://www.gnu.org/licenses/>.
-module(ebomber).
-export([start_link/1, stop/1, received_data/2, game_started/3]).

-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("game_type.hrl").

%% === Records ===

-record(ebomber_state,
        {
          game_types = [],
          listener = undefined,
          clients = [],
          games = []
        }).

-record(client,
        {
          pid = undefined,
          type = undefined,
          session_id = <<"">>,
          name = <<"">>
        }).

-record(game,
        {
          pid = undefined,
          type_id = <<"">>,
          game_id = <<"">>,
          players = [],
          observers = [],
          status = waiting
        }).

%% === Public functions ===

start_link(Port) ->
    io:format("ebomber:start_link~n"),
    gen_server:start_link(?MODULE, [Port], []).

stop(PID) ->
    gen_server:call(PID, stop).

%% Sends data received from client to eBomber server.
received_data(Server, Data) ->
    gen_server:cast(Server, {received, self(), Data}).

game_started(Server, GameID, GameMap) ->
    gen_server:cast(Server, {game_started, GameID, GameMap}).

%% === gen_server behavior ===

init([Port]) ->
    io:format("ebomber:init~n"),
    Listener = json_socket_listener:start_link(self(), Port),
    State = #ebomber_state{
      game_types = get_game_types(),
      listener = Listener
     },
    {ok, State}.

handle_call(stop, _From, State) ->
    io:format("ebomber received stop request~n"),
    {stop, normal, ok, State};
handle_call(Request, _From, State) ->
    io:format("ebomber call with request ~p~n", [Request]),
    {noreply, State}.

handle_cast(Request, State) ->
    io:format("eBomber cast with request ~p~n", [Request]),
    NewState = handle_message(State, Request),
    {noreply, NewState}.

handle_info(Info, State) ->
    io:format("eBomber info message: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State = #ebomber_state{
                     listener = Listener,
                     games = Games
                    }) ->
    io:format("ebomber:terminate~n"),
    io:format("Stopping listener...~n"),
    ok = stop_listener(Listener),
    io:format("Cancelling games...~n"),
    ok = cancel_games(Games),
    io:format("ebomber terminated~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% === Private functions ===

handle_message(State, {received, Client, Message}) ->
    case process_message(State, Client, Message) of
        {reply, Response, NewState} ->
            Client ! {reply, Response},
            NewState;
        {noreply, NewState} ->
            NewState
    end;
handle_message(State = #ebomber_state{
                 game_types = GameTypes,
                 clients = Clients,
                 games = Games
                }, {game_started, GameID, GameMap}) ->
    io:format("Game ~p reported succesful start~n", [GameID]),
    io:format("Current game map: ~p~n", [GameMap]),
    %% TODO: Reply game_started packet to every interested client.
    {noreply, State}.

process_message(State = #ebomber_state{
                  game_types = GameTypes,
                  clients = Clients,
                  games = GameList
                 }, From, Message) ->
    Command = message:get_value(cmd, Message),
    io:format("Processing command ~p~n", [Command]),
    case Command of
        <<"handshake">> ->
            EMail = message:get_value(email, Message),
            _ID = message:get_value(id, Message), % TODO: Use ID for something?
            SessionID = make_unique_id(),
            %% TODO: Use other representation of reference here?

            case message:get_value(type, Message) of
                <<"player">> ->
                    Name = EMail, % TODO: Implement another naming mechanism?
                    Player = #client{
                      type = player,
                      pid = From,
                      session_id = SessionID,
                      name = Name
                     },
                    NewClients = [Player | Clients],
                    GamesInfo = lists:map(fun game_type_info/1,
                                          get_game_types()),
                    Response = message:create(
                                 [
                                  message:create_key_value(status, <<"ok">>),
                                  message:create_key_value(session_id,
                                                           SessionID),
                                  message:create_key_value(your_name, Name),
                                  message:create_key_value(game_types,
                                                           GamesInfo)
                                 ]),
                    NewState = State#ebomber_state{
                                 clients = NewClients
                                },
                    {reply, Response, NewState};
                <<"observer">> ->
                    Name = EMail, % TODO: Implement another naming mechanism?
                    Observer = #client{
                      type = observer,
                      pid = From,
                      session_id = SessionID
                     },
                    NewClients = [Observer | State#ebomber_state.clients],

                    TypesInfo = lists:map(fun game_type_info/1,
                                          get_game_types()),
                    GamesInfo = lists:map(fun game_info/1,
                                          lists:filter(
                                            fun (_Game = #game{
                                                   status = GameStatus
                                                  }) ->
                                                    GameStatus =:= running
                                            end, GameList)),
                    Response = message:create(
                                 [
                                  message:create_key_value(status, <<"ok">>),
                                  message:create_key_value(session_id,
                                                           SessionID),
                                  message:create_key_value(your_name, Name),
                                  message:create_key_value(game_types,
                                                           TypesInfo),
                                  message:create_key_value(games, GamesInfo)
                                 ]),
                    NewState = State#ebomber_state{
                                 clients = NewClients
                                },
                    {reply, Response, NewState}
            end;
        <<"join">> ->
            SessionID = message:get_value(session_id, Message),
            TypeID = message:get_value(type_id, Message),
            {ok, Player} = find_client({From, SessionID}, Clients),
            NewGameList = add_player_to_game(Player, TypeID, GameList,
                                             GameTypes),
            {noreply, State#ebomber_state{
                        games = NewGameList
                       }};
        <<"watch">> ->
            SessionID = message:get_value(session_id, Message),
            GameID = message:get_value(game_id, Message),
            {ok, Observer} = find_client({From, SessionID}, Clients),
            NewGameList = add_observer_to_game(Observer, GameID, GameList),
            {noreply, State#ebomber_state{
                        games = NewGameList
                       }}
    end.

make_unique_id() ->
    list_to_binary(io_lib:format("~p", [make_ref()])). % TODO: Something better?

game_type_info(GameType = #game_type{
                 type_id = TypeID,
                 turn_time = TurnTime,
                 init_bombs_count = InitBombsCount,
                 max_bombs_count = MaxBombsCount,
                 init_bomb_radius = InitBombRadius,
                 bomb_delay = BombDelay,
                 min_players_count = MinPlayersCount,
                 max_players_count = MaxPlayersCount,
                 map_name = MapName,
                 map_width = MapWidth,
                 map_height = MapHeight
                }) ->
    message:create(
      [
       message:create_key_value(type_id, TypeID),
       message:create_key_value(turn_time, TurnTime),
       message:create_key_value(init_bombs_count, InitBombsCount),
       message:create_key_value(max_bombs_count, MaxBombsCount),
       message:create_key_value(init_bomb_radius, InitBombRadius),
       message:create_key_value(bomb_delay, BombDelay),
       message:create_key_value(min_players_count, MinPlayersCount),
       message:create_key_value(max_players_count, MaxPlayersCount),
       message:create_key_value(map_name, MapName),
       message:create_key_value(map_width, MapWidth),
       message:create_key_value(map_height, MapHeight)
      ]).

get_game_types() ->
    %% TODO: Query config for available game types.
    [#game_type{
        type_id = <<"test_game">>,
        turn_time = 30000,
        init_bombs_count = 1,
        max_bombs_count = 1,
        init_bomb_radius = 1,
        bomb_delay = 1,
        min_players_count = 2,
        max_players_count = 2,
        map_name = <<"standard">>,
        map_width = 5,
        map_height = 5
       }].

find_game_type(TypeID, GameTypeList) ->
    case lists:filter(fun (_Type = #game_type{
                             type_id = CurrentTypeID
                            }) ->
                              CurrentTypeID =:= TypeID
                      end, GameTypeList) of
        [] ->
            {error, not_found};
        [Game | _Games] ->
            {ok, Game}
    end.

game_info(_Game = #game{
            type_id = TypeID,
            game_id = GameID,
            players = Players
           }) ->
    message:create(
      [
       message:create_key_value(type_id, TypeID),
       message:create_key_value(game_id, GameID),
       message:create_key_value(players, lists:map(
                                           fun (_Player = #client{
                                                  name = Name
                                                 }) ->
                                                   Name
                                           end, Players))
      ]).

find_client({PID, SessionID}, ClientList) ->
    case lists:filter(fun (_Client = #client{
                             pid = CurrentPID,
                             session_id = CurrentSID
                            }) ->
                              (CurrentPID =:= PID)
                                  and (CurrentSID =:= SessionID)
                      end, ClientList) of
        [] ->
            {error, not_found};
        [Client] ->
            {ok, Client};
        [_Client | _Clients] ->
            {error, multiple_found}
    end.

add_player_to_game(_Client = #client{
                     name = PlayerName
                    }, TypeID, GameList, GameTypesList) ->
    case find_free_game(TypeID, GameList) of
        {ok, Game = #game{
               pid = GamePID,
               players = OldPlayers
              }} ->
            Players = [PlayerName | OldPlayers],
            PlayersCount = length(Players),
            {ok, _GameType = #game_type{
                   min_players_count = MinPlayersCount
                  }} = find_game_type(TypeID, GameTypesList),
            io:format("Adding player ~p to game ~p~n", [PlayerName, Game]),
            io:format("Game now will have ~p players; start at ~p~n",
                      [PlayersCount, MinPlayersCount]),
            if
                PlayersCount =:= MinPlayersCount ->
                    Status = running,
                    game:run(GamePID, Players);
                PlayersCount < MinPlayersCount ->
                    Status = waiting
            end,
            replace_game(Game, Game#game{
                                 status = Status,
                                 players = Players
                                }, GameList);
        {error, not_found} ->
            [new_game(TypeID, GameTypesList, PlayerName) | GameList]
    end.

add_observer_to_game(Observer, GameID, GameList) ->
    case find_running_game(GameID, GameList) of
        {ok, Game} ->
            OldObservers = Game#game.observers,
            replace_game(Game, Game#game{
                                 observers = [Observer | OldObservers]
                                }, GameList)
    end.

find_free_game(TypeID, GameList) ->
    case lists:filter(fun (_Game = #game{
                             type_id = CurrentTypeID,
                             status = CurrentStatus
                            }) ->
                              (CurrentStatus =:= waiting)
                                  and (CurrentTypeID =:= TypeID)
                      end, GameList) of
        [] ->
            {error, not_found};
        [Game | _Games] ->
            {ok, Game}
    end.

find_running_game(GameID, GameList) ->
    case lists:filter(fun (_Game = #game{
                             game_id = CurrentGameID,
                             status = CurrentStatus
                            }) ->
                              (CurrentStatus =:= running)
                                  and (CurrentGameID =:= GameID)
                      end, GameList) of
        [] ->
            {error, not_found};
        [Game, _Games] ->
            {ok, Game}
    end.

replace_game(From, To, List) ->
    [To | lists:filter(fun (Game) ->
                               Game =/= From
                       end, List)].

new_game(TypeID, GameTypes, Player) ->
    io:format("Creating game of type ~p~n", [TypeID]),
    {ok, Type} = find_game_type(TypeID, GameTypes),
    GameID = make_unique_id(),
    PID = game:start(self(), Type, GameID),
    #game{
           pid = PID,
           type_id = TypeID,
           game_id = GameID,
           players = [Player]
         }.

stop_listener(Listener) ->
    Listener ! stop,
    receive
        {Listener, stopped} ->
            ok
    after 5000 ->
            error
    end.

cancel_games([]) ->
    ok;
cancel_games([Game, Other]) ->
    ok = game:cancel(Game),
    cancel_games(Other).
