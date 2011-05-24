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
-export([start_link/1, stop/1, received_data/2, player_accepted/2,
         game_started/1]).

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
          state = waiting
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

player_accepted(Server, Player) ->
    gen_server:cast(Server, {player_accepted, self(), Player}).

game_started(Server) ->
    gen_server:cast(Server, {game_started, self()}).

%% === gen_server behavior ===

init([Port]) ->
    io:format("ebomber:init~n"),
    Listener = json_socket_listener:start_link(self(), Port),
    State = #ebomber_state{
      game_types = get_game_types(),
      listener=Listener
     },
    {ok, State}.

handle_call(stop, _From, State) ->
    io:format("ebomber received stop request~n"),
    {stop, normal, State};
handle_call(Request, From, State) ->
    io:format("ebomber call with request ~p~n", [Request]),
    {noreply, State}.

handle_cast(Request, State) ->
    io:format("eBomber cast with request ~p~n", [Request]),
    NewState = handle_message(State, Request),
    {noreply, NewState}.

handle_info(Info, State) ->
    io:format("eBomber info message: ~p~n", [Info]),
    {noreply, State}.

terminate(normal, State=#ebomber_state{}) ->
    io:format("ebomber:terminate~n"),
    io:format("Stopping listener...~n"),
    ok = stop_listener(State#ebomber_state.listener),
    io:format("Cancelling games...~n"),
    ok = cancel_games(State#ebomber_state.games),
    io:format("ebomber terminated~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% === Private functions ===

handle_message(State, {received, Client, Message}) ->
    {Response, NewState} = process_message(State, Client, Message),
    Client ! {reply, Response},
    NewState.

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
            SessionID = list_to_binary(io_lib:format("~p", [make_ref()])),
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
                    GamesInfo = lists:map(fun game_info/1, get_game_types()),
                    Response = message:create(
                                 [
                                  message:create_key_value(status, <<"ok">>),
                                  message:create_key_value(session_id,
                                                           SessionID),
                                  message:create_key_value(your_name, Name),
                                  message:create_key_value(game_types,
                                                           GamesInfo)
                                 ]),
                    NewState = State#ebomber_state{clients = NewClients},
                    {Response, NewState};
                <<"observer">> ->
                    Observer = #client{
                      type = observer,
                      pid = From,
                      session_id = SessionID
                     },
                    NewClients = [Observer | State#ebomber_state.clients],

                    %% TODO: Response with information about available game
                    %% types AND currently running games.
                    GamesInfo = lists:map(fun game_info/1, get_game_types()),
                    Response = message:create(
                                 [
                                  message:create_key_value(status, <<"ok">>),
                                  message:create_key_value(session_id,
                                                           SessionID),
                                  message:create_key_value(your_name, EMail),
                                  %% TODO: Implement another naming mechanism?
                                  message:create_key_value(game_types,
                                                           GamesInfo)
                                 ]),
                    NewState = State#ebomber_state{clients = NewClients},
                    {Response, NewState}
            end;
        <<"join">> ->
            SessionID = message:get_value(session_id, Message),
            TypeID = message:get_value(type_id, Message),
            {ok, Player} = find_client({From, SessionID}, Clients),
            NewGameList = add_player_to_game(Player, TypeID, GameList,
                                             GameTypes),
            {undefined, State#ebomber_state{games = NewGameList}};
                                                % TODO: What must be responsed?
        <<"watch">> ->
            SessionID = message:get_value(session_id, Message),
            GameID = message:get_value(game_id, Message),
            {ok, Observer} = find_client({From, SessionID}, Clients),
            NewGameList = add_observer_to_game(Observer, GameID, GameList),
            {undefined, State#ebomber_state{games = NewGameList}}
                                                % TODO: What must be responsed?
    end.

game_info(GameType = #game_type{}) ->
    message:create([
      message:create_key_value(type_id, GameType#game_type.type_id),
      message:create_key_value(turn_time, GameType#game_type.turn_time),
      message:create_key_value(init_bombs_count,
                               GameType#game_type.init_bombs_count),
      message:create_key_value(max_bombs_count,
                               GameType#game_type.max_bombs_count),
      message:create_key_value(init_bomb_radius,
                               GameType#game_type.init_bomb_radius),
      message:create_key_value(bomb_delay, GameType#game_type.bomb_delay),
      message:create_key_value(min_players_count,
                               GameType#game_type.min_players_count),
      message:create_key_value(max_players_count,
                               GameType#game_type.max_players_count),
      message:create_key_value(map_name, GameType#game_type.map_name),
      message:create_key_value(map_width, GameType#game_type.map_width),
      message:create_key_value(map_height, GameType#game_type.map_height)
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

find_client({PID, SessionID}, ClientList) ->
    case lists:filter(fun (Client = #client{
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

add_player_to_game(Player, TypeID, GameList, GameTypes) ->
    case find_free_game(TypeID, GameList) of
        {ok, Game} ->
            %% TODO: Start game if needed amount of players available.
            OldPlayers = Game#game.players,
            replace_game(Game, Game#game{
                                 players = [Player | OldPlayers]
                                }, GameList);
        {error, not_found} ->
            [new_game(TypeID, GameTypes) | GameList]
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
    case lists:filter(fun (Game = #game{
                             type_id = CurrentTypeID,
                             state = CurrentState
                            }) ->
                              (CurrentState =:= waiting)
                                  and (CurrentTypeID =:= TypeID)
                      end, GameList) of
        [] ->
            {error, not_found};
        [Game, _Games] ->
            {ok, Game}
    end.

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
