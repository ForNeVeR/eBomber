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
-export([start_link/1, stop/1, received_data/2]).

-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% === Records ===

-record(ebomber_state,
        {
          listener = undefined,
          players = [],
          games = []
        }).

-record(game_type,
       {
         type_id = "",
         turn_time = 0,
         init_bombs_count = 0,
         max_bombs_count = 0,
         init_bomb_radius = 0,
         bomb_delay = 0,
         min_players_count = 0,
         max_players_count = 0,
         map_name = "",
         map_width = 0,
         map_height = 0
       }).

%% === Public functions ===

start_link(Port) ->
    io:format("ebomber:start_link~n"),
    gen_server:start_link(?MODULE, [Port], []).

stop(PID) ->
    gen_server:call(PID, stop).

%% == Functions for calls from connectors ==

%% Sends data received from client to eBomber server.
received_data(Server, Data) ->
    gen_server:cast(Server, {received, self(), Data}).

%% === gen_server behavior ===

init([Port]) ->
    io:format("ebomber:init~n"),
    Listener = json_socket_listener:start_link(self(), Port),
    {ok, #ebomber_state{listener=Listener}}.

handle_call(stop, From, State) ->
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

terminate(normal, State) ->
    io:format("ebomber:terminate~n"),
    Listener = State#ebomber_state.listener,
    Listener ! stop,
    receive
        {Listener, stopped} ->
            ok
    after 30000 ->
            error
    end,
    io:format("ebomber terminated~n").

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% === Private functions ===

handle_message(State, {received, Client, Request}) ->
    {Response, NewState} = process_request(State, Request),
    Client ! {reply, Response},
    NewState.

process_request(State=#ebomber_state{}, Request) ->
    {cmd, Command} = lists:keyfind(cmd, 1, Request),
    io:format("Processing command ~p~n", [Command]),
    case Command of
        "handshake" ->
            EMail = extract_value(email, Request),
            ID = extract_value(id, Request),
            "player" = extract_value(type, Request),
            %% TODO: Validate player.

            Player = {{session_id, SessionID = make_ref()},
                      {id, ID},
                      {email, EMail}},
            NewPlayers = [Player | State#ebomber_state.players],
            GamesInfo = lists:map(fun game_info/1, get_game_types()),
            Response = {
              {status, "ok"},
              {session_id, SessionID},
              {your_name, EMail}, %% TODO: Implement another naming mechanism.
              {game_types, GamesInfo}
             },
            NewState = State#ebomber_state{players = NewPlayers},
            {Response, NewState}
    end.

extract_value(Key, TupleList) ->
    {Key, Value} = lists:keyfind(Key, 1, TupleList),
    Value.

game_info(GameType=#game_type{}) ->
    {
      {type_id, GameType#game_type.type_id},
      {turn_time, GameType#game_type.turn_time},
      {init_bombs_count, GameType#game_type.init_bombs_count},
      {max_bombs_count, GameType#game_type.max_bombs_count},
      {init_bomb_radius, GameType#game_type.init_bomb_radius},
      {bomb_delay, GameType#game_type.bomb_delay},
      {min_players_count, GameType#game_type.min_players_count},
      {max_players_count, GameType#game_type.max_players_count},
      {map_name, GameType#game_type.map_name},
      {map_width, GameType#game_type.map_width},
      {map_height, GameType#game_type.map_height}
    }.

get_game_types() ->
    %% TODO: Implement this function. Query config for available game types.
    [#game_type{
        type_id = "test_game",
        turn_time = 30000,
        init_bombs_count = 1,
        max_bombs_count = 1,
        init_bomb_radius = 1,
        bomb_delay = 1,
        min_players_count = 2,
        max_players_count = 2,
        map_name = "standard",
        map_width = 5,
        map_height = 5
       }].
