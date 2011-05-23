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
-module(game).
-export([start/2, add_player/2, cancel/1]).
-export([init/2]).

-include("game_type.hrl").

%% === Public functions ===

start(Server, Type) ->
    spawn_link(?MODULE, init, [Server, Type]).

add_player(Game, Player) ->
    Game ! {add_player, Player}.

cancel(Game) ->
    Game ! cancel,
    receive
        {cancelled, Game} ->
            ok
    after 5000 ->
            error
    end.

%% === Private functions ===

init(Server, Type) ->
    wait_loop(Server, Type, []).

wait_loop(Server, Type = #game_type{max_players_count = MaxPlayers}, Players) ->
    receive
        {add_player, Player} ->
            NewPlayers = [Player | Players],
            ebomber:player_accepted(Server , Player),
            if
                length(NewPlayers) =:= MaxPlayers ->
                    ebomber:game_started(Server),
                    game_loop({}); % TODO: Fill some valid state here.
                length(NewPlayers) < MaxPlayers ->
                    wait_loop(Server, Type, NewPlayers)
            end;
        cancel ->
            Server ! {cancelled, self()};
        Unknown ->
            io:format("game recieved unknown message: ~p~n", [Unknown]),
            wait_loop(Server, Type, Players)
    end.


%% TODO: Finish this function.
game_loop(State) ->
    io:format("game:game_loop, State = ~p~n", [State]).
