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
-export([start/4, wait_loop/1]).

-include("game.hrl").

%% === Public functions ===

start(Name, MinPlayers, MaxPlayers, Map) ->
    spawn_link(?MODULE, wait_loop, [#game_wait_state {
                                      name=Name,
                                      min_players=MinPlayers,
                                      max_players=MaxPlayers,
                                      players=[],
                                      map=Map
                                    }]).

wait_loop(State=#game_wait_state {
            name=Name,
            min_players=MinPlayers,
            max_players=MaxPlayers,
            players=Players,
            map=Map
           }) ->
    receive
        {Server, add_player, Name} ->
            NewPlayers = lists:append([Players, [Name]]),
            Server ! {self(), accepted, Name},
            if
                length(NewPlayers) == MaxPlayers ->
                    Server ! {self(), started},
                    game_loop(#game_running_state {
                                 name=Name,
                                 players=NewPlayers,
                                 map=Map
                              });
                true ->
                    wait_loop(State#game_wait_state {
                                players=NewPlayers
                               })
            end;
        {Server, cancel} ->
            Server ! {self(), ok};
        Unknown ->
            ?Log("Game recieved unknown message: ~p", [Unknown]),
            wait_loop(State)
    end.

%% === Private functions ===

%% TODO: Finish this function.
game_loop(State=#game_running_state{}) ->
    ?Log("Game entered game_loop function, state=~p.", State).
