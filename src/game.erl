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
-export([start/3, run/2, cancel/1]).
-export([init/3]).

-include("game_type.hrl").
-include("map.hrl").

-record(game_state,
        {
          game_id = <<"">>,
          map = #map{}
        }).

%% === Public functions ===

start(Server, Type, GameID) ->
    spawn_link(?MODULE, init, [Server, Type, GameID]).

run(Game, Players) ->
    Game ! {start, Players}.

cancel(Game) ->
    Game ! cancel,
    receive
        {cancelled, Game} ->
            ok
    after 5000 ->
            error
    end.

%% === Private functions ===

init(Server, Type, GameID) ->
    wait_loop(Server, Type, GameID).

wait_loop(Server, Type = #game_type{
                    map_name = MapName,
                    map_width = MapWidth,
                    map_height = MapHeight
                   }, GameID) ->
    receive
        {start, Players} ->
            Map = map:create(MapName, MapWidth, MapHeight, Players),
            ebomber:game_started(Server, GameID, Map),
            game_loop(#game_state{
                         game_id = GameID,
                         map = Map
                        });
        cancel ->
            Server ! {cancelled, self()};
        Unknown ->
            io:format("game recieved unknown message: ~p~n", [Unknown]),
            wait_loop(Server, Type, GameID)
    end.


%% TODO: Finish this function.
game_loop(State = #game_state{
            game_id = GameID,
            map = Map
         }) ->
    io:format("game:game_loop, State = ~p~n", [State]).
