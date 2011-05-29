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
-module(map).
-export([create/4]).

-include("map.hrl").
-include("player_pos.hrl").

%% === Public functions ===

%% Creates a standard map.
create(<<"standard">>, Width, Height, Players) ->
    Map = #map{
      size_x = Width,
      size_y = Height,
      metal = get_standard_metal(Width, Height),
      stone = []
     },
    Map#map{
      players = get_players_positions(Players, Map)
     }.

%% === Private functions ===

get_standard_metal(Width, Height) ->
    get_standard_metal(1, 1, Width, Height, []).

get_standard_metal(_, Y, _, Height, CoordList) when Y >= Height ->
    CoordList;
get_standard_metal(X, Y, Width, Height, CoordList) when X >= Width ->
    get_standard_metal(1, Y + 2, Width, Height, CoordList);
get_standard_metal(X, Y, Width, Height, CoordList) ->
    get_standard_metal(X + 2, Y, Width, Height, [[X, Y] | CoordList]).

%% This function places players on the map.
%% TODO: This variant just places players into first free cells of map. Change
%% this.
get_players_positions(Players, Map) ->
    get_players_positions(Players, [0, 0], Map, []).

get_players_positions([], _, _, Coords) ->
    Coords;
get_players_positions(_, [_, Y], _Map = #map{
                                   size_y = Height
                                  }, _) when Y >= Height ->
    {error, unable_to_place_players};
get_players_positions(Players, [X, Y], Map = #map{
                                   size_x = Width
                                  }, Coords) when X >= Width ->
    get_players_positions(Players, [0, Y + 1], Map, Coords);
get_players_positions(Players, [X, Y], Map = #map{
                                           size_x = Width
                                          }, _) when X > Width ->
    get_players_positions(Players, [0, Y + 1], Map, []);
get_players_positions([Player | Players], [X, Y], Map, CoordList) ->
    case is_free([X, Y], Map) of
        true ->
            PlayerPos = #player_pos{
              name = Player,
              pos = [X, Y]
             },
            get_players_positions(Players, [X + 1, Y], Map, [PlayerPos |
                                                             CoordList]);
        false ->
            get_players_positions([Player | Players], [X + 1, Y], Map, CoordList)
    end.

is_free(Coords, _Map = #map{
                  metal = Metal,
                  stone = Stone
                 }) ->
    Finder = fun (Elem) ->
                     Elem =:= Coords
             end,
    not (lists:any(Finder, Metal) or lists:any(Finder, Stone)).
