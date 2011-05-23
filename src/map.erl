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
-export([new/0, new/2, new/3]).

-include("map.hrl").

%% === Public functions ===

% Creates a new standard map.
new() ->
    new(5, 5).

% Creates a new map with specified size and default object placement.
new(SizeX, SizeY) ->
    new(SizeX, SizeY, default_objects(SizeX, SizeY)).

% Creates a new map with specified size and objects.
new(SizeX, SizeY, Objects) ->
    #map {
     size_x = SizeX,
     size_y = SizeY,
     objects = Objects
    }.

%% === Private functions ===

% Forms default object placement.
% TODO: Implement it.
default_objects(SizeX, SizeY) ->
    [].
