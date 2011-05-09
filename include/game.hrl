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

-record(coords,
        {
          x=0,
          y=0
        }).

%% Record for game map.
-record(map,
        {
          size_x=0,
          size_y=0,
          objects=[]
        }).

%% Record for game in wait state.
-record(game_wait_state,
        {
          name="",
          min_players=0,
          max_players=0,
          players=[],
          map=#map{}
        }).

-record(game_running_state,
        {
          name="",
          turn=0,
          players=[],
          map=#map{}
        }).

-record(player,
        {
          name="",
          coords=#coords{}
        }).

-define(Log, io:format).
