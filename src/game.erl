%% This file is part of eBomber.
%%
%% eBomber is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% eBomber is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with eBomber.  If not, see <http://www.gnu.org/licenses/>.
-module(game).
-export([wait_loop/1]).

-include("game.hrl").

wait_loop(Players) ->
    receive
	{add_player, Name} ->
	    wait_loop(lists:append([Players, [Name]]));
	Unknown ->
	    ?Log("Game recieved unknown message: ~p", [Unknown])
    end.
