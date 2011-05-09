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
-module(json_socket_listener).
-export([start_link/1]).
-export([init/1]).

%% === Public interface ===

start_link(Server) ->
    io:format("json_socket_server:start_link~n"),
    spawn_link(?MODULE, init, [Server]).

%% === Private functions ===

init(Server) ->
    io:format("json_socket_server:init~n"),
    %% TODO: Start socket listening.
    loop(Server).

loop(Server) ->
    receive
        Message ->
            io:format("json_socket_listener received message ~p.", Message),
            loop(Server)
    end.
