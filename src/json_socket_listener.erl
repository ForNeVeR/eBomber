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
-export([start_link/2]).
-export([init/2]).

%% === Public functions ===

start_link(Server, Port) ->
    io:format("json_socket_server:start_link~n"),
    spawn_link(?MODULE, init, [Server, Port]).

%% === Private functions ===

init(Server, Port) ->
    io:format("json_socket_server:init~n"),
    {ok, ServerSocket} = gen_tcp:listen(Port, [binary, {packet, raw}]),
    {ok, Connector, Ref} = spawn_new_connector(ServerSocket),
    loop(Server, ServerSocket, Connector, []).

spawn_new_connector(ServerSocket) ->
    json_connector:start_monitor(self(), ServerSocket).


loop(Server, ServerSocket, InactiveConnector, Connectors) ->
    receive
        {Connector, connected} ->
            io:format("Client connected."),
            {ok, NewConnector, Ref} = spawn_new_connector(ServerSocket),
            loop(Server, ServerSocket, NewConnector,
                 lists:append(Connectors, Connector));
        Unknown ->
            io:format("json_socket_listener received message ~p~n", Unknown),
            loop(Server, ServerSocket, InactiveConnector, Connectors)
    end.
