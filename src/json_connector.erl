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
-module(json_connector).
-export([start_monitor/3, stop/1]).
-export([init/3]).

%% === Public functions ===

start_monitor(Listener, ServerSocket, Server) ->
    io:format("json_connector:start_monitor~n"),
    {Pid, Ref} = spawn_monitor(?MODULE, init, [Listener, ServerSocket, Server]),
    {ok, Pid, Ref}.

stop(PID) ->
    PID ! stop,
    receive
        {PID, stopped} ->
             ok
    after 30000 ->
            timeout
    end.

%% === Private functions ===

init(Listener, ServerSocket, Server) ->
    io:format("json_connector:init~n"),
    case gen_tcp:accept(ServerSocket) of
        {ok, Socket} ->
            Listener ! {self(), connected},
            loop(Listener, Socket, Server, []);
        {error, closed} ->
            Listener ! {self(), closed}
    end.

loop(Listener, Socket, Server, PendingData) ->
    receive
        {tcp_closed, Socket} ->
            Server ! {self(), client_disconnected};
        {tcp_error, Socket, _Reason} ->
            Server ! {self(), client_disconnected};
        {tcp, Socket, Binary} ->
            %% Recieving data...
            Data = binary_to_list(Binary),
            FullData = lists:append(PendingData, Data),
            %% Null character as packet delimeter:
            case lists:member(0, FullData) of
                true ->
                    Packet = lists:takewhile(fun(Byte) -> Byte =/= 0 end,
                                              FullData),
                    ok = parse_packet(Server, Packet),
                    loop(Listener, Socket, Server,
                         lists:nthtail(length(Packet) + 1, FullData));
                false ->
                    loop(Listener, Socket, Server, FullData)
            end;
        {reply, Message} ->
            io:format("Sending message to client: ~p~n", [Message]),
            gen_tcp:send(Socket,
                         lists:append(json_converter:message_to_json(Message),
                                      [0])),
            loop(Listener, Socket, Server, PendingData);
        stop ->
            Listener ! stopped,
            ok;
        Unknown ->
            io:format("json_connector received unknown message: ~p~n",
                      [Unknown]),
            loop(Listener, Socket, Server, PendingData)
    end.

parse_packet(Server, Packet) ->
    io:format("Parsing packet ~p~n", [Packet]),
    Message = json_converter:json_to_message(Packet),
    io:format("Received message: ~p~n", [Message]),
    ebomber:received_data(Server, Message),
    ok.
