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
-export([start_monitor/3]).
-export([init/3]).

%% === Public functions ===

start_monitor(Listener, ServerSocket, Server) ->
    {Pid, Ref} = spawn_monitor(?MODULE, init, [Listener, ServerSocket, Server]),
    {ok, Pid, Ref}.

%% === Private functions ===

init(Listener, ServerSocket, Server) ->
    gen_tcp:accept(ServerSocket),
    Listener ! {self(), connected},
    loop(Server, []).

loop(Server, PendingData) ->
    receive
        {tcp_closed, _Socket} ->
            Server ! {self(), client_disconnected};
        {tcp_error, _Socket, _Reason} ->
            Server ! {self(), client_disconnected};
        {tcp, _Socket, Binary} ->
            %% Recieving data...
            Data = binary_to_list(Binary),
            FullData = lists:append(PendingData, Data),
            %% Null character as packet delimeter:
            case lists:member(0, FullData) of
                true ->
                    Packet = lists:takewhile(fun(Byte) -> Byte =/= 0 end,
                                              FullData),
                    ok = parse_packet(Server, Packet),
                    loop(Server, lists:nthtail(length(Packet) + 1, FullData));
                false ->
                    loop(Server, FullData)
            end;
        Unknown ->
            io:format("json_connector received unknown message: ~p~n",
                      [Unknown]),
            loop(Server, PendingData)
    end.

parse_packet(Server, Packet) ->
    io:format("Parsing packet ~p~n", [Packet]),
    JSON = mochijson2:decode(Packet),
    Parsed = decode_json(JSON),
    io:format("Decoded object: ~p~n", [Parsed]),
    ebomber:cast(Server, {received, Parsed}),
    ok.

%% Receives packet in mochijson2 format (for example, {struct, {<<"key">>,
%% <<"value">>}}; constructs proper internal message from it (for example, {key,
%% "value"}).
decode_json(JSON) ->
    {struct, Dictionary} = JSON,
    lists:map(fun({Key, Value}) ->
                      decode_pair(binary_to_atom(Key, utf8), Value)
              end, Dictionary).

%% TODO: Add cases for special data structures in Value variable.
decode_pair(Key, Value) ->
    {Key, binary_to_list(Value)}.
