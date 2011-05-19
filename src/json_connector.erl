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
            loop(Socket, Server, []);
        {error, closed} ->
            Listener ! {self(), closed}
    end.

loop(Socket, Server, PendingData) ->
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
                    loop(Socket, Server, lists:nthtail(length(Packet) + 1,
                                                       FullData));
                false ->
                    loop(Socket, Server, FullData)
            end;
        {reply, Term} ->
            io:format("Sending term to client: ~p~n", [Term]),
            Packet = prepare_packet(Term),
            gen_tcp:send(Socket, Packet),
            loop(Socket, Server, PendingData);
        Unknown ->
            io:format("json_connector received unknown message: ~p~n",
                      [Unknown]),
            loop(Socket, Server, PendingData)
    end.

parse_packet(Server, Packet) ->
    io:format("Parsing packet ~p~n", [Packet]),
    JSON = mochijson2:decode(Packet),
    Parsed = decode_json_term(JSON),
    io:format("Decoded object: ~p~n", [Parsed]),
    ebomber:received_data(Server, Parsed),
    ok.

prepare_packet(Term) ->
    JSON = term_to_json(Term),
    io:format("Formed JSON: ~p~n", [JSON]),
    Data = my_flatten(mochijson2:encode(JSON)),
    io:format("Prepared packet: ~p~n", [Data]),
    lists:append(Data, "\0").

%% Flattens list and all binaries inside it.
%% Yeah, I know, it is not the best solution...
my_flatten(Byte) when is_integer(Byte) ->
    [Byte];
my_flatten(List) when is_list(List) ->
    lists:append(lists:map(fun my_flatten/1, List));
my_flatten(Binary) when is_binary(Binary) ->
    binary_to_list(Binary).

%% Decodes JSON terms from mochijson2 format to Erlang format.
decode_json_term(Number) when is_number(Number) ->
    Number;
decode_json_term(String) when is_binary(String) ->
    %% TODO: Parse string as unicode?
    String;
decode_json_term(List) when is_list(List) ->
    lists:map(fun decode_json_term/1, List);
decode_json_term({struct, Dictionary}) ->
    list_to_tuple(lists:map(fun decode_json_pair/1, Dictionary)).

decode_json_pair({Key, Value}) ->
    {binary_to_atom(Key, utf8), decode_json_term(Value)}.

term_to_json({Key, Value}) when is_atom(Key) ->
    {atom_to_binary(Key, utf8), term_to_json(Value)};
term_to_json(Object) when is_tuple(Object) ->
    {struct, lists:map(fun term_to_json/1, tuple_to_list(Object))};
term_to_json(Number) when is_number(Number) ->
    Number;
term_to_json(Ref) when is_reference(Ref) ->
    list_to_binary(io_lib:format("~p", [Ref]));
term_to_json(List) when is_list(List) ->
    lists:map(fun term_to_json/1, List);
term_to_json(String) when is_binary(String) ->
    String.
