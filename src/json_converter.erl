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
-module(json_converter).
-export([message_to_json/1, json_to_message/1]).

%% TODO: This module must use 'message' module interface, not knowledge about
%% internal message structure.

%% === Public functions ===

message_to_json(Message) ->
    JSON = term_to_json(Message),
    Data = my_flatten(mochijson2:encode(JSON)),
    io:format("Prepared packet: ~p~n", [Data]),
    Data.

json_to_message(JSON) ->
    Decoded = mochijson2:decode(JSON),
    Message = decode_json_term(Decoded),
    io:format("Decoded message: ~p~n", [Message]),
    Message.

%% === Private functions ===

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
