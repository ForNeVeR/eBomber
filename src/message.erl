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
-module(message).
-export([create/1, create_key_value/2, get_value/2, set_value/3]).

%% This module contains service functions for working with internal message
%% format.

%% === Public functions ===

%% Creates internal message.
create(ListOfTuples) ->
    list_to_tuple(ListOfTuples).

%% Creates a key-value pair that can be given to previous function.
create_key_value(Key, Value) ->
    {Key, Value}.

%% Extracts value labeled by Key.
get_value(Key, Message) ->
    ListOfTuples = tuple_to_list(Message),
    {Key, Value} = lists:keyfind(Key, 1, ListOfTuples),
    Value.

%% Adds or replaces value.
set_value(Key, Value, Message) ->
    ListOfTuples = tuple_to_list(Message),
    list_to_tuple(lists:keystore(Key, 1, ListOfTuples, {Key, Value})).
