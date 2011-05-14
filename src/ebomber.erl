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
-module(ebomber).
-export([start_link/0, start_link/1, cast/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-behavior(gen_server).

%% === State record ===

-record(ebomber_state,
        {
          players = [],
          games = []
        }).

%% === Public functions ===

start_link() ->
    start_link(6666).

start_link(Port) ->
    io:format("ebomber:start_link~n"),
    gen_server:start_link(?MODULE, [Port], []).

cast(Pid, Message) ->
    gen_server:cast(Pid, Message).

%% === gen_server behavior ===

init([Port]) ->
    io:format("ebomber:init~n"),
    json_socket_listener:start_link(self(), Port),
    {ok, #ebomber_state{}}.

handle_call(Request, From, State) ->
    io:format("eBomber call with request ~p~n", [Request]),
    NewState = handle_message(State, Request),
    {noreply, NewState}.

handle_cast(Request, State) ->
    io:format("eBomber cast with request ~p~n", [Request]),
    NewState = handle_message(State, Request),
    {noreply, NewState}.

handle_info(Info, State) ->
    io:format("eBomber info message: ~p~n", [Info]),
    {noreply, State}.

terminate(normal, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% === Private functions ===

handle_message(State, {received, Message}) ->
    process(State, Message).

process(State=#ebomber_state{}, Message) ->
    {cmd, Command} = lists:keyfind(cmd, 1, Message),
    io:format("Processing command ~p~n", [Command]),
    case Command of
        "handshake" ->
            EMail = extract_value(email, Message),
            ID = extract_value(id, Message),
            "player" = extract_value(type, Message),

            io:format("Adding player with email ~p to player pool.", [EMail]),
            Player = [{email, EMail}, {id, ID}],
            NewPlayers = lists:append([Player], State#ebomber_state.players),
            State#ebomber_state{players = NewPlayers}
    end.

extract_value(Key, TupleList) ->
    {Key, Value} = lists:keyfind(Key, 1, TupleList),
    Value.
