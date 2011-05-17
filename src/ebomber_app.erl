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
-module(ebomber_app).
-export([start/0, stop/0]).

-behavior(application).
-export([start/2, start_phase/2, prep_stop/1, stop/1, config_change/3]).

%% === Public functions ===

start() ->
    application:start(ebomber).

stop() ->
    application:stop(ebomber).

%% === application behavior ===

start(normal, [Port]) ->
    io:format("ebomber starting~n"),
    {ok, PID} = ebomber:start_link(Port),
    State = [PID],
    {ok, PID, State}.

start_phase(_, _) ->
    {error, not_implemented}.

prep_stop([PID]) ->
    ebomber:stop(PID),
    State = [],
    State.

stop(_) ->
    io:format("ebomber stopped~n"),
    ok.

config_change(_, _, _) ->
    ok.
