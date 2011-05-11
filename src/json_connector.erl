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
-export([start_monitor/2]).
-export([init/2]).

%% === Public functions ===

start_monitor(Listener, ServerSocket) ->
    {Pid, Ref} = spawn_monitor(?MODULE, init, [Listener, ServerSocket]),
    {ok, Pid, Ref}.

%% === Private functions ===

init(Listener, ServerSocket) ->
    gen_tcp:accept(ServerSocket),
    Listener ! {self(), connected},
    loop().

loop() ->
    receive
        Unknown ->
            io:format("json_connector recieved unknown message: ~p~n", Unknown),
            loop()
    end.
