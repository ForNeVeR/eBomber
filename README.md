eBomber project
==============

Licensing
=========
All source code of this project is licensed under GNU GPLv3 license. See the
LICENSE file for details.

mochijson2 library licensed under GPL-compartible MIT license.

Description
===========

Another server for bomberbot project. Written in Erlang.

See https://github.com/eugenezamriy/bomberbot for details.

Building
========

First, you need last version of Erlang and OTP installed. See http://erlang.org
for details. To build current version of source code, execute following
commands:

    $ cd eBomber
    $ md ebin
    $ erl -make

Other way from Erlang console:

    $ erl
    Erlang R14B02 (erts-5.8.3) [smp:8:8] [rq:8] [async-threads:0]

    Eshell V5.8.3  (abort with ^G)
    1> cd("path/to/eBomber").
    path/to/eBomber
    ok
    2> make:all().
    ... (some lines skipped)
    up_to_date

Using
=====

First, run Erlang console:

    $ cd ebin && erl

Then start ebomber server:

    1> ebomber:start_link().

Default listening port is 6666. You also may use function
ebomber:start_link(Port) if you want to listen on another port.

For now server only stays in memory and logs all incoming messages. Stay tuned.

Architecture
============

Note that project is still not finished, so this is more a notes about future
project structure.

### Overall

All internal data communication is performed in Erlang format; no JSON inside
server. JSON data parsed when received from clients and created before sending
to client by json_connector process/module. In future, server may be expanded to
handle other data types and connection methods.

### ebomber module

Main program module. It processes data from clients and redirects it to games.
At start it spawns json_socket_listener (and any other types of listeners, if
any other of them will exist). At need it spawns game processes.

### json_socket_listener module

json_socket_listener is process for listening sockets and spawning
json_connector processes for connecting clients.

### json_connector module

json_connector is process that processes JSON data recieved from clients. Then
it sends processed data to main server.

Any connector module must have generic way to send some response back to client.

### game module

game process spawns timer process at start, and may spawn bomb processes when
the game is running.
