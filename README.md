eBomber project
==============

Licensing
=========
All source code of this project is licensed under GNU GPLv3 license. See file
LICENSE for details.

mochijson2 library licensed under GPL-compartible MIT license.

Description
===========

Another server for bomberbot project. Written in Erlang.

See https://github.com/eugenezamriy/bomberbot for details.

Building
========

To build current version of source code, execute following commands:

    $ cd eBomber
    $ md ebin
    $ erl -make

Using
=====

Currently, your only option is running Erlang console:

    $ cd ebin && erl

and direct testing any exported functions of eBomber modules.

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

Main module is ebomber. It processes data from clients and redirects it to
games. At start it spawns json_socket_listener (and any other types of
listeners, if any other of them will exist). At need it spawns game processes.

### json_socket_listener module

json_socket_listener is process for listening sockets and spawning
json_connector processes for connecting clients.

### json_connector module

json_connector is process that processes JSON data recieved from clients. Then
it sends processed data to main server.

### game module

game process spawns timer process at start, and may spawn bomb processes when
game is running.
