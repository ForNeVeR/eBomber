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

### Starting server

First, run Erlang console from the ebin directory:

    $ cd ebin && erl

Then start ebomber server:

    1> ebomber_app:start().

Default listening port is 6666. You also may define port in the ebomber.app
file.

For now server only stays in memory and logs all incoming messages. Stay tuned.

### Stopping server

Use the following function call:

    2> ebomber_app:stop().

This will stop the whole application.

Architecture
============

Note that project is still not finished, so this is more a notes about future
project structure.

### Common

All internal data communication is performed in Erlang format; no JSON inside
server. JSON data parsed when received from clients and created before sending
to client by json_connector process/module. In future, server may be expanded to
handle other data types and connection methods.

### Data format

Inside server, all data packets represented as Erlang terms. Here I'll note
differences between JSON and Erlang data formats.

Please note that eBomber do not use mochijson2's object notation. I think it's
slightly confusing.

JSON struct / object maps to Erlang tuple of tuples. Keys are always represented
by atoms, values vary. For example, following JSON object:

    { "key1": "value1", "key2": "value2" }

may be represented as Erlang term

    {{key1, <<"value1">>}, {key2, <<"value2">>}}

Strings are always saved as binaries; never as lists (see previous example).

Lists are stored as lists, numbers as numbers. Nothing to add here.

### ebomber_app module

This module implements Erlang application behavior and contains functions for
starting and stopping whole application. It is the main entry point for the
application user.

### ebomber module

Main program module. It processes data from clients and redirects it to games.
At start it spawns json_socket_listener (and any other types of listeners, if
any other of them will exist). At need it spawns game processes.

### game module

game process spawns timer process at start, and may spawn bomb processes when
the game is running.

### map module

Now this module only contains few helper functions for dealing with bomberbot
maps.

### message module

This module designed for managing internal message data format. All other
modules must use only functions exported from this module for dealing with
messages.

### connector module

This module contains generic functions for sending data to various connectors.

### json_socket_listener module

json_socket_listener is process for listening sockets and spawning
json_connector processes for connecting clients.

### json_connector module

json_connector is process that processes JSON data recieved from clients. Then
it sends processed data to main server.

Any connector module must have generic way to send some response back to client.

### json_converter module

This module converts messages between internal Erlang format and external JSON
format.
