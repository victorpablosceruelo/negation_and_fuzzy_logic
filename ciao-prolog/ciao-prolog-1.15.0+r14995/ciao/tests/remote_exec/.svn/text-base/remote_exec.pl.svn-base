:- module(remote_exec, [main/0], [assertions]).

:- use_module(library(format)).
:- use_module(library(system)).

:- use_module('.'('../misc_commands')).


remote_server('remote_exec/server').
remote_client('remote_exec/client').

:- test main.

main:-
        format("Compiling remote client and server~n", []),
        remote_server(Server),
        remote_client(Client),
        ciaoc_compile(Server),
        ciaoc_compile(Client),
        (
            file_exists(Server),
            file_exists(Client) ->
            format("Starting client and server~n", []),
            current_host(Host),
            do_command([Server, ' &']),
            do_command([Client, ' ', Host])
        ;
            format("*** Something has happened --- Programs not created!~n",
            [])
        ).
