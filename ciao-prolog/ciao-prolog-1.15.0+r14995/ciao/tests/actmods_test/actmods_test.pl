:- module(actmods_test, [main/0], [assertions]).

:- use_module(library(system)).
:- use_module(library(format)).
:- use_module(library(terms)).
:- use_module(library(dec10_io)).
:- use_module(library(read)).

:- use_module(.('../misc_commands')).


dir_name(actmods_test).

server_name(simple_server).

client_name(simple_client_with_main).

server_path(ServPath):-
    dir_name(Dir),
    server_name(ServName),
    atom_concat([Dir, '/', ServName], ServPath).

client_path(CliPath):-
    dir_name(Dir),
    client_name(CliName),
    atom_concat([Dir, '/', CliName], CliPath).

:- test main.

main:-
        server_path(Server),
        client_path(Client),
        display('(Re)Compiling server and client'), nl,
        ciaoc_compile_flags(Server, '-a actmods/filebased_publish'),
        ciaoc_compile(Client),
        (
            file_exists(Client),
            file_exists(Server) ->
            display('Starting server'), nl,
            do_command([Server, ' &']),
            pause(1),
            server_name(ServName),
            atom_concat(ServName, '.addr', AddressFile),
            (
                file_exists(AddressFile) ->
                display('Starting client'), nl,
                do_command([Client]),
                %%        pause(3),
                %%%%%% What is the server PID?  It is stored in a file!
                server_name(ServerName),
                atom_concat(ServerName, '.addr', AddressFile),
                see(AddressFile),
                read(_Address),
                read(pid(PID)),
                seen,
                number_codes(PID, PidCodes), 
                atom_codes(AtomPid, PidCodes),
                atom_concat('kill ', AtomPid, KillCommand),
                display('Killing server'), 
                nl,
                shell(KillCommand)
            ;
                format("Uh oh!  There is no address file!~n", [])
            )
        ;
            format("Uh oh!  The server and client executables do not exist!~n",
            [])
        ).
