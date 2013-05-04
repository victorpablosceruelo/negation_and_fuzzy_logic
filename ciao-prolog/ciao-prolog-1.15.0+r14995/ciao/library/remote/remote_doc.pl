:- module(remote_doc,
        [
 %%             (@)/2,
 %%              server_stop/1,
 %%              server_trace/1,
 %%              server_notrace/1,
 %%              serve/0
        ], [assertions]).


 %% :- use_module(library(remote(ciao_server_rt)), [serve/0]).
 %% :- reexport(library(remote(ciao_server_rt)), [serve/0]).

:- use_module(library(remote(ciao_client_rt)), 
        [(@)/2, server_stop/1, server_trace/1, server_notrace/1]).
:- reexport(library(remote(ciao_client_rt)),
        [(@)/2, server_stop/1, server_trace/1, server_notrace/1]).

:- doc(title, "The Ciao Remote Services Package").

:- doc(author, "Manuel Carro").

:- doc(module, "Module for The Ciao Remote Services Package").

:- doc(summary, "This package implements basic services to allow
   users to call remote Ciao Prolog servers.  Remote servers are
   processes living in a separate process, possibly in another
   machine, which offer @concept{predicates} as services.  This
   package makes it easy:

@begin{itemize}
@item To use a regular Ciao Prolog module as a service in the server.
@item To call a remotely-served predicate from any Ciao Prolog module.
@end{itemize}

   Unification of variables and attribute encoding/decoding (and thus
   constraint passing and predicate delay) are automatically handled
   by the package (see the examples in the 'examples' subdirectory).
   Concurrency, local and remote, can be used freely.").


:- doc(bug, "Dynamic loading of code not yet implemented.").
:- doc(bug, ":- remote/1 predicate declaration not yet implemented.").
:- doc(bug, "Remote use of modules (http, ftp, ciaotp) not yet implemented.").
:- doc(bug, "Remote creation of objects not yet implemented.").
:- doc(bug, "Code migration not yet implemented (several algorithms possible).").
:- doc(bug, "Evaluation of impact of marshalling and/or attribute encoding not yet done.").
:- doc(bug, "Secure transactions not yet implemented.").
