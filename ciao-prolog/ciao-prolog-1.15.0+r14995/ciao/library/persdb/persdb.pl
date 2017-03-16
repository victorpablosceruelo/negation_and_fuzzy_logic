:- package(persdb).
:- use_package(library(persdb(persdb_decl))).

:- use_module(library(persdb(persdbrt)), [%redefinebuiltins
        asserta_fact/1, 
        assertz_fact/1, 
        retract_fact/1,
	retractall_fact/1,
        init_persdb/0, 
        initialize_db/0,
        make_persistent/2,
        update_files/0,
        update_files/1]).

:- redefining(asserta_fact/1).
:- redefining(assertz_fact/1).
:- redefining(retract_fact/1).
:- redefining(retractall_fact/1).

:- initialization(init_persdb).

:- load_compilation_module(library(persdb(persdbtr))).
:- add_sentence_trans(persistent_tr/3, 1110).
