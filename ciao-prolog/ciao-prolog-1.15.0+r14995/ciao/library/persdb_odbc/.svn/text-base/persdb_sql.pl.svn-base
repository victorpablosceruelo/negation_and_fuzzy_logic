:- package(persdb_sql).
:- use_module(library(persdb_sql(persdbrt_sql))).
:- load_compilation_module(library(persdb_sql_common(persdbtr_sql))).
% note: priority like 'persdb' (incompatible?)
:- add_sentence_trans(sql_persistent_tr/2, 1110).

:- use_package(assertions).

%% Data predicate to declare a predicate as a "sql persistent" one
:- multifile('$is_sql_persistent'/3).
:- discontiguous('$is_sql_persistent'/3).

%% Data predicate used to define "sql persistent locations"
:- multifile sql_persistent_location/2.
:- data sql_persistent_location/2.
:- discontiguous('sql_persistent_location'/2).

%% Data predicates used to keep the status information about the sessions and the queries : .......
 

%% Data predicates used by the pl2sql compiler 
:- multifile([relation/3,attribute/4]).
:- data([relation/3,attribute/4]).
:- discontiguous('relation'/3).
:- discontiguous('attribute'/4).
