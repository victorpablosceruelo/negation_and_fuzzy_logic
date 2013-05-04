:- module( _plus1_multiple_too_few, [p/2], [assertions,nativeprops,regtypes,rtchecks] ).

:- use_module( library(aggregates) , [(^)/2 , bagof/3 , findall/3 , findall/4 , findnsols/4 , findnsols/5 , setof/3]).
:- use_module( library(between) , [between/3]).
:- use_module( library(compiler) , [ensure_loaded/1]).
:- use_module( library(dec10_io) , [close_file/1 , see/1 , seeing/1 , seen/0 , tell/1 , telling/1 , told/0]).
:- use_module( library(dynamic) , [abolish/1 , assert/1 , assert/2 , asserta/1 , asserta/2 , assertz/1 , assertz/2 , clause/2 , clause/3 , retract/1 , retractall/1 , wellformed_body/3]).
:- use_module( library(format) , [format/2 , format/3 , format_control/1]).
:- use_module( library(iso_byte_char) , [atom_chars/2 , char_code/2 , get_byte/1 , get_byte/2 , get_char/1 , get_char/2 , number_chars/2 , peek_byte/1 , peek_byte/2 , peek_char/1 , peek_char/2 , put_byte/1 , put_byte/2 , put_char/1 , put_char/2]).
:- use_module( library(iso_misc) , [\= /2 , compound/1 , once/1 , sub_atom/5 , unify_with_occurs_check/2]).
:- use_module( library(lists) , [add_after/4 , add_before/4 , append/3 , contains1/2 , contains_ro/2 , cross_product/2 , delete/3 , delete_non_ground/3 , difference/3 , dlist/3 , equal_lists/2 , insert_last/3 , intersection/3 , intset_delete/3 , intset_in/2 , intset_insert/3 , intset_sequence/3 , last/2 , length/2 , list1/2 , list_concat/2 , list_insert/2 , list_lookup/3 , list_lookup/4 , list_to_list_of_lists/2 , nocontainsx/2 , nonsingle/1 , nth/3 , powerset/2 , reverse/2 , reverse/3 , select/3 , sublist/2 , subordlist/2 , union/3]).
:- use_module( library(old_database) , [current_key/2 , recorda/3 , recorded/3 , recordz/3]).
:- use_module( library(operators) , [current_op/3 , op/3]).
:- use_module( library(prolog_sys) , [current_atom/1 , garbage_collect/0 , new_atom/1 , predicate_property/2 , statistics/0 , statistics/2]).
:- use_module( library(read) , [read/1 , read/2 , read_term/2 , read_term/3 , read_top_level/3 , second_prompt/2]).
:- use_module( library(sort) , [keylist/1 , keysort/2 , sort/2]).
:- use_module( library(system) , [cd/1 , chmod/2 , chmod/3 , current_executable/1 , current_host/1 , cyg2win/3 , datime/1 , datime/9 , datime_struct/1 , delete_directory/1 , delete_file/1 , directory_files/2 , exec/3 , exec/4 , extract_paths/2 , file_exists/1 , file_exists/2 , file_properties/6 , file_property/2 , fmode/2 , get_pid/1 , getenvstr/2 , make_directory/1 , make_directory/2 , make_dirpath/1 , make_dirpath/2 , mktemp/2 , modif_time/2 , modif_time0/2 , pause/1 , popen/3 , popen_mode/1 , rename_file/2 , setenvstr/2 , shell/0 , shell/1 , shell/2 , system/1 , system/2 , time/1 , umask/2 , working_directory/2]).
:- use_module( library(ttyout) , [ttydisplay/1 , ttydisplay_string/1 , ttydisplayq/1 , ttyflush/0 , ttyget/1 , ttyget1/1 , ttynl/0 , ttyput/1 , ttyskip/1 , ttyskipeol/0 , ttytab/1]).
:- use_module( library(write) , [numbervars/3 , portray_clause/1 , portray_clause/2 , prettyvars/1 , print/1 , print/2 , printable_char/1 , write/1 , write/2 , write_canonical/1 , write_canonical/2 , write_list1/1 , write_option/1 , write_term/2 , write_term/3 , writeq/1 , writeq/2]).

p(Value,Res) :-
        q_2(Tmp,Value),
        arithmetic:(Tmp2 is Tmp+3),
        q_1(Tmp2,Res).

q_1(A,B) :-
        other(A,B),
        plus1_1(A,B).

q_2(A,B) :-
        other(A,B),
        plus1_2(A,B).

other(A,B) :-
        write:write(A),
        write:write(B).

plus1_1(X,Y) :-
        arithmetic:(Y is X+1).

plus1_2(X,Y) :-
        ground(X),
        arithmetic:(Y is X+1).
plus1_2(X,Y) :-
        term_typing:var(X),
        arithmetic:(X is Y-1).



