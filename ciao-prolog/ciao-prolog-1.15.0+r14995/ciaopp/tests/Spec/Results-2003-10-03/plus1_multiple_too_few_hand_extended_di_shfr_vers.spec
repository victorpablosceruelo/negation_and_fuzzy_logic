:- module( _plus1_multiple_too_few_hand_extended, [p/2], [assertions] ).

:- use_module( library( 'assertions/native_props.pl' ) ).

:- use_module( library(aggregates) , [(^)/2 , bagof/3 , findall/3 , findall/4 , findnsols/4 , findnsols/5 , setof/3]).
:- use_module( library(debugger) , [srcdbg_spy/6]).
:- use_module( library(dynamic) , [abolish/1 , assert/1 , assert/2 , asserta/1 , asserta/2 , assertz/1 , assertz/2 , clause/2 , clause/3 , retract/1 , retractall/1 , wellformed_body/3]).
:- use_module( library(iso_byte_char) , [atom_chars/2 , char_code/2 , get_byte/1 , get_byte/2 , get_char/1 , get_char/2 , number_chars/2 , peek_byte/1 , peek_byte/2 , peek_char/1 , peek_char/2 , put_byte/1 , put_byte/2 , put_char/1 , put_char/2]).
:- use_module( library(iso_incomplete) , [close/2 , stream_property/2]).
:- use_module( library(iso_misc) , [\= /2 , compound/1 , once/1 , sub_atom/5 , unify_with_occurs_check/2]).
:- use_module( library(operators) , [current_infixop/4 , current_op/3 , current_postfixop/3 , current_prefixop/3 , op/3]).
:- use_module( library(read) , [read/1 , read/2 , read_term/2 , read_term/3 , read_top_level/3 , second_prompt/2]).
:- use_module( library(write) , [numbervars/3 , portray_clause/1 , portray_clause/2 , prettyvars/1 , print/1 , print/2 , printable_char/1 , write/1 , write/2 , write_canonical/1 , write_canonical/2 , write_list1/1 , write_option/1 , write_term/2 , write_term/3 , writeq/1 , writeq/2]).



