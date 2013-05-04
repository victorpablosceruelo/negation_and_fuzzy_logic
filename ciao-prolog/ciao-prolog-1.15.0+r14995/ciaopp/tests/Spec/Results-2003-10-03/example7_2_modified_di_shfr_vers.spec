:- module( _example7_2_modified, [main/1], [assertions] ).

:- use_module( library( 'assertions/native_props.pl' ) ).

:- use_module( library(debugger) , [srcdbg_spy/6]).
:- use_module( library(sort) , [keylist/1 , keysort/2 , sort/2]).


:- check success tree(_216927).

:- check calls tree(_216927).

:- check success list_of_int_lists(_216927).

:- check calls list_of_int_lists(_216927).

:- check success int_list(_216927).

:- check calls int_list(_216927).


