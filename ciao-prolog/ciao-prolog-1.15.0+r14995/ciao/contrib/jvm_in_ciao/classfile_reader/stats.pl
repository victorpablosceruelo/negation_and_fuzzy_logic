:- module(stats,[class_num_insts/2,class_num_methods/2,class_num_divs/2,class_num_convs/2,
        set_class_num_methods/2,inc_class_num_insts/1,inc_class_num_divs/1,inc_class_num_convs/1,
	method_num_insts/2,method_num_divs/2,method_num_convs/2,
        inc_method_num_insts/1,inc_method_num_divs/1,inc_method_num_convs/1,
	init_class_stats/1,init_method_stats/1,cleanup_stats/0,cleanup_class_stats/0,cleanup_method_stats/0]).

:- data class_num_insts/2.
:- data class_num_methods/2.
:- data class_num_divs/2.
:- data class_num_convs/2.
:- data method_num_insts/2.
:- data method_num_divs/2.
:- data method_num_convs/2.


init_class_stats(Class) :-
	cleanup_class_stats,
	assertz_fact(class_num_insts(Class,0)),
	assertz_fact(class_num_methods(Class,0)),
	assertz_fact(class_num_divs(Class,0)),
	assertz_fact(class_num_convs(Class,0)).

init_method_stats(Method) :- 
	assertz_fact(method_num_insts(Method,0)),
	assertz_fact(method_num_divs(Method,0)),
	assertz_fact(method_num_convs(Method,0)).

set_class_num_methods(Class,N) :-
	retractall_fact(class_num_methods(Class,_)),
	assertz_fact(class_num_methods(Class,N)).

inc_class_num_insts(Class) :-
	retract_fact(class_num_insts(Class,N)),!,
	N_p is N + 1,
	assertz_fact(class_num_insts(Class,N_p)).
inc_class_num_insts(Class) :-
	assertz_fact(class_num_insts(Class,1)).

inc_class_num_divs(Class) :-
	retract_fact(class_num_divs(Class,N)),!,
	N_p is N + 1,
	assertz_fact(class_num_divs(Class,N_p)).
inc_class_num_divs(Class) :-
	assertz_fact(class_num_divs(Class,1)).

inc_class_num_convs(Class) :-
	retract_fact(class_num_convs(Class,N)),!,
	N_p is N + 1,
	assertz_fact(class_num_convs(Class,N_p)).
inc_class_num_convs(Class) :-
	assertz_fact(class_num_convs(Class,1)).

cleanup_stats :- 
	cleanup_method_stats,
	cleanup_class_stats.

cleanup_class_stats :- 
	retractall_fact(class_num_insts(_,_)),
	retractall_fact(class_num_methods(_,_)),
	retractall_fact(class_num_divs(_,_)),
	retractall_fact(class_num_convs(_,_)).

cleanup_method_stats :- 
	retractall_fact(method_num_insts(_,_)),
	retractall_fact(method_num_divs(_,_)),
	retractall_fact(method_num_convs(_,_)).


inc_method_num_insts(Method) :-
	retract_fact(method_num_insts(Method,N)),!,
	N_p is N + 1,
	assertz_fact(method_num_insts(Method,N_p)).
inc_method_num_insts(Method) :-
	assertz_fact(method_num_insts(Method,1)).

inc_method_num_divs(Method) :-
	retract_fact(method_num_divs(Method,N)),!,
	N_p is N + 1,
	assertz_fact(method_num_divs(Method,N_p)).
inc_method_num_divs(Method) :-
	assertz_fact(method_num_divs(Method,1)).

inc_method_num_convs(Method) :-
	retract_fact(method_num_convs(Method,N)),!,
	N_p is N + 1,
	assertz_fact(method_num_convs(Method,N_p)).
inc_method_num_convs(Method) :-
	assertz_fact(method_num_convs(Method,1)).
