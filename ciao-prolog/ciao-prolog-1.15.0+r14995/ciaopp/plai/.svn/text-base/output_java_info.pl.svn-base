:- module(output_java_info,[java_statistics/1]).

:- use_module(plai(plai_db), [memo_table/6]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(sort), [sort/2]).
:- use_module(library(messages), [simple_message/2]).

java_domain(java_types):-!.
java_domain(java_null):-!.
java_domain(java_pairsh):-!.
		       
java_statistics(AbsInt):-
	java_domain(AbsInt),!,
	get_ppoints(PPs_u,AbsInt),
	sort(PPs_u,PPs_s),
	length(PPs_s,NPPs),
	process_ppoints(PPs_u,[],Ss),
	number_contexts(Ss,0,NConts),
	print_results(NPPs,NConts).
java_statistics(_):-!.

get_ppoints(Ss,AbsInt):-
	findall(PPKey,memo_table(PPKey,AbsInt,_,_,_,_),Ss).

process_ppoints([],Acc,Acc).
process_ppoints([X|Xs],Acc,Zs):-
	mymember(X,Acc,(_,N),Res),!,
	N1 is N + 1,
	process_ppoints(Xs,[(X,N1)|Res],Zs).
process_ppoints([X|Xs],Acc,Zs):-
	!,
	process_ppoints(Xs,[(X,1)|Acc],Zs).

mymember(X,[(Y,N)|Ys],(Y,N),Ys):-
	X == Y,!.
mymember(X,[(Y,N)|Ys],(Z,NZ),[(Y,N)|Zs]):-
	!,
	mymember(X,Ys,(Z,NZ),Zs).
	
number_contexts([],Acc,Acc).
number_contexts([(_,N)|Xs],Acc,NAcc1):-
	NAcc is Acc + N,
	number_contexts(Xs,NAcc,NAcc1).

print_results(PP,ASt):-
	St is ASt / PP,
	simple_message("pp: ~q ast:~q st:~q",[PP,ASt,St]).