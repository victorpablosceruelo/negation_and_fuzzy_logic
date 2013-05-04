

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% witt.pl -- the whole WITT clustering system. To start, simply
 %% load this file and call witt/0. It will take some seconds to complete.
 %% Universe to be clustered appended in this file.
 %% AFSID           : $__Header$
 %% Author          : Manuel Carro Li~nares
 %% Created On      : At some point in the year 92/93
 %% Last Modified By: Manuel Carro Li~nares
 %% Last Modified On: Wed Nov  4 17:17:52 1992
 %% Update Count    : 3
 %% Status          : Correct and working (!)

:- module(witt, [witt/0],[]).
:- use_module(library(write)).
:- use_module(library(aggregates)).


:- push_prolog_flag(multi_arity_warnings,off).

:- use_module(library(lists), 
	[ length/2 ]).


:- use_module(library(sort)).

universe(U):-
	bagof(A, attribute(A, _), Ats),
	length(Ats, N),
	Z is N *(N - 1) / 2,
	findall(Instance, instance(Instance, Ats, Z), U).

precluster(World, NewWorld):-
	smallest_dist(World, O1, O2, D),
	factor(F),
	T1 is F*D,
	compare(C, D, T1),
	precluster(C, World, O1, O2, T1, NewWorld).

precluster(>, W, _, _, _, W).
precluster(=, W, _, _, _, W).
precluster(<, W, O1, O2, T1, Nw):-
	select(O1, W, W1),
	select(O2, W1, W2),
	precluster(W2, O1, O2, T1, Nw).
precluster([], O1, O2, _, [O1, O2]).
precluster([W|Ws], O1, O2, T1, Nw):-
	combine(O1, O2, Category),
	UpW = [Category, W|Ws],
	smallest_dist(UpW, Ob1, Ob2, D),
	compare(C, D, T1),
	precluster(C, UpW, Ob1, Ob2, T1, Nw).


smallest_dist(World, O1, O2, Best):-
	World = [Ob1, Ob2|_],
	distance(Ob1, Ob2, D),
	smallest_dist(World, Ob1, Ob2, D, O1, O2, Best).
smallest_dist([], O1, O2, D, O1, O2, D).
smallest_dist([W|Ws], Ob1, Ob2, D, O1, O2, Best):-
	smallest_dist(Ws, W, Ob1, Ob2, D, Oi1, Oi2, Di),
	smallest_dist(Ws, Oi1, Oi2, Di, O1, O2, Best).
smallest_dist([], _, O1, O2, Best, O1, O2, Best).
smallest_dist([Ob|Obs], Object, Ob1, Ob2, D, O1, O2, Best):-
	distance(Ob, Object, Di),
	compare(C, D, Di),
	select_tri(C,  Ob, Object, Di, Ob1, Ob2, D,NOb1, NOb2, Nd),
	smallest_dist(Obs, Object, NOb1, NOb2, Nd, O1, O2, Best).


refinement(Ins, Cats, Classes):-
	threshold2(T2),
	threshold3(T3),
	refinement(Ins, Cats, T2, T3, Classes).
refinement([], Cats, T2, T3, Classes):-
	compute_within(Cats, [], T2, T3, Classes).
refinement([I|Is], Cats, T2, T3, Classes):-
	classify_pairs([I|Is], Cats, T2, T3, Classes).

classify_pairs(Ins, Cats, T2, T3, Classes):-
	get_best_pair(Ins, Cats, In, Cat, Score),
	compare(C, Score, T2),
	check_add(C, Ins, Cats, In, Cat, T2, T3, Classes).

check_add(>, Ins, Cats, I, Cat, T2, T3, [Partition|Classes]):-
	NewWorld = [NewCat|NewCats],
	combine(I, Cat, NewCat),
	select(I, Ins, NewIns),
	select(Cat, Cats, NewCats),
	select_names(pair(NewIns, NewWorld), Partition),
	refinement(NewIns, NewWorld, T2, T3, Classes).
check_add(=, Ins, Cats, _, _, T2, T3, Classes):-
	new_categories(Ins, Cats, T2, T3, Classes).
check_add(<, Ins, Cats, _, _, T2, T3, Classes):-
	new_categories(Ins, Cats, T2, T3, Classes).


new_categories([], Cats, T2, T3, Classes):-            %% No hay ejemplos
	compute_within(Cats, [], T2, T3, Classes).
new_categories([I|Ins], Cats, T2, T3, Classes):-       %% Extraemos uno
	new_categories(Ins, I, Cats, T2, T3, Classes).
new_categories([], I, Cats, T2, T3, Classes):-         %% Solo habia un ejemplo
	compute_within(Cats, [I], T2, T3, Classes).
new_categories([I1|Is], I, Cats, T2, T3, Classes):-    %% Dos o mas
	Ins = [I, I1|Is],
	precluster(Ins, NewIns),
	split(NewIns, _, NewCats),
	find_addable(NewCats, Cats, T3, Addable),
	extract_instances(Ins, Addable, TrueIns),
	add_cats(Addable, TrueIns,  Cats, T2, T3, Classes).

add_cats([], Ins, Cats, T2, T3, Classes):-
	compute_within(Ins, Cats, T2, T3, Classes).
add_cats([C|Cs], Ins, Cats, T2, T3, [Partition|Classes]):-
	append(Cs, [C|Cats], NewCats),
	append(Ins, NewCats, Part),
	select_names(Part, Partition),
	refinement(Ins, NewCats, T2, T3, Classes).

compute_within([C|Cs], Ins, T2, T3, Classes):-
	compute_within(Cs, C, Ins, T2, T3, Classes).
compute_within([], C, Ins, _, _, [Classes]):-        %% Solo una categoria
	select_names([C|Ins], Classes).
compute_within([Ca1|Cats], Ca2, Ins, T2, T3, Classes):-
	best_cohesion(Ca1, Ca2, Cats, C1, C2, Score),
	compare(C, Score, T3),
	check_end(C, Ins, [Ca1, Ca2|Cats], C1, C2, T2, T3, Classes).


check_end(>, Ins, Cats, C1, C2, T2, T3, [Partition|Classes]):-
 	NewWorld = [C3|Cats2],
	combine(C1, C2, C3),
	select(C1, Cats, Cats1),
	select(C2, Cats1, Cats2),
	select_names(pair(Ins, NewWorld), Partition),
	refinement(Ins, NewWorld, T2, T3, Classes).
check_end(<, Ins, Cats, _, _, _, _, [Classes]):-
	select_names(pair(Ins,  Cats), Classes).
check_end(=, Ins, Cats, _, _, _, _, [Classes]):-
	select_names(pair(Ins,  Cats), Classes).


attribute(epoca, [antigua, media, actual, futura, nula]).
attribute(tema, [ciencia, vida, aventuras, amor, guerra, poesia, religion,
	muerte, ficcion, sexo]).
attribute(autor, [anonimo, antiguo, moderno, medio]).
attribute(publico, [joven, adulto, culto, no_culto]).

example(jazaro, [epoca-[antigua], tema-[ficcion], autor-[moderno],
	publico-[culto]]). 
example(sodoma, [epoca-[actual], tema-[sexo], autor-[moderno],
	publico-[adulto]]).
example(biblia, [epoca-[antigua], tema-[religion], autor-[anonimo, antiguo],
	publico-[joven, adulto]]). 
example(quijote, [epoca-[media], tema-[aventuras, vida, ficcion],
	autor-[medio], publico-[joven, adulto]]).
example(perfume, [epoca-[actual], tema-[ficcion], autor-[moderno],
	publico-[adulto, culto]]).
example(jazmin, [epoca-[actual, media], tema-[amor], autor-[moderno],
	publico-[no_culto]]).
example(dragon, [epoca-[nula], tema-[ciencia], autor-[moderno],
	publico-[culto]]).
example(flores_mal, [epoca-[actual], tema-[amor, muerte, vida],
	autor-[moderno], publico-[adulto, culto]]).
example(maldoror, [epoca-[actual], tema-[vida, muerte], autor-[moderno],
	publico-[adulto, culto]]).
example(fundacion, [epoca-[futura], tema-[ciencia, ficcion], autor-[moderno],
	publico-[joven, adulto]]).
example(neuromante, [epoca-[futura], tema-[ciencia, ficcion], autor-[moderno],
	publico-[joven, adulto]]).
example(lolita, [epoca-[actual], tema-[sexo, amor], autor-[moderno],
	publico-[adulto, culto]]).
example(godel, [epoca-[nula], tema-[ciencia], autor-[moderno],
	publico-[joven, culto]]).
example(nicomaco, [epoca-[nula], tema-[ciencia], autor-[antiguo],
	publico-[adulto, culto]]).

factor(1.2).
threshold2(0.15).
threshold3(1.0).


%   ord_union(+Set1, +Set2, ?Union)
%   is true when Union is the union of Set1 and Set2.  Note that when
%   something occurs in both sets, we want to retain only one copy.

ord_union([], Set2, Set2) :- !.
ord_union(Set1, [], Set1) :- !.
ord_union([Head1|Tail1], [Head2|Tail2], Union) :-
	compare(Order, Head1, Head2),
	ord_union(Order, Head1, Tail1, Head2, Tail2, Union).

ord_union(<, Head0, [], Head2, Tail2, [Head0,Head2|Tail2]) :- !.
ord_union(<, Head0, [Head1|Tail1], Head2, Tail2, [Head0|Union]) :-
	compare(Order, Head1, Head2),
	ord_union(Order, Head1, Tail1, Head2, Tail2, Union).
ord_union(=, Head,  Tail1, _,	  Tail2, [Head|Union]) :-
	ord_union(Tail1, Tail2, Union).
ord_union(>, Head1, Tail1, Head0, [], [Head0,Head1|Tail1]) :- !.
ord_union(>, Head1, Tail1, Head0, [Head2|Tail2], [Head0|Union]) :-
	compare(Order, Head1, Head2),
	ord_union(Order, Head1, Tail1, Head2, Tail2, Union).

%   ord_intersection(+Set1, +Set2, ?Intersection)
%   is true when Intersection is the ordered representation of Set1
%   and Set2, provided that Set1 and Set2 are ordered sets.

ord_intersection([], _, []) :- !.
ord_intersection(_, [], []) :- !.
ord_intersection([Head1|Tail1], [Head2|Tail2], Intersection) :-
	compare(Order, Head1, Head2),
	ord_intersection(Order, Head1, Tail1, Head2, Tail2, Intersection).

ord_intersection(<, _, [], _, _, []) :- !.
ord_intersection(<, _, [Head1|Tail1], Head2, Tail2, Intersection) :-
	compare(Order, Head1, Head2),
	ord_intersection(Order, Head1, Tail1, Head2, Tail2, Intersection).
ord_intersection(=, Head, Tail1, _, Tail2, [Head|Intersection]) :-
	ord_intersection(Tail1, Tail2, Intersection).
ord_intersection(>, _, _, _, [], []) :- !.
ord_intersection(>, Head1, Tail1, _, [Head2|Tail2], Intersection) :-
	compare(Order, Head1, Head2),
	ord_intersection(Order, Head1, Tail1, Head2, Tail2, Intersection).

%   list_to_ord_set(+List, ?Set)
%   is true when Set is the ordered representation of the set represented
%   by the unordered representation List.  

list_to_ord_set(List, Set) :-
	sort(List, Set).

%   select(?Element, ?List, ?List2)
%   is true when the result of removing an occurrence of Element in List
%   is List2.

select(Element, [Element|Tail], Tail).
select(Element, [Head|Tail1], [Head|Tail2]) :- 
	select(Element, Tail1, Tail2).

%   mymember(?Element, +List)
%   is true when Element is a member of List.  It may be used to test 
%   for membership in a list, but it can also be used to enumerate all 
%   the elements in List.

mymember(Element, [Head|Tail]) :-
	member_(Tail, Head, Element).

% auxiliary to avoid choicepoint for last element
member_(_, Element, Element).
member_([Head|Tail], _, Element) :-
	member_(Tail, Head, Element).

%   memberchk(+Element, +List)
%   is true when Element is a member of List, but memberchk/2 only succeeds
%   once and can therefore not be used to enumerate the elements in List.

memberchk(Element, [Element|_]) :- !.
memberchk(Element, [_|Rest]) :-
	memberchk(Element, Rest).

%   append(?Prefix, ?Suffix, ?Combined)
%   is true when Combined is the combined list of the elements in Prefix 
%   followed by the elements in Suffix. It can be used to form Combined or
%   it can be used to find Prefix and/or Suffix from a given Combined  

append([], List, List).
append([Head|Tail], List, [Head|Rest]) :- 
	append(Tail, List, Rest).

%   non_member(+Element, +List)
%   non_member is true when Element does not exist in List.

non_member(Element, List) :-
	non_member_(List, Element).

non_member_([], _).
non_member_([Head|Tail], Element) :-
%% 	dif(Head, Element),
	\+(Head = Element),
	non_member_(Tail, Element).
  %% natural logarithm, which treats the special case of a zero as
  %% argument. For our convenience, 0 will be returned.

mylog(Number, 0):- Number =< 0.0, !.
mylog(Number, Log):- Log is log(Number). 


dij(M, Dij):-
	dij(M, 0, Num, 0, D),
	mylog(D, L),
	Den is L*D,
	divide(Num, Den, Dij).

divide(X, X, 1).
divide(X, Y, R):-
	X =\= Y,
	R is X / Y.

dij([], X, X, Y, Y):- !.          %% Green
dij([M|Ms], Xi, Xo, Yi, Yo):- !,  %% Green
	dij(M, Xi, Xm, Yi, Ym),
	dij(Ms, Xm, Xo, Ym, Yo).
dij(A, Xi, Xo, Yi, Yo):-
	number(A), !,              %% Green
	Yo is Yi + A,
	mylog(A, L),
	Xo is Xi + A * L.


wc(ml(_, _, Z, Conts), WC):-
	wc(Conts, 0, Out),
	WC is Out / Z.
wc([], A, A).
wc([mat(D, _, _, _)|Ms], I, O):-
	I1 is I + D,
	wc(Ms, I1, O).

distance(ml(_, Ats1, _, _), ml(_, Ats2, _, _), D):-
	distance(Ats1, Ats2, 1, D1),
	D is 1 / D1.
distance([], [], D, D).
distance([A-V1s|A1s], [A-V2s|A2s], In, Out):-
	ord_intersection(V1s, V2s, Vs),
	length(Vs, L),
	Mid is In + L,
	distance(A1s, A2s, Mid, Out).

/*
distance(O1, O2, D1):-
	combine(O1, O2, O3),
	wc(O3, D),
	D1 is 1 / D.
*/
bck(C1, C2, B):-
	combine(C1, C2, C12),
	wc(C1, W1),
	wc(C2, W2),
	wc(C12, W12),
	B is 1 / (W1 + W2 - 2*W12).


oc([], _, N, In, Out):-
	Out is In / N.
oc([C|Cs], Cat, N, In, Out):-
	N1 is N + 1,
	bck(C, Cat, Bck),
	Mid is In + Bck,
	oc(Cs, Cat, N1, Mid, Out).


cc(Cat, Cats, C):-
	wc(Cat, Wc),
	oc(Cats, Cat, 0, 0, Oc),
	C is Wc / Oc.

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% ml.pl -- main file fot the WITT clustering system. To start, simply
 %% load this file and call witt/0. It will take some seconds to complete.
 %% Universe to be clustered in examples.pl.
 %% AFSID           : $__Header$
 %% Author          : Manuel Carro Li~nares
 %% Created On      : At some point in the year 92/93
 %% Last Modified By: Manuel Carro Li~nares
 %% Last Modified On: Wed Nov  4 17:17:52 1992
 %% Update Count    : 3
 %% Status          : Correct and working

 %% HISTORY 
 %% 4-Nov-1992		Manuel Carro Li~nares	
 %%    Last Modified: Wed Nov  4 17:16:44 1992 #2 (Manuel Carro Li~nares)
 %%    Adapted for sicstus 2.1, natural logarithm now as a arithmetic
 %%    builtin, no need of external C file.

%foreign_file('lnp.o', [lnp]).
%foreign(lnp, c, lnp(+float, [-float])).

% :- load_foreign_files(['lnp.o'], ['-lm']).

witt:-
	Taxo = [Initial, First_Partition|Classes],
	universe(World),
	select_names(World, Initial),
	precluster(World, First_step),
	select_names(First_step, First_Partition),
	split(First_step, Instances, Categories),
	refinement(Instances, Categories, Classes),
	write_taxo(Taxo, 1).

write_taxo([_], _).
write_taxo([T, T1|Ts], N):-
	write(nivel(N)), nl,
	write_clas(T),
	N1 is N + 1,
	write_taxo([T1|Ts], N1).
write_clas([T]):- write(T), nl, nl.
write_clas([T, T1|Ts]):-
	write(T),
	write(', '),
	write_clas([T1|Ts]).


select_names(O, N):-
	select_names(O, [], N).
select_names(ml(N, _, _, _), I, [N|I]).
select_names(pair(O1, O2), I, N):-
	select_names(O1, I, M),
	select_names(O2, M, N).
select_names([], N, N).
select_names([ml(N, _, _, _)|Os], A, Ns):-
	select_names(Os, [N|A], Ns).

combine(ml(Nm1, Cars1, Z, M1), ml(Nm2, Cars2, Z, M2), ml(Nm3, Cars3, Z, M3)):-
	append(Nm1, Nm2, Nm3),
	combine_cars(Cars1, Cars2, Cars3),
	combine_conts(M1, M2, M3).

combine_cars([], [], []).
combine_cars([A-V1s|C1s], [A-V2s|C2s], [A-V3s|C3s]):-
	ord_union(V1s, V2s, V3s),
	combine_cars(C1s, C2s, C3s).

combine_conts([], [], []).
combine_conts([mat(_, Ai, Aj, M1)|M1s], [mat(_, Ai, Aj, M2)|M2s], [mat(D, Ai,
	Aj,M3)|M3s]):-
	add_matrices(M1, M2, M3),
	dij(M3, D),
	combine_conts(M1s, M2s, M3s).

add_matrices([], [], []).
add_matrices([M1|M1s], [M2|M2s], [M3|M3s]):-
	add_matrices(M1, M2, M3),
	add_matrices(M1s, M2s, M3s).
add_matrices(E1, E2, E3):-
	number(E1),
	E3 is E1 + E2.

extract_instances(Ins, Addable, True):-
	select_names(Addable, AdNames),
	plain(AdNames, [], Plain),
	extract_instances_1(Ins, Plain, True).
extract_instances_1([], _, []):- !.
extract_instances_1([I|Ins], NCats, TIns):-
	I = ml([N], _, _, _),
	mymember(N, NCats),!,
	extract_instances_1(Ins, NCats, TIns).
extract_instances_1([I|Ins], NCats, [I|TIns]):-
	I = ml([N], _, _, _),
	non_member(N, NCats),!,
	extract_instances_1(Ins, NCats, TIns).

plain([], P, P):- !.
plain([A|As], I, O):- !,
	plain(A, I, M),
	plain(As, M, O).
plain(A, I, [A|I]):-
	atomic(A), !.

split([], [], []).
split([X|Xs], [X|Ys], Zs):-
	instance(X),
	split(Xs, Ys, Zs).
split([X|Xs], Ys, [X|Zs]):-
	category(X),
	split(Xs, Ys, Zs).

instance(ml([_], _, _, _)).
category(ml([_,_|_], _, _, _)).

get_best_pair([In|Ins], [Cat|Cts], BestI, BestC, BestSc):-
	combine(In, Cat, Prv),
	cc(Prv, Cts, Sco), 
	get_best_pair([Cat|Cts],[In|Ins],[],Cat,In,Sco,BestC,BestI,BestSc).

get_best_pair([], _, _, C, I, S, C, I, S). 
get_best_pair([C|Cs], Ins, PCts, PrCat, PrIns, PrSco, BCat, BIns, BSco):-
	append(Cs, PCts, NCts),
	get_best_pair_ins(Ins,C,NCts,PrCat,PrIns,PrSco,MdCat,MdIns,MdSco),
	get_best_pair(Cs,Ins,[C|PCts],MdCat,MdIns,MdSco,BCat,BIns,BSco).

get_best_pair_ins([], _, _, C, I, S, C, I, S).
get_best_pair_ins([In|Ins], CCat, Cts, PrCat, PrIns, PrSco, BCat, BIns, BSco):-
	combine(In, CCat, NCat),
	cc(NCat, Cts, Sco),
	compare(C, Sco, PrSco),
	select_tri(C, CCat, In, Sco, PrCat, PrIns, PrSco, MC, MI, MS),
	get_best_pair_ins(Ins, CCat, Cts, MC, MI, MS, BCat, BIns, BSco).


find_addable(NewCats, Cats, T3, Addable):-
	find_addable(NewCats, Cats, T3, [], Addable).

find_addable([], _, _, A, A).
find_addable([Nc|Ncs], Cats, T3, InA, OutA):-
	is_addable(Cats, Nc, T3, Verdict),
	verdict(Verdict, Nc, InA, MidA),
	find_addable(Ncs, Cats, T3, MidA, OutA).

verdict(yes, Nc, InA, [Nc|InA]).
verdict(no, _, InA, InA).

is_addable([], _, _, yes).
is_addable([C1|Cs], Nc, T3, V):-
	combine(C1, Nc, C2),
	wc(C2, W),
	compare(C, W, T3),
	is_addable(C, Cs, Nc, T3, V).
is_addable(<, Cats, Cat, T3, V):-
	is_addable(Cats, Cat, T3, V).
is_addable(>, _, _, _, no).
is_addable(=, _, _, _, no).

best_cohesion(Cat1, Cat2, Cats, C1, C2, Score):-
	combine(Cat1, Cat2, Cat3),
	wc(Cat3, W3),
	best_cohesion([Cat2|Cats], Cat1, Cat1, Cat2, W3, C1, C2, Score).
best_cohesion([], _, C1, C2, S, C1, C2, S).
best_cohesion([Cat|Cats], C, PCat1, PCat2, PSco, BCat1, BCat2, BSco):-
	combine(Cat, C, NCat),
	wc(NCat, NSco),
	compare(Comp, NSco, PSco),
	select_tri(Comp, Cat, C, NSco, PCat1, PCat2, PSco, MCat1, MCat2, MSco),
	best_cohesion(Cats, C, MCat1, MCat2, MSco, QCat1, QCat2, QSco),
	best_cohesion(Cats, Cat, QCat1, QCat2, QSco, BCat1, BCat2, BSco).

select_tri(<, _, _, _, A, B, C, A, B, C).
select_tri(=, _, _, _, A, B, C, A, B, C).
select_tri(>, A, B, C, _, _, _, A, B, C).

instance(ml([Name], OrdCars, Z, Mats), Ats, Z):-
	example(Name, Cars),
	intra_order(Cars, IntraCars),
	list_to_ord_set(IntraCars, OrdCars),
	findall(mat(D, Ai, Aj, M), cont_table(D, Ai, Aj, M, Ats, Cars), Mats).

intra_order([], []).
intra_order([A-Vs|Cs], [A-Ovs|Os]):-
	list_to_ord_set(Vs, Ovs),
	intra_order(Cs, Os).

cont_table(D, Ai, Aj, M, Ats, Cars):-
	append(_, [Ai|Ats1], Ats), attribute(Ai, Vali),
	mymember(Aj, Ats1), attribute(Aj, Valj),
	mymember(Ai-Vi, Cars),
	mymember(Aj-Vj, Cars),
	fill_matrix(Vali, Valj, Vi, Vj, M),
	dij(M, D).

fill_matrix([], _, _, _, []).
fill_matrix([Vali|Valis], Valjs, Presi, Presj, [V|Vs]):-
	fill_vector(Valjs, Vali, Presi, Presj, V),
	fill_matrix(Valis, Valjs, Presi, Presj, Vs).

fill_vector([], _, _, _, []).
fill_vector( [Valj|Valjs], Vali, Presi, Presj, [E|Es]):-
	double_member_check(Vali, Valj, Presi, Presj, E),
	fill_vector(Valjs, Vali, Presi, Presj, Es).

double_member_check(E1, E2, L1, L2, 1):-
	memberchk(E1, L1),
	memberchk(E2, L2), !.
double_member_check(_, _, _, _, 0).

:- pop_prolog_flag(multi_arity_warnings).
