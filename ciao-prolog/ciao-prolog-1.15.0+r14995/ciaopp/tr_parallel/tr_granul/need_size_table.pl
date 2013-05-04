:- module(need_size_table,
	[
	    actualize_nt/4, 
	    insert_needed_sizes/4, 
	    find_needed_sizes/3, 
	    find_size/3 
	], 
	[assertions]). 

:- doc(author,"Pedro L@'{o}pez").  

% own library
:- use_module(infercost(init), [find_symbol_entry/3]). 
:- use_module(g_utility, 
	[
	    open_union_changes/3, 
	    is_in_open_list/2 
	]).

% Actualize the names table: the field Insizes 
actualize_nt(NT, _ST, _NS, _NNT):-
	var(NT),
	!.
actualize_nt(NT, ST, NS, NNT):-
	nonvar(NT),
	!,
	NT = [Entry|RNT],
	Entry = st(Pred, Seq_Name, Gran_Name, _ArgList, Sizes, TimeFunc),
	find_needed_sizes(Pred, NS, NeedHeadSizes),
	create_new_arguments(ST, Pred, NeedHeadSizes, NewArgList),
	NewEntry = st(Pred, Seq_Name, Gran_Name, NewArgList, Sizes, TimeFunc),
	NNT = [NewEntry|NewNT],
	actualize_nt(RNT, ST, NS, NewNT).

create_new_arguments(ST, Pred, NeedHeadSizes, NewArgList):-
	find_symbol_entry(ST,Pred, 
	                  st(Pred,_,ModeList,MeaList,_,_,_,_,_,_)),
        get_needed_arguments(1, ModeList, MeaList,
                             NeedHeadSizes, NewArgList).


% Creates a list with the input arguments numbers needed by the
% versions that perform spawning tests or supply sizes. 
% Has the format [(e, 1), ..., (e, N), (h, 1), ..., (h, M)].
% where (e, i) represents that the size of the argument number i will be
% supplied by an extra argument of the annotated head.  (h, i) means
% that the size of the argument number i will be supplied by the
% argument number i itself, and no extra argument is needed for the
% annotated head (in that case, the measure of the argument number i is
% supposed to be int), which means that before calling a version of a
% predicate that performs granularity control only is neccessary to
% supply the sizes of arguments of type (e, i).

 
get_needed_arguments(_, [], [], _NeedHeadSizes, []):-
        !.
get_needed_arguments(ArgNum, [+|ModeList], [Mea|MeaList], 
                     NeedHeadSizes, [InputArg|AList]):-
        is_in_open_list(NeedHeadSizes, $(0, ArgNum)),
        !,
        NewArgNum is ArgNum + 1,
        (Mea == int -> InputArg = (h, ArgNum) ;  InputArg = (e, ArgNum)),
	get_needed_arguments(NewArgNum,ModeList,MeaList,NeedHeadSizes,AList).
get_needed_arguments(ArgNum,[_|ModeList],[_|MeaList],NeedHeadSizes,AList):-
	NewArgNum is ArgNum + 1,
	get_needed_arguments(NewArgNum,ModeList,MeaList,NeedHeadSizes,AList).

insert_needed_sizes(Pred, NS, NeHeadSizes2, Changes):-
    insert_sizes_entry(NS, Pred, st(Pred, NeededSizes)),
    open_union_changes(NeHeadSizes2, NeededSizes, Changes1),
    (
	(nonvar(NeHeadSizes2), Changes1 == true) -> Changes = true
    ;
	true
    ).

find_needed_sizes(Pred, NS, NeededSizes):-
	find_sizes_entry(NS, Pred, st(Pred, NeededSizes)). 

%
%  Insert an entry for predicate Pred in the sizes table.
%  If it is already in, return the entry as Entry;
%  otherwise, an entry Entry for Pred is inserted and returned.
%

insert_sizes_entry(ST, Pred, Entry) :- 
	var(ST),
        !,
	Entry = st(Pred, _),
	ST = [Entry|_].
insert_sizes_entry(ST, Pred, Entry) :- 
	nonvar(ST),
	ST = [Entry1|_],
        Entry1 = st(Pred1, _),
	Pred1 == Pred, !,
        Entry = Entry1.
insert_sizes_entry(ST, Pred, Entry) :- 
	nonvar(ST),
	ST = [Entry1|RST],
        Entry1 = st(Pred1, _),
	Pred1 \== Pred, 
	insert_sizes_entry(RST, Pred, Entry).

find_sizes_entry(ST, _Pred, _Entry) :- 
	var(ST),
        !.
find_sizes_entry(ST, Pred, Entry) :- 
	nonvar(ST),
	ST = [Entry1|_],
        Entry1 = st(Pred1, _),
	Pred1 == Pred, !,
        Entry = Entry1.
find_sizes_entry(ST, Pred, Entry) :- 
	nonvar(ST),
	ST = [Entry1|RST],
        Entry1 = st(Pred1, _),
	Pred1 \== Pred, 
	find_sizes_entry(RST, Pred, Entry).

find_size([], _, _).
find_size([iasize(Lit, Size)|_], LitS, Size):-
	LitS == Lit,
        !. 
find_size([iasize(Lit, _)|SizeList], LitS, Size):- 
	LitS \== Lit,
	find_size(SizeList, LitS, Size).


