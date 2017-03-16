% TODO: Add export list!
:- module(_, _, [argnames, api(ciaopp_api)]).

:- use_module(library(vndict)).
:- use_module(library(terms)).

:- use_module(library(lists), [append/3]).

:- use_module(ciaopp(api(api_base)), [append_if_not_member/3]).

% :- op(160,xfx,'$~').
% :- function('$~'/2).

remove_disj_and_cond_cl(A, L, LT, Names) :- 
	% should be process_clauses
        clean_clauses(A, AC),
        remove_from_cl(AC, [], _, L, LT, Names).
%	display(Names),nl.


clean_clauses([C|Cs], [NC|NCs]) :-
	!,
	clean_clauses(C, NC),
	clean_clauses(Cs, NCs).
clean_clauses([], []) :-
	!.
clean_clauses(Cl, NCl) :-
	Cl = cls${ ref => Ref, id => ID, key => Key, 
	           head => H, body => B, dic => D   ,
		   locator => Loc },
	!,
	% should be process_literals(Cl, remove_ciao_metacalls, NCL),
        clean_literals(B, NewBody),
	NCl = cls${ ref => Ref, id => ID, key => Key, 
	            head => H, body => NewBody, dic => D,
		    locator => Loc }.



% this have to be called remove ciao_metacalls
clean_literals(A, A) :-
	var(A),
	!.
clean_literals($(A,_,_), A) :-
	!.
clean_literals([A|As], [NA|NAs]) :-
	clean_literals(A, NA),
	clean_literals(As, NAs).
clean_literals([], []) :-
	!.
clean_literals(A, NA) :-
	A =.. [F|Args],
	clean_literals(Args, NArgs),
	NA =.. [F|NArgs].

% clean_literals((A,B), (NA,NB)) :-
% 	!,
% 	clean_literals(A, NA),
% 	clean_literals(B, NB).
% clean_literals((A;B), (NA;NB)) :-
% 	!,
% 	clean_literals(A, NA),
% 	clean_literals(B, NB).
% clean_literals((A->B;C), (NA->NB;NC)) :-
% 	clean_literals(A, NA),
% 	clean_literals(B, NB),
% 	clean_literals(C, NC).
% clean_literals((A->B;C), (NA->NB;NC)) :-

remove_from_cl(A, N, N, L, L, []) :- 
     ( var(A) ; A == [] ),
     !.
remove_from_cl([Cl|Cls], N, NO, [MCl|L], LT, Na) :-
     !,
     remove_from_cl(Cl, N, N1, [MCl|New], Cls, Na1),
     remove_from_cl(New, N1, NO, L, LT, Na2),
     append_if_not_member(Na1, Na2, Na).
remove_from_cl(Cl, N, NO, [u(NCl)|L], LT, Na) :-
     Cl = cls${ ref => Ref, id => ID, key => Key, 
                head => H, body => B, dic => D,
		locator => Loc },
     !,
     remove_from_body(B, Cl, NewBody, N, NO, L, LT, Na),
     NCl = cls${ ref => Ref, id => ID, key => Key, 
                 head => H, body => NewBody, dic => D   ,
		 locator => Loc }.
remove_from_cl(a(Cl), N, NO, [a(Cl)|NCl], LT, Na) :-
	!,
	remove_from_cl(Cl, N, NO, NCl, LT, Na).
remove_from_cl(A, N, N, [A|L], L, []).





remove_from_body(L:_PPI, Cl, NewLit, N, NO, Cls, ClsT, Na) :-
	!,
	remove_from_body(L, Cl, NewLit, N, NO, Cls, ClsT, Na).
% remove_from_body('$'(L,_,_), Cl, NewLit, N, NO, Cls, ClsT) :-
% 	!,
% 	remove_from_body(L, Cl, NewLit, N, NO, Cls, ClsT).
remove_from_body((IF->THEN;ELSE), Cl, NewLit, N, NO, Cls, ClsT, [Name]) :-
	!,
	remove_disj(((IF,!,THEN) ; ELSE), Cl, cond, 
	             NewLit, N, NO, Cls, ClsT, Name).
remove_from_body((IF->THEN), Cl, NewLit, N, NO, Cls, ClsT, [Name]) :-
	!,
	remove_disj(((IF,!,THEN) ; fail), Cl, cond, 
	             NewLit, N, NO, Cls, ClsT, Name).
remove_from_body((A;B), Cl, NewLit, N, NO, Cls, ClsT, [Name]) :-
	!,
	remove_disj((A;B), Cl, disj, NewLit, N, NO, Cls, ClsT, Name).
remove_from_body((A,B), Cl, (NA,NB), N, NO, Cls, ClsT, Na) :-
	!,
% --- usar lit_concat porque NA,NB no es cierto
	remove_from_body(A, Cl, NA, N, N1, Cls, Cls1, Na1),
	remove_from_body(B, Cl, NB, N1, NO, Cls1, ClsT, Na2),
	append_if_not_member(Na1, Na2, Na).
remove_from_body(A, _Cl, A, N, N, Cls, Cls, []).
%	display(no_toco(A)), nl.



remove_disj((A ; B), Cl, CondOrDisj, NewLit, 
	     N, NO ,
	     Cls, ClsT ,
	     NewLit) :-
	Cl = cls${ head => H, dic => D, locator => Loc },
	create_aux_clause_name(H, CondOrDisj, N, NO, NewHeadName),
	prune_dict((A,B), D, NDict),
        vars_names_dict(NDict, Vars, _),
	Cls  = [a(Cls1), a(Cls2)|ClsT],
%	curr_module(M),
%	atom_concat(M, ':', Mtp),
%	atom_concat(Mtp, NewHeadName, NewModHeadName),
	NewLit =.. [NewHeadName|Vars],
	% Prune_dict to A, complete(A) => not used vars are _
        Cls1 = cls${ head => NewLit, body => A, dic => NDict, locator => Loc },
	Cls2 = cls${ head => NewLit, body => B, dic => NDict, locator => Loc }.


	


create_aux_clause_name(H, CondOrDisj, Numbers, NNumbers, NewHeadNameNumber) :-
	functor(H, F, _),
	atom_concat([F, '_$', CondOrDisj], NewHeadName),
	( member_and_remove(n(NewHeadName, N), Numbers, RestNumbers) ->
	    N1 is N + 1,
	    NNumbers = [n(NewHeadName, N1)|RestNumbers]
	; N = 1,
          NNumbers = [n(NewHeadName, 2)|Numbers]
	),
	atom_number(NA, N),
	atom_concat([NewHeadName, '_$', NA], NewHeadNameNumber).


%%%%%%%%%%%%%%%%%%%%
%% INVERSE STEP   %%
%%%%%%%%%%%%%%%%%%%%

add_disj_and_cond_cl(Cls, Names, NCls) :-
	ghater_cls(Names, Cls, RenCls, RestCls),
	transform_cls_to_disj_and_cond(RenCls, TRenCls),
	substitute_disj_in_body(RestCls, Names, TRenCls, NCls).




ghater_cls([], A, [], A).
ghater_cls([Na|Nas], Cls, [REN|Rest], OrigCls) :-
	get_cls_with_header(Cls, Na, ClsWithNa, ClsWithoutNa),
	ClsWithNa = [CC|_],
	'$~'(CC, cls${body => ClsWithNa}, REN),
	ghater_cls(Nas, ClsWithoutNa, Rest, OrigCls).




get_cls_with_header([], _Na, [], []).
get_cls_with_header([C|Cls], Na, [C|Rest], ClsOrig) :-
	C = cls${ head => H },
	\+ \+ (H=Na),
	!,
	get_cls_with_header(Cls, Na, Rest, ClsOrig).
get_cls_with_header([C|Cls], Na, Rest, [C|ClsOrig]) :-
	get_cls_with_header(Cls, Na, Rest, ClsOrig).




% The body field is the list of the bodies of the clauses that compose
% the predicate
transform_cls_to_disj_and_cond([], []).
transform_cls_to_disj_and_cond([Cls|Rs], [NCls|Rss]) :-
	'$~'(Cls, cls${ body => Cond }, NCls),
	Cls = cls${ body => L },
	transform_cls_to_disj_and_cond__(L, Cond),
%	api_write(L),
%	display('\nSe transforma en:\n'),
%	api_write(NCls),nl,nl,
	transform_cls_to_disj_and_cond(Rs, Rss).




transform_cls_to_disj_and_cond__([], fail).
transform_cls_to_disj_and_cond__([L|Ls], Out) :-
%	L = cls${ body => B },
	B = L,
	conj_to_list(B, BL),
	append(Cond, [!|Then], BL),
	!,
	transform_cls_to_disj_and_cond__(Ls, Else),
	list_to_conj(Cond, CondC),
	list_to_conj(Then, ThenC),
	( Else == fail -> 
	    Out = ( CondC -> ThenC ) 
	; Out = ( CondC -> ThenC ; Else )
	).
transform_cls_to_disj_and_cond__([L|Ls], Out) :-
%	L = cls${ body => B },
	B =  L,
	transform_cls_to_disj_and_cond__(Ls, Other),
	( Other == fail -> 
	    Out =  B
	; Out = (B ; Other)
	).

substitute_disj_in_body([], _Names, _, []).
substitute_disj_in_body([Cl|Cls], Names, TRenCls, [NCl|NCls]) :-
	Cl = cls${ body => B },
	!,
	process_literals(B, subs_lit_per_body(TRenCls), NBL),
	'$~'(Cl, cls${ body => NBL}, NCl),
	substitute_disj_in_body(Cls, Names, TRenCls, NCls).

%substitute_disj_in_body__(B, TRenCls, NB) :-

subs_lit_per_body(L, TRenCls, Cond) :-
	nonvar(L),
	( L = H:_PPI -> true ; L = H ),
	Cls = cls${ head => H, body => Cond },
	member(Cls, TRenCls),
	!.
subs_lit_per_body(L, _TRenCls, L).

	

%%%%%%%%%%%%%%%%%%%%
%% TRANSFORMATION %%
%%%%%%%%%%%%%%%%%%%%


:- push_prolog_flag(multi_arity_warnings, off).

:- multifile transformation/1.
:- multifile transformation/4.

transformation(rem_disj).
/*
transformation(rem_disj, _Cls, _Ds, _Info) :-
	display('Here I am\n'),
	get_clauses(Cls),
	remove_disj_and_cond_cl(Cls, NCls, []),
	api_write(NCls).
*/
:- pop_prolog_flag(multi_arity_warnings).

% %%%%%%%%%%%%%
% %% TESTING %%
% %%%%%%%%%%%%%
%
% :- use_module(library(pretty_print)).
%
% %% _Term = [cls(qsort(A,B,C), (display(pp),display(jj)), dic([A,B,C],['_1','_2','_3'])), cls(qsort(A1,B1,C1), (var(C1) -> display(A1);display(B1)), dic([A1,B1,C1],['_1','_2','_3'])), cls(qsort(A2,B2,C2), ((ground(A2),display(B2);write(C2))), dic([A2,B2,C2],['_1','_2','_3']))], remove_from_cl(_Term, _NewTerm, []), display_cl(_Term), nl,nl,nl, display_cl(_NewTerm).
%
% test1 :-
% 	Term = [
%                  cls(qsort(A,B,C), (display(pp),display(jj)), dic([A,B,C],['_1','_2','_3'])), 
% 	         cls(qsort(A1,B1,C1), (var(C1) -> (write(aa)->true;display(A1));display(B1)), 
%                       dic([A1,B1,C1],['_1','_2','_3'])), 
%                  cls(qsort(A2,B2,C2), ((ground(A2),display(B2);write(C2))), 
%                       dic([A2,B2,C2],['_1','_2','_3']))
%                ],
%         remove_disj_and_cond_cl(Term, NewTerm, [], Names), 
% 	display(names(Names)),nl,nl,
%         display_cl(Term), nl,nl,nl, 
%         display_cl(NewTerm).
%
% %% _Term = [cls(qsort(A,B,C), (display(pp),display(jj)), dic([A,B,C],['_1','_2','_3'])), cls(qsort(A1,B1,C1), (var(C1) -> display(A1),display(B1)), dic([A1,B1,C1],['_1','_2','_3'])), cls(qsort(A2,B2,C2), ((ground(A2),display(B2);write(C2))), dic([A2,B2,C2],['_1','_2','_3']))], remove_from_cl(_Term, _NewTerm, []), display_cl(_Term), nl,nl,nl, display_cl(_NewTerm).
%
% test2 :-  _X = [
% 	         cls(_,'rdj3:p/1/1','rdj3:p'(_657),
%                      'rdj3:p'(_659),'rdj3:p_$cond_$1'(_659),
%                       dic([_659],['X']),loc('pl',2,16,rdj3)),
% 		 cls(_1990,_1991,_1992,
%                       'rdj3:p_$cond_$1'(_659),','('basic_props:num'(_659),','(!,'rdj3:p_$cond_$1_$cond_$1'(_659))),dic([_659],['X']), 
%                       loc('/home/dtm/CiaoDE/ciaopp/testcases/rdj3.pl',2,16,rdj3)),  
% 		 cls(_2708,_2709,_2710,
%                       'rdj3:p_$cond_$1_$cond_$1'(_659),','('arithmetic:>'(_659,1),','(!,','('io_basic:display'('X is greater than 1 '),','('io_basic:display'(_659),'io_basic:nl')))),
% 		      dic([_659],['X']),
% 		      loc('/home/dtm/CiaoDE/ciaopp/testcases/rdj3.pl',2,16,rdj3)),  
% 		 cls(_2728,_2729,_2730,'rdj3:p_$cond_$1_$cond_$1'(_659),','('io_basic:display'('X < 1: '),','('io_basic:display'(_659),'io_basic:nl')),
% 		     dic([_659],['X']),
% 		     loc('/home/dtm/CiaoDE/ciaopp/testcases/rdj3.pl',2,16,rdj3)),  
% 		 cls(_2010,_2011,_2012,'rdj3:p_$cond_$1'(_659),','('io_basic:display'('What is X?'),','('io_basic:display'(_659),'io_basic:nl')),
% 		     dic([_659],['X']),
% 		     loc('/home/dtm/CiaoDE/ciaopp/testcases/rdj3.pl',2,16,rdj3)),  
% 		 cls('$ref'(12446288,1),'rdj3:p/1/2','rdj3:p'(_623),'rdj3:p'(_625),true,
% 		     dic([_625],['_1']),
% 		     loc('/home/dtm/CiaoDE/ciaopp/testcases/rdj3.pl',17,17,rdj3))
% 	      ],
% 		 add_disj_and_cond_cl(_X, ['rdj3:p_$cond_$1','rdj3:p_$cond_$1_$cond_$1'], XX), 
% 		 api_write(XX).
%
%
% display_cl([]) :-
% 	!.
% display_cl([A|As]) :-
% 	!,
%         display_cl(A),
% 	display_cl(As).
% display_cl(A) :-
% 	A = cls${ head => H, body => B, dic => D },
% 	!,
% 	pretty_print(clause(H, B), [], D).

    





