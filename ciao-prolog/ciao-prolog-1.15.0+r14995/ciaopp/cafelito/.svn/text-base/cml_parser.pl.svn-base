:- module(_, _, [assertions, regtypes, dcg, hiord]).

:- use_module(library(lists), [append/3]).
:- use_module(library(read_from_string), [read_from_string_atmvars/3]).

:- use_module(cafelito(parser_utils)).
:- use_module(cafelito(java_lexer)).


method_annotation(Ann) -->
	zero_or_more([single_annotation(_)], Ann).


single_annotation(Ann) -->
	( pred_annotation(Ann)
	    |non_pred_annotation(Ann, entry)
	    |non_pred_annotation(Ann, calls)
	    |non_pred_annotation(Ann, success)
	    |non_pred_annotation(Ann, comp)
	), !.

pred_annotation(Annotation) -->
	line(L1),
	separator("/*@"),
	zero_or_one([flag(_)], Flag),
% call, if any 
	zero_or_one([keyword("if"), pred_cond(_)], HCall),
	zero_or_more([operator('&&', ignore), pred_cond(_)], RCall),
% success, if any 
	zero_or_one([separator("{"), pred_cond(_)], HSucc),
	zero_or_more([operator('&&', ignore), pred_cond(_)], RSucc),
	zero_or_one([separator("}")], _),
% comp	
	zero_or_one([operator('+', ignore), pred_cond(_)], HComp),
	zero_or_more([operator('&&', ignore), pred_cond(_)], RComp),
	separator(";"),
	separator("*/"),
	line(L2),
	{
	    ( Flag = [] ->
		Flag1 = check;
		Flag = [Flag1]
	    ),
	    append(HCall, RCall, Tmp1),
	    unzip(Tmp1, Call, Call_Dict),
	    append(HSucc, RSucc, Tmp2),
	    unzip(Tmp2, Succ, Succ_Dict),
	    append(HComp, RComp, Tmp3),
	    unzip(Tmp3, Comp, Comp_Dict),
	    varnames_union(Call_Dict, Succ_Dict, Dict1),
	    varnames_union(Dict1,     Comp_Dict, Dict),
	    Annotation = ((L1, L2), Flag1, comp, Call, Succ, Comp, Dict)
	}.

non_pred_annotation(Annotation, Type) -->
	line(L1),
	separator("/*@"),
	zero_or_one([flag(_)], Flag),
	type(Type),
	pred_cond(HCond),
	zero_or_more([operator('&&', ignore), pred_cond(_)], RCond),
	separator(";"),
	separator("*/"),
	line(L2),
	{
	    ( Flag = [] ->
		Flag1 = check;
		Flag = [Flag1]
	    ),
	    unzip([HCond|RCond], Cond, Dict),
	    ( Type = success ->
		Call = [],
		Succ = Cond,
		Comp = []
	    ;
		( Type = comp ->
		    Call = [],
		    Succ = [],
		    Comp = Cond
		;
		    Call = Cond,
		    Succ = [],
		    Comp = []
		)
	    ),
	    Annotation = ((L1, L2), Flag1, Type, Call, Succ, Comp, Dict)
	}.


flag(F) -->
	(
	    keyword("check"), {F = check}
	    |keyword("trust"), {F = trust}
	), !.


type(Type) -->
	( keyword("entry"), {Type = entry}
	    |keyword("requires"), {Type = calls}
	    |keyword("ensures"), {Type = success}
	    |keyword("comp"), {Type = comp}
	), !.

pred_cond((New_Term, Dict), I0, IF) :-
	I0 = (S0, L0),
	read_from_string_atmvars(S0, Term, S1),
	atom2var(Term, New_Term, Dict),
	comments_and_spaces((S1, L0), IF).


:- regtype varnamesl/1.

varnamesl(_).

:- pred unzip(Pair_List, Firsts, Seconds) :
	(list(Pair_List), var(First), var(Seconds)) =>
	(list(First, term), varnamesl(Seconds))
# "Convert a list of pairs @var{Pair_List} into a pair of lists
  @var{Firsts} and @var{Seconds} such that the first one contains the
  first elements in the 2-tuple and the former contains the seconds. The
  first element can be of any type; the second is a  dictionary".

unzip([],                  [],    []).
unzip([(Term, Dict)|Rest], NTerm, NDict) :-
	unzip(Rest, RTerm, RDict),
	NTerm = [Term|RTerm],
	varnames_union(Dict, RDict, NDict).


:- pred atom2var(Term, Var_Term, Dict) :
	(nonvar(Term), var(Var_Term), var(Dict)) =>
	(term(Var_Term), varnamesl(Dict))
# " Convert a non-variable term @var{Var} in a variable one
  @var{Var_Term} by replacing all the atoms on it to variables and
  storing the equivalences between them in @var{Dict}".

atom2var([], [], []) :-
	!.
atom2var(true, [], []) :-
	!.
% atom2var(A,_Var,Dict):-
%    atom(A),
%    !,
%    Dict = ['='(A,_Var)].
atom2var([H|T], [NH|NT], Dict) :-
	!,
	atom2var(H, NH, Dict1),
	atom2var(T, NT, Dict2),
	varnames_union(Dict1, Dict2, Dict).
atom2var(F, NF, Dict) :-
	F =.. [H|R],
	!,
	atom2var(R, NR, Dict),
	NF =.. [H|NR].
atom2var(A, A, []) :-
	!.


:- pred varnames_union(Dict1, Dict2, Dict) :
	(varnamesl(Dict1), varnamesl(Dict2), var(Dict)) => varnamesl(Dict)
# "Union of dictionaries @var{Dict1} and @var{Dict1} by unifying 
all those pairs whose first element (the atom) are equal".

varnames_union([],     D,    D).
varnames_union([H|RH], Dict, NDict) :-
	varnames_union_el(H, Dict, Dict1),
	varnames_union(RH, Dict1, NDict).

varnames_union_el(E, L, L) :-
	member(E, L),
	!.
varnames_union_el(E, L, [E|L]).
