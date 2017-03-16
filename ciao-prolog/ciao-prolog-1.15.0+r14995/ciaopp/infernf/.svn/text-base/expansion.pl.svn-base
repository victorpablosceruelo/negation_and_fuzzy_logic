% Expansion of type annotated terms.

:- pred expand_ta_term(+Ta_Term, +BS, -NewTa_Term, -Rest_Ta)

# "Performs the expansion of the type-annotated term @var{Ta_Term}
      respect to @var{BS}, into the type-annotated term
      @var{NewTa_Term} and the list of type-annotated terms
      @var{Rest_Ta}. @var{BS} is syntactically a basic set, but
      represents the cobasic set comp(@var{BS}). @var{NewTa_Term} is
      an instance of @var{Ta_Term}.  The pair @var{NewTa_Term},
      @var{Rest_Ta} is a partition of @var{Ta_Term}. For any variable
      x in @var{NewTa_Term} it holds that its type is an infinite
      function symbol type, or theta(x) is a variable, where theta is
      the mgu of @var{BS} and the tuple of term of @var{Ta_Term}.
      This predicate can fail if @var{Ta_Term} and @var{BS} are
      disjoint (in that case expansion is not possible). The variables
      in @var{Ta_Term} and @var{BS} are not instantiated. This
      predicate corresponds to the function expansion(R, Cob) of the
      paper.".

expand_ta_term(Ta_Term, BS, Ta_T1, Rest_ta) :-
%% type_rule_simplify, %% -PL warning!
	expand_ta_term_0(Ta_Term, BS, Ta_T1, Rest_ta).
% selec_type_rule_simplify, !. %% -PL warning!
% I think is not neccesary because rules are yet simplified.

expand_ta_term_0(Ta_Term, BS, Ta_T1, Rest_ta) :-
	Ta_Term = (Term, _TypeAss),
	copy_term(Term, New_Term),
	copy_term(BS,   New_BS),
	closed_var_list(Term,     Var_List),
	closed_var_list(New_Term, New_Var_List),
	unify_with_occurs_check(New_Term, New_BS),
	ta_expand1(Var_List, New_Var_List, Ta_Term, [], Ta_T1, Rest_ta).

ta_expand1([], [], ITa_Term, InTaList, ITa_Term, InTaList) :-
	!.
ta_expand1([_Var|VList], [Term|TList], ITa_Term, InTaList,
	    OTa_Term, OuTaList) :-
	var(Term),
	!,
	ta_expand1(VList, TList, ITa_Term, InTaList, OTa_Term, OuTaList).
ta_expand1([Var|VList], [Term|TList], ITa_Term, InTaList,
	    OTa_Term, OuTaList) :-
	nonvar(Term),
	ITa_Term = (ITerm, ITypeAss),
	find_type(ITypeAss, Var, Type),
	functor(Term, F, A),
	find_type_functor(F/A, Type, [], Type1, Types2),
% Types2 is a list of types.
	( Types2 == [] ->
	    NewInTaList = InTaList;
	    change_type_in_typassgn(Types2, Var, ITypeAss, TypAss2),
	    NewInTaList = [(ITerm, TypAss2)|InTaList]
	),
	functor(NTerm, F, A),
	remove_item_from_typAss(ITypeAss, Var, ITypAss3),
	actualize_var_term_typass(A, NTerm, Term, Type1,
	    VList, TList, ITypAss3,
	    OuVList, OuTList, OuTypAss3),
	replace_var_by_term(ITerm, Var, NTerm, Term3),
	ta_expand1(OuVList, OuTList, (Term3, OuTypAss3), NewInTaList,
	    OTa_Term, OuTaList).

% not_enpandible_var(Term, Type) :-
% 	not_expandible_type(Type) ; var(Term).

:- pred actualize_var_term_typass(+A, +NTerm, +Term, +Type,
	    +IVarList, +ITermList, +InTypAss,
	    -OVarList, -OTermList, -OuTypAss)

# "Expands the type annotated term 

Ta_T1 and Rest_ta are a partition of Ta_term

@var{Type} is the (compound) type with the same main functor than
@var{Term} (and @var{NTerm}).  @var{NTerm} has the same main functor
than @var{Term}, but its arguments are all variables.  @var{IVarList}
is the list of variables appearing in the tuple of terms (also basic
set) of a type annotated term, and @var{ITermList} is the list of
terms to which variables in @var{IVarList} are bounded to (in
correspondence order). That is, the pair @var{IVarList} and
@var{ITermList} represent a substitution.  @var{InTypAss} is a type
assignment that stores the type of each variable in
@var{IVarList}. The variables in @var{InTypAss} are exactly those
variables in @var{IVarList}. The same explained relation between
@var{IVarList}, @var{ITermList} and @var{InTypAss} holds for
@var{OVarList}, @var{OTermList}, @var{OuTypAss}.".


actualize_var_term_typass(0, _NTerm, _Term, _Type,
	    IVarList, ITermList, InTypAss,
	    IVarList, ITermList, InTypAss) :- !.
actualize_var_term_typass(A, NTerm,  Term,  Type,
	    IVarList, ITermList, InTypAss,
	    OVarList, OTermList, OuTypAss) :-
	A > 0, NA is A - 1,
	arg(A, NTerm, NTeArg), % NTeArg is a variable.
	arg(A, Term,  TeArg),
	arg(A, Type,  TyArg),
	add_item_to_typassign(InTypAss, NTeArg, TyArg, NewInTypAss),
	NewIVarList = [NTeArg|IVarList],
	NewITermList = [TeArg|ITermList],
	actualize_var_term_typass(NA, NTerm, Term, Type,
	    NewIVarList, NewITermList, NewInTypAss,
	    OVarList, OTermList, OuTypAss).


%% Expansion of finite types.

finite_expand(Vars, Ta_Term, Expan) :-
	Ta_Term = (Term, TypAss),
	expand_2(Vars, TypAss, [Term], Expan).

expand_2([], TyAssign, IExpan, OExpan) :- !,
	put_type_assignments(IExpan, TyAssign, [], OExpan).
expand_2([Var|SelVars], TyAssign, IExpan, OExpan) :-
	find_type(TyAssign, Var, Type),
	finite_unfold(Type, Terms),
	create_ta_terms(Terms, Var, IExpan, TExpan),
	remove_item_from_typAss(TyAssign, Var, NTyAssign),
	expand_2(SelVars, NTyAssign, TExpan, OExpan).

create_ta_terms([],       _Var, Expan,  Expan) :- !.
create_ta_terms([Term|L], Var,  IExpan, OExpan) :-
	all_replace_var_by_term(IExpan, Var, Term, [], TExpan),
	create_ta_terms(L, Var, TExpan, OExpan).

all_replace_var_by_term([],             _Var, _Term, Expan,  Expan) :- !.
all_replace_var_by_term([Term1|ExpanL], Var,  Term,  IExpan, OExpan) :-
	replace_var_by_term(Term1, Var, Term, NTerm),
	all_replace_var_by_term(ExpanL, Var, Term, [NTerm|IExpan], OExpan).

put_type_assignments([],       _TyAssign, Expan,  Expan) :- !.
put_type_assignments([Term|L], TyAssign,  IExpan, OExpan) :-
	put_type_assignments(L, TyAssign, [(Term, TyAssign)|IExpan], OExpan).



finite_unfold(Type, NType) :-
	functor_pure_type_term(Type), !,
	unfold_args(Type, NType).
finite_unfold(Type, LTypes) :-
	em_defined_type_symbol(Type, TyDefin), !,
	finite_unfold_union_1(TyDefin, [], LTypes).

unfold_args(Type, NType) :-
	functor(Type,  F, A),
	functor(NType, F, A),
	unfold_args_3(A, Type, NType).

unfold_args_3(0, _,    _) :- !.
unfold_args_3(A, Type, NType) :-
	A > 0,
	arg(A, Type, Arg),
	finite_unfold(Arg, NArg),
	arg(A, NType, NArg),
	A1 is A - 1,
	unfold_args_3(A1, Type, NType).

%% We assume that type rules are simplified (unfolded, and removed empty
%% types, and top simplified.

finite_unfold_union_1([Type|Defin], InDefin, OuDefin) :-
	functor_pure_type_term(Type), !,
	unfold_args(Type, NType),
	finite_unfold_union_1(Defin, [NType|InDefin], OuDefin).
finite_unfold_union_1([Type|Defin], InDefin, OuDefin) :-
	em_defined_type_symbol(Type, TyDefin), !,
	append(TyDefin, Defin, TemDefin),
	finite_unfold_union_1(TemDefin, InDefin, OuDefin).
