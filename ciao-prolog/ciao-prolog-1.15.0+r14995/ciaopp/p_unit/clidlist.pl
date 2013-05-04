:- module(clidlist,
	[ cleanup_clidlist/0,
	  % keys
	  clause_key/2,
	  clauses_keys/2,
	  clause_number/2,
	  first_key/2,
	  last_clause/1,
	  null_directive_key/1,
	  % program rewriting
	  inverse_rewrite_source_program/2,
	  inverse_rewrite_source_program_cl/2,
	  inverse_rewrite_source_body/2,
	  % --- DTM: KLUDGE: HAVE TO DISSAPEAR SOON!
	  inverse_rewrite_source_program_wk/2,

%	  rewrite_source_program/2,
	  rewrite_source_all_clauses/2,
	  rewrite_source_clause/4,
	  rewrite_cls/2,
	  % extracting information
	  atom2data/5,
	  clid2data/4,
	  pred2data/3,
	  is_clid/1,
	  is_atomid/1,
	  term2atom/2,
	  is_directive/3,
          is_clause/4,
	  orig_clause_id/2,
	  unpack_id/2
	],
	[assertions, basicmodes, regtypes, andprolog]).

:- use_module(program(prednames), [orig_pred_name/2]).
:- use_module(program(p_unit_basic), [type_of_goal/2, meta_to_list/2]).
:- use_module(library(lists), [append/3, length/2]).
:- use_module(program(clidtypes)).
:- reexport(program(clidlist_basic)).

%----------------------------------------------------------------------------

:- doc(module,"This module handles the format of the structures used to
   represent the program. It is a list of clauses, where each clause is
   identified by a key, and the clause body is a conjunction of literals,
   where each literal is also identified by a key.

   When creating a new clause for a predicate @tt{H} in the program, the 
   following goal will give you a clause in the correct format:
   @begin{verbatim}
clause_key(H,ClId), rewrite_source_clause(H,Body,ClId,Clause), 
   @end{verbatim}
").

% :- doc(bug,"1. atom2data/5 and clid2data/4 should be reprogrammed.").
:- doc(bug,"2. Code must be cleaned up.").
% :- doc(bug,"3. term2atom/2 definition is a botched job and should be 
% 	reprogrammed.").
:- doc(bug,"4. PRED_N and the meta-terms should not be done here.").
:- doc(bug,"5. Revise first_key for the treatment of meta preds.").
:- doc(bug,"6. This is not true with user files:
	if a goal is a meta-predicate, its meta-arguments are terms of
	the form @tt{'$'(Term,Body,Type)},").
/* 
Module a:
clause('a:p',('a:a':'a:p/0/1/1',
              'aggregates:findall'(_1897,$('a:b','a:b':'a:p/0/1/3',goal),
                                   _1899):'a:p/0/1/2'))
User file:
clause('user:p',('user:a':'user:p/0/1/1',
	         'aggregates:findall'(_1913,'user:b',_1915):'user:p/0/1/2'))
*/

/*
%----------------------------------------------------------------------------
% these should be obsolete by now:

:- prop idclause(CL) # "@var{CL} is a @tt{(clause,id)} pair.".

%idclause((clause(H,_B),Id)):- clause_id(H,Id).
idclause( (directive(_B),Id) ) :- null_directive_key(Id).
idclause( (directive(_B),Id) ) :- null_assertion_key(Id).
idclause( (directive(_B),Id) ) :- assertion_key(Id).
*/

:- doc(hide,null_directive_key/1).
:- regtype null_directive_key/1.

null_directive_key((0,0)).

/*
:- regtype null_assertion_key/1.

null_assertion_key((0,1)).

:- regtype assertion_key/1.

assertion_key((K,N)):- atomic(K), number(N).
*/
% directive_key((K,N),K,N).

:- doc(doinclude,body/1).
:- prop body/1.
:- doc(body(Body),"@var{Body} is a conjunction of simple goals:
	@includedef{body/1}").
body((G,B)):-
	callable(G),
	body(B).
body(G):-
	callable(G).

%---------------------------------------------------------------------------
% The keys

:- data counter/3.

:- doc(cleanup_clidlist/0,"Cleanups internal database.").

cleanup_clidlist :- retractall_fact( counter(_,_,_) ).

/*
directive_inccounter( Val) :-
        ( retract_fact(counter( 0 , OldVal,_)) -> 
	  Val is OldVal + 1
	; Val = 1 ),
        asserta_fact(counter( 0, Val, Val)).
*/

:- pred clause_key(H,ClId) : head * var => clid(ClId)
	# "@var{ClId} is the clause key that an additional clause of
           predicate @var{H} should have.".

%% clause_key(0,Clid):-
%% 	clause_count(directive,Clid).
%% clause_key(1,Clid):-
%% 	clause_count(assertion,Clid).
clause_key(H,Clid):-
	functor(H,F,A),
	make_atom([F,A],Key),
	clause_count(Key,Clid).

clause_count(Key,Clid):-
        ( retract_fact(counter(Key, OldVal,_)) -> 
	  Val is OldVal + 1
	; Val = 1 ),
	make_atom([Key,Val],Clid),
        asserta_fact( counter(Key, Val, Clid) ).

clause_number(H,N):-
	functor(H,F,A),
	make_atom([F,A],Key),
	current_fact( counter(Key, N, _) ).

:- regtype head(Head) # "@var{Head} is a clause head identifier.".
:- doc(head/1,"Defined as @includedef{head/1}").
:- doc(doinclude,head/1).

head(0).
head(1).
head(X):- callable(X).

:- pred clauses_keys(H,ClIds) : callable * var => list(ClIds,clid)
	# "@var{ClIds} are the clause keys of all clauses already known
           for predicate @var{H}.".

clauses_keys(H,Keys):-
	functor(H,F,A),
	make_atom([F,A],Key),
	current_fact( counter(Key, N, _) ),
	clauses_keys_(Key,0,N,Keys).

clauses_keys_(_Key,N0,N,CutCls):- N0>N, !,
	CutCls=[].
clauses_keys_(Key,N0,N,[Id|CutCls]):-
	make_atom([Key,N0],Id),
	N1 is N0+1,
	clauses_keys_(Key,N1,N,CutCls).

:- pred last_clause(ClId) : clid(ClId)
	# "@var{ClId} is the clause key of the last known clause of
           the predicate to which it corresponds.".

last_clause(Clid):-
        counter(_, _,Clid).

/*
%----------------------------------------------------------------------------
% Rewrite source program, putting in ':' to all goals
% input:  folded format - plain syntax - with '\&'
% output: internal format - plain syntax - with '\&'
%----------------------------------------------------------------------------

rewrite_source_program( Cls, NCls ) :-
	cleanup_clidlist,
	rewrite_source_program_(Cls,NCls).

rewrite_source_program_([],[]).
rewrite_source_program_([clid(Cl,Loc)|Cls],[clid(NCl,Clid,Loc)|NCls]):-
	rewrite_source_program_3(Cl,Clid,NCl),
	rewrite_source_program_(Cls,NCls).

rewrite_source_program_3(directive(D),Clid,directive(D)):- !,
	directive_inccounter( Clid ).
%	clause_key(0,Clid).
rewrite_source_program_3(clause(H,true),Clid,clause(H,true)):- !,
	clause_key(H,Clid).
rewrite_source_program_3(clause(H,!),Clid,clause(H,!)):- !,
	clause_key(H,Clid).
rewrite_source_program_3(clause(H,B),Clid,clause(H,NB)):- !,
	clause_key(H,Clid),
	rewrite_source_body(B,Clid,0,_,NB).
*/

:- doc(rewrite_source_all_clauses(Clauses,NewClauses),
      "@var{NewClauses} is the result of applying the format
      transformation to the list of clauses @var{Clauses}.").

rewrite_source_all_clauses([],[]).

rewrite_source_all_clauses([clause(H,B)|Clauses],[NCl:ClId|NewClauses]):-
	clause_key(H,ClId), 
	rewrite_source_clause(H,B,ClId,NCl),
	rewrite_source_all_clauses(Clauses,NewClauses).

:- regtype headcl/1.

headcl(0).
headcl(H) :- callable(H).

:- pred rewrite_source_clause(H,B,ClId,Clause)
	: headcl * body * clid * var => clause(Clause)
        # "@var{Clause} is an structure representing the clause 
	  @var{H} :- @var{B} identified by @var{ClId}.".

rewrite_source_clause(0,Body,_Clid,Clause):- !,
	Clause=directive(Body).
rewrite_source_clause(H,B,Clid,clause(H,NB) ):-
	rewrite_source_body(B,Clid,0,_,NB).
	
rewrite_source_body(A,Clid,N,N1,AK):-
	var(A), !,
	rewrite_source_atom(A,Clid,N,N1,AK).
rewrite_source_body((A,B),K,N0,N,(NA,NB)):- !,
	rewrite_source_body(A,K,N0,N1,NA),
	rewrite_source_body(B,K,N1,N,NB).
rewrite_source_body(!,_,N,N,!):- !.
rewrite_source_body(builtin(B),_,N,N,builtin(B):noinfo):- !.
rewrite_source_body(A,Clid,N,N1,AK):-
	type_of_goal(metapred(_Type,Meta),A), !,
	functor(A,F,Args),
	functor(B,F,Args),
	rewrite_source_atom(B,Clid,N,N0,AK),
	meta_to_list(Meta,MetaL),
	meta_calls(A,0,Args,MetaL,Clid,N0,N1,B).
rewrite_source_body(A:K,_Clid,N,N,A:K):- !.
rewrite_source_body(A,Clid,N,N1,AK):-
	rewrite_source_atom(A,Clid,N,N1,AK).




%rewrite_source_atom(A:_,Clid,N,N1,A:Key):-
%	N1 is N+1,
%	make_atom([Clid,N1],Key).

rewrite_source_atom(A,Clid,N,N1,A:Key):-
	N1 is N+1,
	make_atom([Clid,N1],Key).

meta_calls(_,Args,Args,_Meta,_Clid,N,N,_):- !.
meta_calls(A,Arg0,Args,[M1|Meta],Clid,N0,N,B):-
	Arg1 is Arg0+1,
	arg(Arg1,A,A1),
%	arg(Arg1,Meta,M1),
	( meta_term(M1,A1,Meta1,goal)
	-> rewrite_source_body(Meta1,Clid,N0,N1,A2),
	   B1='$'(A1,A2,goal)
	 ; B1=A1,
	   N1=N0
	),
	arg(Arg1,B,B1),
	meta_calls(A,Arg1,Args,Meta,Clid,N1,N,B).

% This may add variables (spec and pred(N)), which are not in the
% dictionaries of the clauses!!!
meta_term(goal,B,B,goal).
meta_term(:,B,B,goal).
meta_term(clause,D,D,data).
meta_term(fact,D,D,data).
meta_term(spec,Term,G,goal):-
	( nonvar(Term), Term=F/A, nonvar(F), nonvar(A)
	  -> functor(G,F,A)
	   ; true % G a variable 
	).
meta_term(pred(N),Term,G,goal):-
	( nonvar(Term), number(N)
	-> length([A|L],N),
	   Term=..[F|As],
	   append([A|As],L,Args),
	   G=..[F|Args]
	 ; true % G a variable
	).

%----------------------------------------------------------------------------
% Rewrite source program, by just taking out ':'
% input:  internal format - full syntax
% output: folded format - full syntax
%----------------------------------------------------------------------------


inverse_rewrite_source_program_wk([],[]) :- !.
inverse_rewrite_source_program_wk([Cl:K|Cls],[NCl:K|NCls]):-
	!,
	inverse_rewrite_source_program_cl(Cl,NCl),
	inverse_rewrite_source_program_wk(Cls,NCls).

% THIS_ONE_NEEDS_REVISION
:- doc(hide,inverse_rewrite_source_program/2).

inverse_rewrite_source_program([],[]) :- !.
inverse_rewrite_source_program([Cl:_|Cls],[NCl|NCls]):-
	!,
	inverse_rewrite_source_program_cl(Cl,NCl),
	inverse_rewrite_source_program(Cls,NCls).

inverse_rewrite_source_program_cl(directive(D),directive(D)) :- !.
inverse_rewrite_source_program_cl(clause(H,true),clause(H,true)):- !.
inverse_rewrite_source_program_cl(clause(H,!),clause(H,!)):- !.
inverse_rewrite_source_program_cl(clause(H,B),clause(H,NB)):-
	!,
	inverse_rewrite_source_body(B,NB).
	
inverse_rewrite_source_body((A,B),(NA,NB)):-
	!,
	inverse_rewrite_source_body(A,NA),
	inverse_rewrite_source_body(B,NB).
inverse_rewrite_source_body((A&B),(NA&NB)):-
	inverse_rewrite_source_body(A,NA),
	inverse_rewrite_source_body(B,NB).
%% inverse_rewrite_source_body((A\&B),(NA\&NB)):-
%% 	inverse_rewrite_source_body(A,NA),
%% 	inverse_rewrite_source_body(B,NB).
inverse_rewrite_source_body((A->B;C),(NA->NB;NC)):-
	!,
	inverse_rewrite_source_body(A,NA),
	inverse_rewrite_source_body(B,NB),
	inverse_rewrite_source_body(C,NC).
inverse_rewrite_source_body((A->B),(NA->NB)):-
	!,
	inverse_rewrite_source_body(A,NA),
	inverse_rewrite_source_body(B,NB).
inverse_rewrite_source_body((A=>B),(NA=>NB)):-
	!,
	inverse_rewrite_source_body(A,NA),
	inverse_rewrite_source_body(B,NB).
inverse_rewrite_source_body(!,!):- !.
inverse_rewrite_source_body(\+($(A,_,_)):_,\+(A)):- !.
%inverse_rewrite_source_body('hiord_rt:call'($(A,_,_)):_,A):- !.
inverse_rewrite_source_body('hiord_rt:call'($(A,_,_)):_,NBody):- !,
	NBody = 'hiord_rt:call'(A).
inverse_rewrite_source_body('aggregates:bagof'(Var,$(A,_,_),List):_Id,NBody):-!,
	NBody = 'aggregates:bagof'(Var,A,List).
inverse_rewrite_source_body('aggregates:setof'(Var,$(A,_,_),List):_Id,NBody):-!,
	NBody = 'aggregates:setof'(Var,A,List).
inverse_rewrite_source_body('aggregates:findall'(Var,$(A,_,_),List):_Id,NBody):-!,
	NBody = 'aggregates:findall'(Var,A,List).
inverse_rewrite_source_body(A:_,A):- !.
inverse_rewrite_source_body(A,A):- !,
	display( 'Sorry cannot process: ' ),
	display( A ),nl.

%% inverse_rewrite_source_body(ground(L),ground(L)):- !.
%% inverse_rewrite_source_body(indep(L),indep(L)):- !.

%---------------------------------------------------------------------------

:- pred is_atomid(Id):: atm 

# "Succeeds if @var{Id} is a valid atom identifier. The predicate name
  must not contain '/' characters.".

is_atomid(Id):-
	atom2data(Id,_N,_A,_C,_G).

:- pred is_clid(Id):: atm

# "Succeeds if @var{Id} is a valid clause identifier. The predicate
  name must not contain '/' characters.".

is_clid(Id):-
	\+ atom2data(Id,_,_,_,_),
	clid2data(Id,_N,_A,_C).
	
:- pred term2atom(+Term,-Atom) :: term * atm

# "Returns in @var{Atom} the representation of @var{Term} as it was
  written using @tt{format(\"~w\",[Term])}.".

:- use_module(library(format), [sformat/3]).
term2atom(Term,Atom):-
	sformat(String,"~w",[Term]),
	atom_codes(Atom,String),
	!.

%---------------------------------------------------------------------------
% Management of internal keys
%---------------------------------------------------------------------------
% These should become obsolote:

first_key(((!),Right),Key):-!, % Added by PLG 11-Oct-04
	first_key(Right,Key).
first_key((_:noinfo,Right),Key):-!,
	first_key(Right,Key).
first_key((Left,_Right),Key):-
	first_key(Left,Key).
first_key((Left & _Right),Key):-
 	first_key(Left,Key).
%% first_key((Left \& _Right),Key):-
%% 	first_key(Left,Key).
first_key(_:noinfo,_):-!,
	fail.
first_key((_:Key),Key).

/*
incr_key(Key,Key1):-
	separate_atom(Key,[F,A,C,N]),
	N1 is N+1,
	make_atom([F,A,C,N1],Key1).

create_key(Key,Key1):-
	separate_atom(Key,[F,A,C,_]),
	make_atom([F,A,C],Key1).

separate_atom(Key,Components):-
	name(Key,String),
	name('/',[Slash]),
	separate_each(String,Slash,Components).

separate_each(String,Slash,[Comp|Components]):-
	append(String1,[Slash|Rest],String), !,
	name(Comp,String1),
	separate_each(Rest,Slash,Components).
separate_each(String,_Slash,[Comp]):-
	name(Comp,String).

false_key(F/A,Key):- make_atom([F,A,0,0],Key).

false_create_key(F/A,Key):- make_atom([F,A,0],Key).

subgoal_number(Key,N):- separate_atom(Key,[_F,_A,_C,N]).

clidlist2cllist([],[]).
clidlist2cllist([(Cl,_)|Cls],[NCl|NCls]):-
	clidlist2cllist(Cl,NCl),
	clidlist2cllist(Cls,NCls).
clidlist2cllist(directive(D),(:- D)).
clidlist2cllist(clause(H,true),H):- !.
clidlist2cllist(clause(H,B),(H:-B)).
*/

:- doc(hide,rewrite_cls/2).

rewrite_cls([clid(Cl,_,_)|Cls0],[Cl|Cls]):-
	rewrite_cls(Cls0,Cls).
rewrite_cls([],[]).

%-------------------------------------------------------------------%
% next two predicates are necessary to get the information not      %
% directly available stored in the keys for goals and clauses       % 
%-------------------------------------------------------------------%

:- pred pred2data(PredId,F,A) :: predid * atm * num
	# "@var{PredId} identifies the predicate @var{F}/@var{A}.".

pred2data(Atom,N,A):-
	atom(Atom),
	!,
	unpack_id( Atom , List ),
	append( Name , [LA] , List ),
	unpack_id( N , Name ),
 	name(A,LA).
pred2data(Atom,N,A):-
	unpack_id( N , Name ),
 	name( A , LA ),
	append( Name , [LA] , List ),
	unpack_id( Atom , List ).


:- pred clid2data(ClId,F,A,C) :: clid * atm * num * num
	# "@var{ClId} identifies the @var{C}th clause of predicate
	  @var{F}/@var{A}.".

clid2data(Clid,N,A,C):-
	unpack_id( Clid , List ),
	append( Name , [LA, LC] , List ),
	unpack_id( N , Name ),
 	name(A,LA),
 	name(C,LC).


% --- DTM: should be literal2data!
:- pred atom2data(AtId,F,A,C,G) :: bodykey * atm * num * num * num
	# "@var{AtId} identifies the @var{G}th literal of the body of the
	  @var{C}th clause of predicate @var{F}/@var{A}.".

atom2data(Atom,N,A,C,G):-
	unpack_id( Atom , List ),
	append( Name , [LA, LC,LG] , List ),
	unpack_id( N , Name ),
 	name(A,LA),
 	name(C,LC),
 	name(G,LG).



is_directive(directive(Body):Clid, Body, Clid).
is_clause(clause(Head,Body):Clid, Head, Body, Clid).

:- doc(orig_clause_id(ClId,Orig_ClId), "@var{Orig_ClId} is the
      clause identifier resulting from replacing the name of a newly
      generated predicate with the name of its corresponding predicate
      in the original program. ").

orig_clause_id(ClId,Orig_ClId):-
	clid2data(ClId,Pred,Arity,Clause),
	orig_pred_name(Pred,Orig_Pred),
	make_atom([Orig_Pred,Arity,Clause],Orig_ClId).



:- pred unpack_id(Atom,IdList) :: atm * list(string)
# "Given an atom @var{String} (which contains 0'/ characters) it
returns a list of substrings of @var{String} contained between 0'/
characters.".

% ALMA MATTER :D
% An atom is given...
unpack_id( A , B ) :-
	atom(A),
	atom_codes( A , AA ),
	unpack_id_( AA , B ),
	!.

% The atom is the output
unpack_id( A , B ) :-
	unpack_id_( AA , B ),
	!,
	atom_codes( A , AA ).


unpack_id_( [0'/|R] , [ [] | L ] ) :-
	unpack_id_( R , L ).
unpack_id_( [A|R] , [ [A|AA] | As ] ) :-
	unpack_id_( R , [AA|As] ).
unpack_id_( [] , [ [] ] ).


:- pred make_atom(AtomNumberList,Id) :: list(constant) * atm
# "@var{Id} is the atom obtained from contatenating each element of
   @var{AtomNumberList} with '/' between elements.".

make_atom( AtomList , Key ) :-
	namel_2_atoml( AtomList , StrList ),
	unpack_id( Key , StrList ).


namel_2_atoml( [] , [] ).
namel_2_atoml( [A|AA] , [S|SS] ) :-
	name( A , S ),
	namel_2_atoml( AA , SS ).
