:- module(clidlist_,
	[ cleanup_clidlist/0,
	  % types
	  clause/1,
	  % keys
	  clause_key/2,
	  clauses_keys/2,
	  clause_number/2,
	  first_key/2,
	  last_clause/1,
	  null_directive_key/1,
	  % program rewriting
	  inverse_rewrite_source_program/2,
%	  rewrite_source_program/2,
	  rewrite_source_all_clauses/2,
	  rewrite_source_clause/4,
	  rewrite_cls/2,
	  % extracting information
	  atom2data/5,
	  clid2data/4,
	  pred2data/3,
	  clid_of_atomid/2,
	  is_clid/1,
	  is_atomid/1,
	  term2atom/2,
	  is_directive/3,
          is_clause/4,
	  orig_clause_id/2
	],
	[ assertions, basicmodes, regtypes
	]).

:- use_module('..'(spec(s_simpspec_)), [make_atom/2]).
:- use_module(p_unit_, [type_of_goal/2]).
:- use_module('..'(spec(unfold_operations_)), [orig_pred_name/2]).
:- use_module(library(lists), [append/3, length/2, reverse/2]).

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

:- doc(bug,"1. atom2data/5 and clid2data/4 should be reprogrammed.").
:- doc(bug,"2. Code must be cleaned up.").
:- doc(bug,"3. term2atom/2 definition is a botched job and should be 
	reprogrammed.").
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

%----------------------------------------------------------------------------
% these should be obsolete by now:

:- prop idclause(CL) # "@var{CL} is a @tt{(clause,id)} pair.".

%idclause((clause(H,_B),Id)):- clause_id(H,Id).
idclause((directive(_B),Id)):- null_directive_key(Id).
idclause((directive(_B),Id)):- null_assertion_key(Id).
idclause((directive(_B),Id)):- assertion_key(Id,_,_).

:- doc(hide,null_directive_key/1).
:- regtype null_directive_key/1.

null_directive_key((0,0)).

:- regtype null_assertion_key/1.

null_assertion_key((0,1)).

:- regtype assertion_key/1.

assertion_key((K,N),K,N):- atomic(K), number(N).

directive_key((K,N),K,N).

%----------------------------------------------------------------------------
% The type of the program clauses

:- regtype clause/1.
:- doc(clause/1,"Defined as: @includedef{clause/1}").

clause(clause(Head,Body):Clid):-
	atom(Head),
	clausebody(Body),
	clid(Clid).
clause(directive(Body):Clid):-
	clausebody(Body),
	drid(Clid).

:- doc(doinclude,clausebody/1).
:- prop clausebody/1.
:- doc(clausebody(Body),"@var{Body} is a conjunction of simple goals;
	if a goal is a meta-predicate, its meta-arguments are terms of
	the form @tt{'$'(Term,Body,Type)}, where @tt{Term} is the original
	goal term, @tt{Type} the type of meta-term as in the meta_predicate
	directive, and, if @tt{Type} corresponds to an executable form of
	meta-term, @tt{Body} is the @tt{clausebody} that corresponds to
	@tt{Term}.
	@includedef{clausebody/1}").
clausebody(G:K):-
	callable(G),
	bodykey(K).
clausebody((G:K,B)):-
	callable(G),
	bodykey(K),
	clausebody(B).

:- doc(doinclude,body/1).
:- prop body/1.
:- doc(body(Body),"@var{Body} is a conjunction of simple goals:
	@includedef{body/1}").
body(G):-
	callable(G).
body((G,B)):-
	callable(G),
	body(B).

:- doc(doinclude,bodykey/1).
:- regtype bodykey(Key)
	# "@var{Key} is an atom that uniquely identifies a program point
	   of a program clause body literal.".
bodykey(Key):- atom(Key).

:- doc(doinclude,clid/1).
:- regtype clid(Id)
	# "@var{Id} is an atom that uniquely identifies a program clause.".
clid(Clid):- atom(Clid).

:- doc(doinclude,predid/1).
:- regtype predid(Id)
	# "@var{Id} is an atom that uniquely identifies a program predicate.".
predid(Clid):- atom(Clid).

:- doc(doinclude,drid/1).
:- regtype drid(Id)
	# "@var{Id} is a number that uniquely identifies a program directive.".
drid(Clid):- nnegint(Clid).


%---------------------------------------------------------------------------
% The keys

:- data counter/2.

:- doc(cleanup_clidlist/0,"Cleanups internal database.").

cleanup_clidlist:- retractall_fact(counter(_,_)).

inccounter(Name, Val) :-
        ( retract_fact(counter(Name, OldVal)) -> 
	  Val is OldVal + 1
	; Val = 1 ),
        asserta_fact(counter(Name, Val)).

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
	inccounter(Key,Val),
	make_atom([Key,Val],Clid).

clause_number(H,N):-
	functor(H,F,A),
	make_atom([F,A],Key),
	current_fact(counter(Key, N)).

:- regtype head(X) # "@var{Head} is a clause head identifier.".
:- doc(head/1,"Defined as @incudedef{head/1}").
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
	current_fact(counter(Key, N)),
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
	name(Clid,String),
	name('/',[C]),
	reverse(String,Reversed),
	append(X,[C|S],Reversed), !,
	reverse(S,Name),
	name(Key,Name),
	reverse(X,Num),
	name(N,Num),
	current_fact(counter(Key, N)).

%----------------------------------------------------------------------------
% Rewrite source program, putting in ':' to all goals
% input:  folded format - plain syntax - with '\&'
% output: internal format - plain syntax - with '\&'
%----------------------------------------------------------------------------

rewrite_source_program(Cls,NCls):-
	retractall_fact(counter(_,_)),
	rewrite_source_program_(Cls,NCls).

rewrite_source_program_([],[]).
rewrite_source_program_([clid(Cl,Loc)|Cls],[clid(NCl,Clid,Loc)|NCls]):-
	rewrite_source_program_3(Cl,Clid,NCl),
	rewrite_source_program_(Cls,NCls).

rewrite_source_program_3(directive(D),Clid,directive(D)):- !,
	inccounter(0,Clid).
rewrite_source_program_3(clause(H,true),Clid,clause(H,true)):- !,
	clause_key(H,Clid).
rewrite_source_program_3(clause(H,!),Clid,clause(H,!)):- !,
	clause_key(H,Clid).
rewrite_source_program_3(clause(H,B),Clid,clause(H,NB)):- !,
	clause_key(H,Clid),
	rewrite_source_body(B,Clid,0,_,NB).

:- doc(rewrite_source_all_clauses(Clauses,NewClauses),
      "@var{NewClauses} is the result of applying the format
      transformation to the list of clauses @var{Clauses}.").

rewrite_source_all_clauses([],[]).

rewrite_source_all_clauses([clause(H,B)|Clauses],[NCl:ClId|NewClauses]):-
	clause_key(H,ClId), 
	rewrite_source_clause(H,B,ClId,NCl),
	rewrite_source_all_clauses(Clauses,NewClauses).

:- pred rewrite_source_clause(H,B,ClId,Clause)
	: callable * body * clid * var => clause(Clause)
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
rewrite_source_body(A,Clid,N,N1,AK):-
	type_of_goal(metapred(_Type,Meta),A), !,
	functor(A,F,Args),
	functor(B,F,Args),
	rewrite_source_atom(B,Clid,N,N0,AK),
	meta_calls(A,0,Args,Meta,Clid,N0,N1,B).
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
meta_calls(A,Arg0,Args,Meta,Clid,N0,N,B):-
	Arg1 is Arg0+1,
	arg(Arg1,A,A1),
	arg(Arg1,Meta,M1),
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

% THIS_ONE_NEEDS_REVISION
:- doc(hide,inverse_rewrite_source_program/2).

inverse_rewrite_source_program([],[]) :- !.
inverse_rewrite_source_program([Cl:_|Cls],[NCl|NCls]):-
	!,
	inverse_rewrite_source_program(Cl,NCl),
	inverse_rewrite_source_program(Cls,NCls).
inverse_rewrite_source_program(directive(D),directive(D)) :- !.
inverse_rewrite_source_program(clause(H,true),clause(H,true)):- !.
inverse_rewrite_source_program(clause(H,!),clause(H,!)):- !.
inverse_rewrite_source_program(clause(H,B),clause(H,NB)):-
	!,
	inverse_rewrite_source_body(B,NB).
	
inverse_rewrite_source_body((A,B),(NA,NB)):-
	!,
	inverse_rewrite_source_body(A,NA),
	inverse_rewrite_source_body(B,NB).
%% inverse_rewrite_source_body((A&B),(NA&NB)):-
%% 	inverse_rewrite_source_body(A,NA),
%% 	inverse_rewrite_source_body(B,NB).
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
inverse_rewrite_source_body('hiord_rt:call'($(A,_,_)):_,A):- !.
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

:- pred is_atomid(Id):: atom 

# "Succeeds if @var{Id} is a valid atom identifier. The predicate name
  must not contain '/' characters.".

is_atomid(Id):-
	atom2data(Id,_N,_A,_C,_G).

:- pred is_clid(Id):: atom

# "Succeeds if @var{Id} is a valid clause identifier. The predicate
  name must not contain '/' characters.".

is_clid(Id):-
	\+ atom2data(Id,_,_,_,_),
	clid2data(Id,_N,_A,_C).
	
:- pred clid_of_atomid(+AtomId,-ClId):: atom * atom

# "Returns in @var{ClId} the clause identifier corresponding to
  @var{AtomId} goal identifier.".

clid_of_atomid(GoalKey,ClauseKey):-
	atom_codes(GoalKey,Gs),
	append(Cs,[0'/|GoalId],Gs),
	number_codes(_,GoalId),
	atom_codes(ClauseKey,Cs).


:- pred term2atom(+Term,-Atom):: term * atom

# "Returns in @var{Atom} the representation of @var{Term} as it was
  written using @tt{format(\"~w\",[Term])}.".

:- use_module(library(format), [format/3]).
:- use_module(library(system), [mktemp/2, delete_file/1]).
:- use_module(library(file_utils), [file_to_string/2]).
term2atom(Term,Atom):-
	mktemp('/tmp/term2atomXXXXXX',File),
	open(File,write,S1),
	format(S1,"~w",[Term]),
	close(S1),
	file_to_string(File,String),
	delete_file(File),
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
%% first_key((Left &_Right),Key):-
%% 	first_key(Left,Key).
%% first_key((Left \&_Right),Key):-
%% 	first_key(Left,Key).
first_key(_:noinfo,_):-!,
	fail.
first_key((_:Key),Key).

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

:- doc(hide,rewrite_cls/2).

rewrite_cls([clid(Cl,_,_)|Cls0],[Cl|Cls]):-
	rewrite_cls(Cls0,Cls).
rewrite_cls([],[]).

%-------------------------------------------------------------------%
% next two predicates are necessary to get the information not      %
% directly available stored in the keys for goals and clauses       % 
%-------------------------------------------------------------------%

:- pred clid2data(ClId,F,A,C) :: clid * atom * num * num
	# "@var{ClId} identifies the @var{C}th clause of predicate
	  @var{F}/@var{A}.".

clid2data(Clid,N,A,C):-
	name(Clid,List),
	first_datum(List,Head1,Tail),
	first_datum(Tail,Head2,Tail2),
	append(Tail2,[47,45],List2),
	first_datum(List2,Head3,Tail3),
	list2data(Head1,Head2,Head3,Tail3,LN,LA,LC,[45]),
	name(N,LN),
	name(A,LA),
	name(C,LC).

:- pred atom2data(AtId,F,A,C,G) :: bodykey * atom * num * num * num
	# "@var{AtId} identifies the @var{G}th literal of the body of the
	  @var{C}th clause of predicate @var{F}/@var{A}.".

atom2data(Atom,N,A,C,G):-
	name(Atom,List),
	first_datum(List,Head1,Tail1),
	first_datum(Tail1,Head2,Tail2),
	first_datum(Tail2,Head3,Tail3),
	list2data(Head1,Head2,Head3,Tail3,LN,LA,LC,LG),
	name(N,LN),
	name(A,LA),
	name(C,LC),
	name(G,LG).

list2data(Head1,Head2,Head3,Tail3,LN,LA,LC,LG):-
	(first_datum(Tail3,Head4,Tail4)->
	    append(Head1,[47|Head2],Head),
	    list2data(Head,Head3,Head4,Tail4,LN,LA,LC,LG)
	;
	    LN = Head1,
	    LA = Head2,
	    LC = Head3,
	    LG = Tail3).

first_datum([47|L],[],L):-!.
first_datum([X|L],[X|Y],Z):-
	first_datum(L,Y,Z).

:- pred pred2data(PredId,F,A) :: predid * atom * num
	# "@var{PredId} identifies the predicate @var{F}/@var{A}.".

pred2data(Atom,N,A):-
	name(Atom,List),
	first_datum(List,PredList,ArityList),
	name(N,PredList),
	name(A,ArityList).

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


%----------------------------------------------------------------------------
:- doc(version_maintenance,dir('../version/')).

:- doc(version(1*0+732,2004/10/11,22:57*30+'CEST'), "first_key/2
   takes into account cuts without keys (just ignore them).  (Pedro
   Lopez Garcia)").

:- doc(version(1*0+422,2004/04/08,00:12*49+'CEST'), "Added
   exported predicate @pred{orig_clause_id/2}.  (German Puebla)").

:- doc(version(1*0+408,2004/04/04,15:46*08+'CEST'), "Added
   handling of @tt{bagof}, @tt{setof} and @tt{findall} to
   @pred{inverse_rewrite_source_program/2}.  (German Puebla)").

:- doc(version(1*0+405,2004/03/31,20:17*43+'CEST'),
   "rewrite_source_body does not rewrite atoms which are already of
   the form A:K.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+385,2004/03/25,17:17*46+'CET'), "Extended
   @pred{inverse_rewrite_source_program/2} to handle meta-predicates
   correctly.  (German Puebla)").

:- doc(version(1*0+180,2003/12/29,16:36*31+'CET'), "Added
   clause_number/2.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+176,2003/12/29,14:03*26+'CET'), "Added exported
   predicate @pred{rewrite_source_all_clauses/2} which is handy when
   rewrite_source_clause/4 has to be applied to a list of clauses.
   (German Puebla)").

:- doc(version(1*0+93,2003/09/20,19:25*12+'CEST'), "Key of a meta-call
   is now ahead of the keys of its meta-terms in rewrite_source_body
   (Francisco Bueno Carrillo)").

%----------------------------------------------------------------------------
