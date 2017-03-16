:- module(unexpand_,
	[ module_spec/2,
	  transform_clause_list/3, 
	  transform_head/3, transform_body/3,
	  transform_assrt_body/3,
	  unexpand_meta_calls/2,
	  transform_name/3,
	  generate_unexpanded_data/1, % TODO: kludge?
	  clean_unexpanded_data/0     % TODO: kludge?
	],
	[ assertions
	]).

:- use_package( .('../api/ciaopp_api_') ).

% :- use_module(engine(internals),[term_to_meta/2]).

:- use_module(library(idlists), [subtract/3]).
:- use_module(library(lists), [append/3]).

:- use_module(assrt_db_, [assertion_body/7]).
:- use_module(itf_db_, [curr_file/2]).
:- use_module(p_unit_, [type_of_goal/2]).

% -------------------------------------------------------------------------

% Done with set_ciaopp_expansion(true)
%% :- doc(bug,"Maybe it all would be easier if we changed the meta-pred
%% 	expansion...").
%% :- doc(bug,"Heads of exports and goals of imports which are meta-preds
%% 	with addmodule must be un-expanded.").
:- doc(bug,"Having set_ciaopp_expansion(true), unexpand_meta_calls should not
	be needed, except for 'hiord_rt:call' instead of 'call'.").

% -------------------------------------------------------------------------

transform_clause_list( [] , _M , [] ).
transform_clause_list( [clause( H , B ) | C ] ,M, [clause( HT , BT ) | CR] ) :-
	transform_clause( H, B, M, HT, BT),
	transform_clause_list( C , M , CR ).

transform_clause( H, B, M, H1, B1):-
	transform_head( H , M, H1 ),
	unexpand_meta_calls( B , BT ),
	transform_body( BT , M, B1 ),
	!.
transform_clause( H, B, _, H, B):-
	throw(unexpansion_fails(H,B)).

%transform_body( true , true ).
transform_body( ','(A,B), M, ','(AT,BT) ) :-
	!,
	transform_body( A, M, AT ),
	transform_body( B, M, BT ).
% DTM: This is necesary for assertion body disjunction 
transform_body( Disj, M, DO ) :-
	Disj = ';'(A,_),
	list(A),
	!,
	transform_body_disj( Disj , M , DO ).
transform_body( ';'(A,B), M, ';'(AT,BT) ) :-
	!,
	transform_body( A, M, AT ),
	transform_body( B, M, BT ).
transform_body( H , M, HT ) :-
	transform_name( H , M, HT ).
%transform_body( A , A ).


transform_body_disj( (A;B) , M , (AT;BT) ) :-
	!,
	transform_body_disj( A , M , AT ),
	transform_body_disj( B , M , BT ).
transform_body_disj( A , M , AT ) :-
	list2conj( A , AC ),
	transform_body( AC, M, ATM ),
	list2conj( AT , ATM ).


transform_name( H , user(_), HT ) :-
	transform_name( H , user , HT ),!.
transform_name( H , M, HT ) :-
	H =.. [ F | A ],
	functor( H , F , N ),
	transform_atom( F , N , M , FT ),
	( type_of_goal(metapred(_Type,Meta),H)
	-> Meta =.. [ _ | Metaterms ],
	   transform_terms( A, Metaterms, M, A0 )
	 ; A0 = A
	),
	reconstruct( FT , A0 , HT ).

reconstruct( M:FT , A , M:HT ):- !,
	HT =.. [ FT | A ].
reconstruct( FT , A , HT ):-
	HT =.. [ FT | A ].

transform_terms( [], [], _M, [] ).
transform_terms( [A0|As0], [T|Ts], M, [A|As] ):-
	T \== ?,
	nonvar(A0), !,
	% JF: very dirty hack -- add extra arguments to unexpand the predicate properly...
	( T = pred(ExtraArity) ->
	    add_extra_args(ExtraArity, A0, A1)
	; A1 = A0
	),
	transform_body(A1, M, A2),
	( T = pred(ExtraArity) ->
	    remove_extra_args(ExtraArity, A2, A)
	; A = A2
	),
	transform_terms( As0, Ts, M, As ).
transform_terms( [A|As0], [_|Ms], M, [A|As] ):-
	transform_terms( As0, Ms, M, As ).

% ---------------------------------------------------------------------------
:- use_module(library(lists)).
add_extra_args(N, A0, A) :-
	A0 =.. [F|As0],
	length(Extra, N),
	append(As0, Extra, As), !,
	A =.. [F|As].

remove_extra_args(N, M:A0, M:A) :- !,
	remove_extra_args(N, A0, A).
remove_extra_args(N, A0, A) :-
	A0 =.. [F|As0],
	length(Extra, N),
	append(As, Extra, As0), !,
	A =.. [F|As].
% ---------------------------------------------------------------------------

transform_head( H , _M, HT ) :- % any M (could be multifile:)
	H =.. [ F | A ],
	atom_codes( F, Codes ),
	append( _MCodes, ":"||FTCodes, Codes ), !,
	atom_codes( FT, FTCodes ),
	HT =.. [ FT | A ].
transform_head( H , _M, H ).

% Here we can translate from 'module:clause' to 'clause'
/* 
transform_atom( F , _A , ModuleName , CT ) :-
	atom( F ),
	atom_concat( ModuleName , ':' , Module ),
	atom_concat( Module , CT , F ),
	!.
% TOMA YA!!!!!
transform_atom( '=:=' , 2 , _ModuleName , '=:=' ):- !.
transform_atom( F , A , _ModuleName , M:CT ):-
	atom( F ),
	atom_codes(F,Codes),
	append(MCodes,":"||CTCodes,Codes), !,
	atom_codes(Q,MCodes),
	transform_reexported_atom(F , A , Q , M ),
	atom_codes(CT,CTCodes).
transform_atom( F , _A , _ModuleName , F ).
*/

transform_atom( F , A , _ModuleName , MCT ):-
	atom( F ),
	atom_codes(F,Codes),
	append(MCodes,":"||CTCodes,Codes), !,
	atom_codes(Q,MCodes),
	transform_reexported_atom(F , A , Q , M ),
	atom_codes(CT,CTCodes),
	simplify_qualify(M:CT, A, MCT). % JF
transform_atom( F , _A , _ModuleName , F ).

% ---------------------------------------------------------------------------
% JF {

:- pred unexpanded_import(Name, Arity, Module).
:- data unexpanded_import/3.
:- pred unexpanded_defines(Name, Arity).
:- data unexpanded_defines/2.

:- use_module(itf_db_, [current_itf/3]).
:- use_module(library(filenames), [no_path_file_name/2]).


%% DTM: To support user files
generate_unexpanded_data( user(File) ) :-
	!,
	no_path_file_name( File , Module ),
	generate_unexpanded_data( Module ).
generate_unexpanded_data(Module) :-
        % Generate inverse table for defines 
	current_itf(defines, MF, A),
	  atom_concat(Module, F0, MF),
	  atom_concat(':', F, F0),
	  assertz_fact(unexpanded_defines(F, A)),
	  fail.
generate_unexpanded_data(_) :-
        % Generate inverse table for imports
%	current_itf(imports, Goal, ISpec),
	type_of_goal(imported(ISpec), Goal),
	  module_spec(ISpec, IM),
	  functor(Goal, MF, A),
	  atom_concat(IM, F0, MF),
	  atom_concat(':', F, F0),
	  assertz_fact(unexpanded_import(F, A, IM)),
	  fail.
generate_unexpanded_data(_).

clean_unexpanded_data :-
	retractall_fact(unexpanded_defines(_, _)),
	retractall_fact(unexpanded_import(_, _, _)).

simplify_qualify(M:F, A, F) :-
	superfluous_qualify(M, F, A), !.
simplify_qualify(MF, _, MF).

superfluous_qualify( M , F, A) :-
	( unexpanded_defines(F, A) ->
	    % defined in module and not imported from any module
	    \+ unexpanded_import(F, A, _)
	; % (not defined in module and) imported only from one module
          unexpanded_import(F, A, M),
	  \+ ( unexpanded_import(F, A, M2), M2 \== M )
	).

% JF }
% ---------------------------------------------------------------------------


transform_reexported_atom(F , A , Q , M ):-
	functor(Goal,F,A),
	type_of_goal(imported(Import),Goal),
	% Q is the one from which it is effectively imported
	module_spec(Import,Q),
	!,
	M=Q.
transform_reexported_atom(F , A , _Q , M ):-
	functor(Goal,F,A),
	% it is probably reexported, take anyone:
	type_of_goal(imported(Import),Goal),
	!,
	module_spec(Import,M).
transform_reexported_atom(_F , _A , M , M ).

module_spec(Spec,M):-
	functor(Spec,_,1), !,
	arg(1,Spec,File),
	quitar_slash_hasta_ultimo(File,M).
module_spec(Spec,M):-
	atom(Spec),
	quitar_slash_hasta_ultimo(Spec,M).

quitar_slash_hasta_ultimo(Spec,M):-
	atom_codes(Spec,Codes),
	last_datum(Codes,T,T,Datum),
	atom_codes(M,Datum).

last_datum([],L,[],L).
last_datum([47|L],_,_,New):- !,
	last_datum(L,T,T,New).
last_datum([X|L],Old,Tail,New):- !,
	Tail=[X|NTail],
	last_datum(L,Old,NTail,New).

% -------------------------------------------------------------------------

transform_assrt_body( Body , M , BodyT ) :-
	assertion_body(Pred,Compat,Call,Succ,Comp,Comm,Body),
	transform_head(Pred,M,PredT),
	transform_assrt_field(Compat,M,CompatT),
	transform_assrt_field(Call,M,CallT),
	transform_assrt_field(Succ,M,SuccT),
	transform_assrt_field(Comp,M,CompT),
	assertion_body(PredT,CompatT,CallT,SuccT,CompT,Comm,BodyT).

transform_assrt_field([],_M,[]):- !.
transform_assrt_field([G|Gs],M,[GT|GTs]):- !,
	transform_assrt_field(G,M,GT),
	transform_assrt_field(Gs,M,GTs).
transform_assrt_field(G,M,GT):-
	unexpand_meta_calls(G,G1),
	transform_body(G1,M,GT).

% -------------------------------------------------------------------------

% %% --- is this necessary? (ciaopp_expansion)
% unexpand_meta_calls((rt_module_exp(T0,_,_,_,_,T),A0),A):- !,
% 	unexpand_meta_term(T0,T),
% 	unexpand_meta_calls(A0,A).
% %% --- is this necessary? (ciaopp_expansion)
% unexpand_meta_calls(rt_module_exp(T0,_,_,_,_,T),true):- !,
% 	unexpand_meta_term(T0,T).
unexpand_meta_calls((A0,B0),(A,B)):- !,
	unexpand_meta_calls(A0,A),
	unexpand_meta_calls(B0,B).
unexpand_meta_calls('hiord_rt:call'(A,''(X)),call(A,X)):- !.
unexpand_meta_calls(\+(X),\+(X1)):- !,
	unexpand_primitive_meta_term(X,X1).
unexpand_meta_calls(if(X,Y,Z),if(X1,Y1,Z1)):- !,
	unexpand_primitive_meta_term(X,X1),
	unexpand_primitive_meta_term(Y,Y1),
	unexpand_primitive_meta_term(Z,Z1).
unexpand_meta_calls(A0,A):-
% not enough info in itf_db (for the imported metapreds)
	type_of_goal(metapred(_Type,Meta),A0), !,
	functor(A0,F,N),
	functor(A,F,N),
	unexpand_meta_terms(A0,N,Meta,A).
unexpand_meta_calls(A,A).

unexpand_primitive_meta_term(X,X1):- var(X), !, X1=X.
unexpand_primitive_meta_term(call(X),X1):- !, X1=X.
unexpand_primitive_meta_term(X,X1):-
	unexpand_meta_term(X,X1).

unexpand_meta_terms(_,0,_Meta,_).
unexpand_meta_terms(A0,N,Meta,A):-
	N > 0,
	arg(N,A0,T0),
	arg(N,A,T),
	unexpand_meta_term(T0,T),
	N1 is N-1,
	unexpand_meta_terms(A0,N1,Meta,A).

unexpand_meta_term(T0,T):-
	nonvar(T0), !,
	( meta_term_abstraction(T0,T2) -> true
	; T2=T0
	),
	unexpand_meta_calls_in_term(T2,T).
unexpand_meta_term(T,T).

unexpand_meta_calls_in_term(A0,A):-
	var(A0), !,
	A = A0.
unexpand_meta_calls_in_term((A0,B0),(A,B)):- !,
	unexpand_meta_calls_in_term(A0,A),
	unexpand_meta_calls_in_term(B0,B).
unexpand_meta_calls_in_term(A0,A):-
	unexpand_meta_calls(A0,A).

meta_term_abstraction('PA'(_Term,Abs,Call),Prop):-
	Call=..[F|Args],
	Abs=..[''|NonArgs],
	subtract(Args,NonArgs,PropArgs),
	Prop=..[F|PropArgs].
meta_term_abstraction('$'(Term,_Call,_Meta),Term).

% -------------------------------------------------------------------------
:- doc(version_maintenance,dir('../version/')).

:- doc(version(1*0+618,2004/09/10,19:09*45+'CEST'), "Added 1
   clause to predicate @pred{generate_unexpanded_data/1} which control
   the user(_) module load.  (David Trallero Mena)").

:- doc(version(1*0+596,2004/07/30,21:38*38+'CEST'), "Added code in
   @pred{transform_body/2} necessary to treat (A;B) (that appears in
   assertions body).  (David Trallero Mena)").

:- doc(version(1*0+501,2004/07/02,13:28*11+'CEST'), "Modifications
   when printing assertions. Now they are well printed (without ' in
   names) (David Trallero Mena)").

:- doc(version(1*0+187,2003/12/29,19:32*13+'CET'), "Added T \== ?
   in transform_terms: not correct, though complete (Francisco Bueno
   Carrillo)").

:- doc(version(1*0+134,2003/10/15,04:43*34+'CEST'), "Changed call
   to 'hiord_rt:call'.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+87,2003/09/18,12:31*34+'CEST'), "export
   transform_name/3, necesary for writting types correctly (David Trallero
   Mena)").

:- doc(version(1*0+83,2003/09/15,11:51*12+'CEST'), "Added
   unexpand_terms.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+82,2003/09/15,11:19*35+'CEST'), "Added '$'/3
   meta-term type and primitive mea-preds to the unexpansion.
   (Francisco Bueno Carrillo)").

:- doc(version(0*1+1,2003/08/27,18:01*46+'CEST'), "Added
   unexpand_meta_calls_in_term/2.  (Francisco Bueno Carrillo)").

:- doc(version(0*1+0,2003/08/06,17:41*51+'CEST'), "unexpand_meta_calls
   modified for unexpanding metacalls in output. Example:call( $(A,_,_) )
   is written now like call(A).  (David Trallero Mena)").

% -------------------------------------------------------------------------
