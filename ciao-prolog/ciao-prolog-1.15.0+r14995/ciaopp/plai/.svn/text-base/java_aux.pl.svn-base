:- module(java_aux,
	[ 
	  basic_type/1,
	  null_type/1,
	  blt/4,
 	  special_builtin/2,
% 	  % primitives
	  object/2,
	  variable/2,
	  null/2,
	  atom/2,
	  compose_array_type/2,
% 	  % for resource analysis
	  eq_size/1,
	  eq_int/1,
	  asg_int/1,
	  asg_size/1
	  ],
	[assertions,regtypes,basicmodes]).

:- use_module(library(messages)).

:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.
%% Only for debugging purposes, uncomment these lines.
% issue_debug_messages(java_cha).
% issue_debug_messages(java_nullity).


% primitives 
object(X,K_X):-
	var(X),
	\+ basic_type(K_X).
variable(X,K_X):-
	var(X),
	basic_type(K_X).
null(X,K_X):-
	X == 'null',
	null_type(K_X).
atom(X,K_X):-
	\+ var(X),
	\+ null(X,K_X).

basic_type(T):- 
	nonvar(T),
 	member(T,[void,boolean,byte,char,double,float,int,long,short]).
null_type(T):-
	T == null_type.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% blt/1 provides an abstraction layer for the builtins to be used for 
% java analyses.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
blt(Key,SgKey,Pred,CondVars):-
	remove_mod_from_sgkey(SgKey,_,SgKey0),
	remove_mod_from_pred(Pred,_,Pred0),!,
	blt_(Key,SgKey0,Pred0,CondVars).
blt_(asg,'lang.Builtin.asg.2#0/4',
	'lang.Builtin.asg.2#0'(L,K_L,R,K_R),
	 [L,K_L,R,K_R]).
blt_(asg,'lang.Builtin.asg_size.4#0/4',
	'lang.Builtin.asg_size.4#0'(L,K_L,R,K_R),
	[L,K_L,R,K_R]).
blt_(asg,'lang.Builtin.asg_int.3#0/4',
  	'lang.Builtin.asg_int.3#0'(L,K_L,R,K_R),
	[L,K_L,R,K_R]).

blt_(new,'lang.Builtin.newb.21#0/2',
	'lang.Builtin.newb.21#0'(O,K_O), [O,K_O]).

blt_(newa,'lang.Builtin.newa.20#0/3',
	  'lang.Builtin.newa.20#0'(O,K_O,Dim), [O,K_O,Dim]).

blt_(ge,'lang.Builtin.ge.10#0/5'  ,
       'lang.Builtin.ge.10#0'(_,L,K_L,R,K_R),
       [L,K_L,R,K_R]).

blt_(gt,'lang.Builtin.gt.11#0/5',
       'lang.Builtin.gt.11#0'(_,L,K_L,R,K_R),
       [L,K_L,R,K_R]).

blt_(lt,'lang.Builtin.lt.15#0/5'  ,
       'lang.Builtin.lt.15#0'(_,L,K_L,R,K_R),
       [L,K_L,R,K_R]).

blt_(le,'lang.Builtin.le.14#0/5',
       'lang.Builtin.le.14#0'(_,L,K_L,R,K_R),
       [L,K_L,R,K_R]).

blt_(eq,'lang.Builtin.eq.7#0/5',
       'lang.Builtin.eq.7#0'(_,L,K_L,R,K_R),
       [L,K_L,R,K_R]).

blt_(eq,'lang.Builtin.eq_size.9#0/5',
       'lang.Builtin.eq_size.9#0'(_,L,K_L,R,K_R),
       [L,K_L,R,K_R]).

blt_(eq,'lang.Builtin.eq_int.8#0/5',
       'lang.Builtin.eq_int.8#0'(_,L,K_L,R,K_R),
       [L,K_L,R,K_R]).	

blt_(ne,'lang.Builtin.ne.17#0/5',
       'lang.Builtin.ne.17#0'(_, L,K_L,R,K_R),
       [L,K_L,R,K_R]).

blt_(ne,'lang.Builtin.ne_size.19#0/5',
       'lang.Builtin.ne_size.19#0'(_, L,K_L,R,K_R),
       [L,K_L,R,K_R]).

blt_(ne,'lang.Builtin.ne_int.18#0/5',
       'lang.Builtin.ne_int.18#0'(_, L,K_L,R,K_R),
       [L,K_L,R,K_R]).

blt_(sub,'lang.Builtin.sub.27#0/6',
	'lang.Builtin.sub.27#0'(X,K_X,Y,K_Y,Z,K_Z),
	[X,K_X,Y,K_Y,Z,K_Z]).

blt_(add,'lang.Builtin.add.1#0/6',
        'lang.Builtin.add.1#0'(X,K_X,Y,K_Y,Z,K_Z),
	[X,K_X,Y,K_Y,Z,K_Z]).

blt_(div,'lang.Builtin.div.6#0/6',
	'lang.Builtin.div.6#0'(X,K_X,Y,K_Y,Z,K_Z),
	[X,K_X,Y,K_Y,Z,K_Z]).

blt_(mul,'lang.Builtin.mul.16#0/6',
	'lang.Builtin.mul.16#0'(X,K_X,Y,K_Y,Z,K_Z),
	[X,K_X,Y,K_Y,Z,K_Z]).

blt_(rem,'lang.Builtin.rem.22#0/6',
	'lang.Builtin.rem.22#0'(X,K_X,Y,K_Y,Z,K_Z),
	[X,K_X,Y,K_Y,Z,K_Z]).

blt_(stf,'lang.Builtin.stf.26#0/7',
        'lang.Builtin.stf.26#0'(A,B,C,D,E,F,G),
	[A,B,C,D,E,F,G]).

blt_(sta,'lang.Builtin.sta.25#0/5',
	 'lang.Builtin.sta.25#0'(A,B,C,D,E), 
	 [A,B,C,D,E]).

blt_(gtf,'lang.Builtin.gtf.13#0/6' ,
	'lang.Builtin.gtf.13#0'(A,B,C,D,E,F),
	 [A,B,C,D,E,F]).

blt_(gta,'lang.Builtin.gta.12#0/5',
	 'lang.Builtin.gta.12#0'(A,B,C,D,E),
 	 [A,B,C,D,E]).

blt_(unk,'lang.Builtin.unk.28#0/2',
	'lang.Builtin.unk.28#0'(_,_),_).


compose_array_type(Type,Array_Type):-
	atm(Type),!,
	atom_concat(Type,'[]',Array_Type).
compose_array_type(Type,_):-
	error_message("~q must be atm to be used in the type of an array",[Type]),
	!,fail.


eq_size( 'lang.Builtin.eq_size.9#0'/5 ).
eq_int(  'lang.Builtin.eq_int.8#0'/5   ).
asg_int( 'lang.Builtin.asg_int.3#0'/4 ).
asg_size('lang.Builtin.asg_size.4#0'/4 ).

special_builtin(object_init,SgKey):-
	remove_mod_from_sgkey(SgKey,_,SgKey0),
	SgKey0 == 'java.lang.Object.<init>.0#0/2',!.

/*
:- use_module(library(lists), [sublist/2]).

contains(Atom,SubAtom):-
	atom_codes(Atom,List1),
	atom_codes(SubAtom,List2),!,
	sublist(List2,List1).
*/
	
remove_mod_from_sgkey(SgKey,_Mod,_Pred):-
	var(SgKey),!.
remove_mod_from_sgkey(SgKey,Mod,Pred):-
	nonvar(SgKey),
	atom_concat(X,Pred,SgKey),
	atom_concat(Mod,':',X).
remove_mod_from_sgkey(Key,_Mod,Key).

remove_mod_from_pred(Pred,_Mod,_Pred0):-
	var(Pred),!.
remove_mod_from_pred(Pred,Mod,Pred0):-
	nonvar(Pred),
	Pred =..[F|Args],	
	atom_concat(X,F0,F),
	atom_concat(Mod,':',X),
	Pred0 =..[F0|Args].
remove_mod_from_pred(Pred,_Mod,Pred).
