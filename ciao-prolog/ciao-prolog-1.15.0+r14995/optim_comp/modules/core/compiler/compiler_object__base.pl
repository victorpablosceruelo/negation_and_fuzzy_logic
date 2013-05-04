% This is the base class for all objects
:- class(compiler_object__base, [], [pure]).
:- use_module(compiler(compiler_object__rt)).
:- use_package(compiler(complang_mini)).

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).
:- use_module(engine(internals), 
	['$asserta_root'/3,
	 '$assertz_root'/3,
	 '$erase_nb_root'/3,
	 '$current_nb_root'/3]).

:- use_module(engine(rt_exp), ['$oo_class_i'/3]).
:- use_module(engine(rt_exp), ['$oo_class_v'/3]).

% TODO: optimize using low level stuff, like special instructions, etc.
% TODO: use low level encoding
:- public '\6\self_attr_unify'/2.
:- '$props'('\6\self_attr_unify'/2, [impnat=cbool(prolog_ooget)]).

% TODO: never use it nor setarg/3... define and use mutables instead
%:- public set/2.
%set(U, V) :-
%	'$self'(This), '$oo_class_v'(This, U, I2), '$setarg'(I2, This, V, on).

% Add method  
:- public add/1.
add(U) :-
	'$self'(This), '$oo_class_i'(This, U, I2), arg(I2, This, Root),
	'$assertz_root'(U, 'basiccontrol:true', Root).

% Adda method  
:- public adda/1.
adda(U) :-
	'$self'(This), '$oo_class_i'(This, U, I2), arg(I2, This, Root),
	'$asserta_root'(U, 'basiccontrol:true', Root).

% Get method (multidet)
:- public get__/1.
get__(U) :-
	'$self'(This), '$oo_class_i'(This, U, I2), arg(I2, This, Root),
	'$current_nb_root'(U, 'basiccontrol:true', Root).

% Del method  
:- public del/1.
del(U) :-
	'$self'(This), '$oo_class_i'(This, U, I2), arg(I2, This, Root),
	( '$erase_nb_root'(U, 'basiccontrol:true', Root),
	  fail
	; true
	).

% ---------------------------------------------------------------------------
% (Other methods)

% Add1 method (add without repeating solutions)
% TODO: replace by set/1?
:- public add1/1.
add1(X) :-
	( get__(X) -> true ; add(X) ).

