%%------------------------------------------------------------------------
%%
%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% OBJECT ORIENTED DECLARATION INTERFACING
%%
%% AUTHOR : Angel Fernandez Pineda
%% DATE   : July 1999
%%
%%------------------------------------------------------------------------

:- module(class_itf,
	[
	    attribute_set/4,
	    implementation/5,
	    inherited_pred/5,
	    public_pred/4,
	    inheritable_pred/4,
	    virtual_pred/4,
	    impl_interface/2,
	    cleanup_class_info/1,
	    class_generate_oop_info/1
	], []).

:- use_module(.(class_old_itf)).

%%------------------------------------------------------------------------

:- data current_class/1.       % CLASS/INTERFACE info has been generated.

:- data attribute_set/4.       % CLASS knows that CLASS_OR_ASCENDANT
                               % declares F/A as attribute.

:- data implementation/5.      % CLASS defines/inherits an implementation of
                               % WHAT (method/attribute) at CLASS_OR_ASCENDANT.

:- data inherited_pred/5.      % CLASS declares KIND (method/attribute),
                               % as an inherited predicate from ascendants.

:- data public_pred/4.         % CLASS declares KIND (method/attribute),
                               % as public.

:- data inheritable_pred/4.    % CLASS declares KIND (method/attribute),
                               % as inheritable.

:- data virtual_pred/4.        % CLASS declares KIND (method/attribute),
                               % as virtual.

:- data impl_interface/2.      % CLASS has implemented INTERFACE.

%%------------------------------------------------------------------------

% INITIALIZATION: Must be called before expanding the involved class.

cleanup_class_info(Module) :-
	retractall_fact(attribute_set(Module,_,_,_)),
	retractall_fact(implementation(Module,_,_,_,_)),
	retractall_fact(inherited_pred(Module,_,_,_,_)),
	retractall_fact(public_pred(Module,_,_,_)),
	retractall_fact(virtual_pred(Module,_,_,_)),
	retractall_fact(impl_interface(Module,_)),
	retractall_fact(current_class(Module)).

% INITIALIZATION
class_generate_oop_info(Module) :-
	current_class(Module),   % already generated.
	!.
class_generate_oop_info(Module) :-
	assertz_fact(current_class(Module)),
	fail.
% GENERATE ASCENDANCY AND ABSTRACT INTERFACE INFO
class_generate_oop_info(Module) :-
	assertz_fact(impl_interface(Module,Module)),
	fail.
class_generate_oop_info(Module) :-
	decl_super(Module,Super),
	class_generate_oop_info(Super),
	impl_interface(Super,Itf),
	\+ impl_interface(Module,Itf),
	assertz_fact(impl_interface(Module,Itf)),
	fail.
class_generate_oop_info(Module) :-
	decl_implements(Module, Itf),
	class_generate_oop_info(Itf),
	\+ impl_interface(Module,Itf),
	assertz_fact(impl_interface(Module,Itf)),
	impl_interface(Itf,OtherItf),
	class_generate_oop_info(Itf),
	\+ impl_interface(Module,OtherItf),
	assertz_fact(impl_interface(Module,OtherItf)),
	fail.
% GENERATE ATTRIBUTE SET INFO
class_generate_oop_info(Module) :-
	decl_attribute(Module, F/A),
	assertz_fact(attribute_set(Module,Module,F,A)),
	fail.
class_generate_oop_info(Module) :-
	decl_super(Module,Super),
	attribute_set(Super,At,F,A),
	assertz_fact(attribute_set(Module,At,F,A)),
	fail.
% GENERATE IMPLEMENTATION INFO
% (table that indicates the latest definition of each method or attribute)
class_generate_oop_info(Module) :-
	decl_method(Module, F/A),
	assertz_fact(implementation(Module,method,Module,F,A)),
	fail.
class_generate_oop_info(Module) :-
	decl_attribute(Module, F/A),
	assertz_fact(implementation(Module,attribute,Module,F,A)),
	fail.
class_generate_oop_info(Module) :-
	decl_super(Module,Super),
	implementation(Super,Kind,At,F,A),
	\+ implementation(Module,_,_,F,A),
	assertz_fact(implementation(Module,Kind,At,F,A)),
	fail.
% GENERATE INHERITED PREDICATE INFO
class_generate_oop_info(Module) :-
	decl_super(Module,Super),
	decl_inheritable(Super, F/A),
	( decl_method(Super, F/A) -> Kind = method
	; decl_attribute(Super, F/A) -> Kind = attribute
	),
	assertz_fact(inherited_pred(Module,Kind,Super,F,A)),
	fail.
class_generate_oop_info(Module) :-
	decl_super(Module,Super),
	inherited_pred(Super,Kind,At,F,A),
	\+ inherited_pred(Module,_,_,F,A),
	assertz_fact(inherited_pred(Module,Kind,At,F,A)),
	fail.
% GENERATE PUBLIC PREDICATE INFO
class_generate_oop_info(Module) :-
	impl_interface(Module,Itf),
	decl_public(Itf, F/A),
	( decl_method(Itf, F/A) -> Kind = method
	; decl_attribute(Itf, F/A) -> Kind = attribute 
	; inherited_pred(Module,Kind,_,F,A)
	),
	\+ public_pred(Module,_,F,A),
	assertz_fact(public_pred(Module,Kind,F,A)),
	fail.
% GENERATE INHERITABLE PREDICATE INFO
class_generate_oop_info(Module) :-
	impl_interface(Module,Itf),
	decl_inheritable(Itf, F/A),
	( decl_method(Itf, F/A) -> Kind = method
	; decl_attribute(Itf, F/A) -> Kind = attribute 
	; inherited_pred(Module,Kind,_,F,A)
	),
	\+ public_pred(Module,_,F,A),
	assertz_fact(inheritable_pred(Module,Kind,F,A)),
	fail.
% GENERATE VIRTUAL PREDICATE INFO
class_generate_oop_info(Module) :-
	decl_virtual(Module, F/A),
	( decl_method(Module, F/A) -> Kind = method
	; decl_attribute(Module, F/A) -> Kind = attribute 
	; inherited_pred(Module,Kind,_,F,A)
	),
	assertz_fact(virtual_pred(Module,Kind,F,A)),
	fail.
class_generate_oop_info(_).




