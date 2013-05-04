:- doc(section, "Internal access to ptoc analyzer").
% TODO: Rename, not only for the compilation to C.
% TODO: Merge with assertions once you are able to enable/disabled
%   them easily and without forcing operators on other program parts
%   (e.g., by enabling/disabling special contexts)

% ---------------------------------------------------------------------------
% Ad-hoc entries for predicates (for analysis)

decl__treatDom('$trust_entry'(_, _, _)).
decl__treat('$trust_entry'(F/A, AbsInt, Lambda)) :- !,
	Module = ~def_envmod,
	trust(Module instance_of module_s),
	Module.do_trust_entry(F, A, AbsInt, Lambda).

% ---------------------------------------------------------------------------
% '$props'/2 - set properties of a predicate

:- use_module(.(ptoc__props)).

decl__treatDom('$props'(_, _)).
decl__treat('$props'(F/A, Props)) :-
	Pred = ~pred_ref_ac(F, A),
	Pred.set_pred_props(Props).

% ---------------------------------------------------------------------------
% '$forceprops'/2 - internal declaration % TODO: document

% TODO: Properties here are at a different level. Here we are forcing
%       some properties on predicates that are not necessarily being
%       defined in this module. This is the same than for assertions.
%       Find a solution that works for assertions too. 
%
%       Maybe, qualified names in assertion head can be used to 
%       force properties. In that way I can still use '$props'/2.

decl__treatDom('$forceprops'(_, _)).
decl__treat('$forceprops'(M:F/A, Props)) :-
	Module = ~def_envmod,
	trust(Module instance_of module_s),
	Module.do_forced_props(M, F, A, Props).

% ---------------------------------------------------------------------------
% ptoc types

% '$ptoc_type'/2 - internal declaration % TODO: document
decl__treatDom('$ptoc_type'(_, _)).
decl__treat('$ptoc_type'(Type, Def)) :- !,
	Module = ~def_envmod,
	trust(Module instance_of module_s),
	Module.add_ptoc_type(Type, Def).

% '$ptoc_typeprop'/3 - internal declaration % TODO: document
decl__treatDom('$ptoc_typeprop'(_, _, _)).
decl__treat('$ptoc_typeprop'(Type, Op, Name)) :- !,
	% TODO: check errors (e.g. defining an ilegal type property)
	Module = ~def_envmod,
	trust(Module instance_of module_s),
	Module.add_ptoc_typeprop(Type, Op, Name).

% '$ptoc_imptype_c'/2 - internal declaration % TODO: document
decl__treatDom('$ptoc_imptype_c'(_, _)).
decl__treat('$ptoc_imptype_c'(Name, Def)) :- !,
	Module = ~def_envmod,
	trust(Module instance_of module_s),
	Module.add_ptoc_imptype_c(Name, Def).

