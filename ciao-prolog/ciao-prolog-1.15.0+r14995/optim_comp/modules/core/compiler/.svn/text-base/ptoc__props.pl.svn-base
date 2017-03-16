:- module(_, [], [compiler(complang)]).

:- use_module(library(lists)).
:- use_module(compiler(errlog)).
% TODO: module_exp and this module have a lot of common things... the problem is that properties are used in compiler/frontend; maybe compiler/frontend should not care about properties and just try to do some preprocessing on them... (checking errors, etc; considering each property as a individual item? or not?)
:- use_module(.(module_exp)). % TODO: should this dependence be here?
:- use_module(.(sht_analyzer)). % (for shtdef)

%-----------------------------------------------------------------------------

% TODO: clean!
% TODO: in some cases 'register' property should not be necessary... (i.e. not exported, not called from bytecode and not hiord calls or metaexpansions -> that is already detected by reg_analysis)
{
:- fluid props :: accum.
:- public expand_impnat_prop/2.
expand_impnat_prop(cboolforeign(CName, Desc), Def) :- !,
	Def = implicit_cbool(CName),
	props.add(register=true),
	props.add(impforeign=Desc).
expand_impnat_prop(cbool(CName), Def) :- !,
	Def = implicit_cbool(CName),
	props.add(register=true).
expand_impnat_prop(cinsnp(CName), Def) :- !,
	Def = implicit_cinsnp(CName),
	props.add(register=true).
expand_impnat_prop(cfun(CName, UsesHeap), Def) :- !,
	Def = func(CName, UsesHeap).
expand_impnat_prop(cfunre(CName, UsesHeap), Def) :- !,
	Def = funcre(CName, UsesHeap).
expand_impnat_prop(cblt(CName, MemUsage), Def) :- !,
	Def = builtin(CName, MemUsage).
expand_impnat_prop(ptoc_macro(Code), Def) :- !,
	% TODO: is this Def correct???
	Def = ptoc_macro,
        props.add(impmacro = Code).
expand_impnat_prop(ptoc_builtin, Def) :- !,
	Def = ptoc_builtin.
expand_impnat_prop(cswitchcfun(Xs), Def) :- !,
	Def = cswitchcfun,
        props.add(defswitchcfun = Xs).
expand_impnat_prop(ptoc, Def) :- !,
	Def = ptoc.
expand_impnat_prop(bytecode, Def) :- !,
	Def = bytecode.
expand_impnat_prop(indefinable, Def) :- !, % TODO: properties that cannot be defined (e.g. iso/1)
	Def = unknown.
expand_impnat_prop(intrinsic, Def) :- !, % in standard compiler terminology, a procedure whose implementation is handled specially by the compiler
	Def = unknown.
}.

%-----------------------------------------------------------------------------

% pub_props(DefType, Meta, Visibility, IsProp, Context)
%   DefType: unknown, static, dynamic, data, concurrent, ...
%   Meta: meta_predicate declaration, or 0 if it has not
%   Visibility: vs_public, vs_private, vs_multifile
%   Context: context info

:- public pub_props__default/1.
pub_props__default := pub_props(unknown, 0, vs_private, false, none).

:- public pub_props__set_def/3.
pub_props__set_def(PubProps0, Def) := PubProps :-
	PubProps0 = pub_props(_, Meta, Visibility, IsProp, Context),
	PubProps = pub_props(Def, Meta, Visibility, IsProp, Context).

:- public pub_props__set_visibility/3.
pub_props__set_visibility(PubProps0, Visibility) := PubProps :-
	PubProps0 = pub_props(Def, Meta, _, IsProp, Context),
	PubProps = pub_props(Def, Meta, Visibility, IsProp, Context).

{
:- fluid exp :: module_exp.
:- public norm_prop/3.
% TODO: emit errors if properties are wrong
norm_prop(sht_usermemo, Value0, Value) :- !,
        ( Value0 = shtdef(CT, ET) -> % (note: syntax at the source)
	    true
	; Value0 = ~shtdef.new(CT, ET) -> % (already normalized - for internally defined assertions; avoid it)
	    true
	; fail % TODO: emit error
	),
	Value = ~shtdef.new_norm(CT, ET).
norm_prop(_, Value, Value).

:- public prop_merge/3.
% TODO: there may be problems in the future... this is used to store properties before and after module expansion
% replace properties in As of list in Bs
prop_merge([A = V0|Xs], As0) := [A = V|As] :-
	norm_prop(A, V0, V1),
	( select((A = OldV), As0, As1) ->
	    V = ~single_prop_merge(A, OldV, V1)
	; As1 = As0,
	  V = V1
	),
	As = ~prop_merge(Xs, As1).
prop_merge([], As) := As.
}.

:- public prop_merge_noexp/3.
% TODO: (before module expansion)
% replace properties in As of list in Bs
prop_merge_noexp([A = V0|Xs], As0) := [A = V|As] :-
	( select((A = OldV), As0, As1) ->
	    V = ~single_prop_merge(A, OldV, V0)
	; As1 = As0,
	  V = V0
	),
	As = ~prop_merge_noexp(Xs, As1).
prop_merge_noexp([], As) := As.

% TODO: write other predicate to ensure that properties are correct and this will not fail, use that predicate in compiler/frontend
single_prop_merge(specialize, OldV, NewV) := V :- !,
	V = ~append(OldV, NewV).
single_prop_merge(_A, _OldV, NewV) := NewV. % overwrite it

% TODO: is this the correct module to put this???

:- public replace_mode/5.
replace_mode([_|Xs], [Mode|Ms], Mode, [Y|Ys]) :=
	[Y|~replace_mode(Xs, Ms, Mode, Ys)] :- !.
replace_mode([X|Xs], [_|Ms], Mode, Ys) :=
	[X|~replace_mode(Xs, Ms, Mode, Ys)] :- !.
replace_mode([], [], _, []) := [] :- !.

:- public filter_mode/4.
filter_mode([X|Xs], [Mode|Ms], Mode) := [X|~filter_mode(Xs, Ms, Mode)] :- !.
filter_mode([_|Xs], [_|Ms], Mode) := ~filter_mode(Xs, Ms, Mode) :- !.
filter_mode([], [], _) := [] :- !.
	
:- public filter_cvar/3.
filter_cvar([], []) := [].
filter_cvar([X|Xs], [cvar|Rs]) := [X|~filter_cvar(Xs, Rs)] :- !.
filter_cvar([X|Xs], [cvar(_)|Rs]) := [X|~filter_cvar(Xs, Rs)] :- !.
filter_cvar([_|Xs], [_|Rs]) := ~filter_cvar(Xs, Rs) :- !.

% ---------------------------------------------------------------------------

{
:- fluid exp :: module_exp.

:- public caller_argmems/2.
caller_argmems(PredId) := Mems :-
	trust(PredId instance_of predicate_x),
	Mems0 = ~PredId.get_prop(argmems),
	Modes = ~PredId.get_prop(argmodes),
	ImpTypes = ~PredId.get_prop(argimptypes),
	Mems = ~argmems_for_imptypes(ImpTypes, Mems0, Modes).

:- public callee_argmems/2.
callee_argmems(PredId) := Mems :-
	trust(PredId instance_of predicate_x),
	Mems0 = ~PredId.get_prop(argmems),
	Modes = ~PredId.get_prop(argmodes),
	ImpTypes = ~PredId.get_prop(argimptypes),
	Mems = ~argmems_for_imptypes(ImpTypes, Mems0, Modes).
}.
	
% tood: argmem lattice top is anymem... DOCUMENT!!
% TODO: sometimes the user wants to overwrite the argmems... is that right? (at least it is needed to specify the default_choice... but I don't like it very much, in the future it should be changed...)

% Selects input and output mems
argmems_for_imptypes(ImpTypes, Mems, Modes) :=
	~argmems_for_imptypes__2(ImpTypes, Mems, Modes, 0, 0).

argmems_for_imptypes__2([], [], [], _, _) := [] :- !.
argmems_for_imptypes__2([IT|ITs], [Mem0|Mems0], [Mode|Modes], I, O) := [Mem|Mems] :-
	( \+ Mem0 = anymem ->
	    Mem = Mem0,
	    I1 = I,
	    O1 = O
	; IT = tagged ->
	    ( Mode = in ->
	        Mem = x(I),
		I1 is I + 1,
		O1 = O
	    ; Mode = out ->
	        Mem = x(O),
		I1 = I,
		O1 is O + 1
	    ; Mode = param ->
	        Mem = cvar,
	        I1 = I,
	        O1 = O
	    )
	; Mem = cvar,
	  I1 = I,
	  O1 = O
	),
	Mems = ~argmems_for_imptypes__2(ITs, Mems0, Modes, I1, O1).

% ---------------------------------------------------------------------------

% TODO: wrong!! X regs may have holes!
:- public maxreg/2.
maxreg(Xs) := J :- J0 = ~maxreg_2(Xs, 0), J is J0 + 1.

maxreg_2([], J) := J :- !.
maxreg_2([x(I)|Xs], J) := ~maxreg_2(Xs, ~maxnum(I, J)) :- !.
maxreg_2([_|Xs], J) := ~maxreg_2(Xs, J) :- !.

maxnum(I, J) := I :- I >= J, !.
maxnum(_, J) := J :- !.



