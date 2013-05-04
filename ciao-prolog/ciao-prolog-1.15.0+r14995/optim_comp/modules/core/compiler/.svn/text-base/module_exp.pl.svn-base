:- class(module_exp, [], [compiler(complang)]).

% Class to store the program code and declarations in a optimal way
% for compilation and analysis.
%
% Author: Jose F. Morales
%
% * Important note about 'predid' *
%
%   'predid' (or any key obtained through '$mut__init' cannot be
%   stored in a dic.  The reason is that @> and @< does not work as
%   for that object. The only working comparison predicate is ==.
%   _Any_ implementation of mutables will probably fail here:
%
%     - if based on attributed variables: it fails because the
%       variable (itself) changes.
%
%     - if based on mutable terms (setarg-like): it fails because
%       garbage collection may shuffle addresses.
%
%   If you want to store a mutable variable as a key in a map, table,
%   dictionary, etc. you need to associate a unique identifier to the
%   immutable part of the object, and use that as a key. For
%   library(dict), that value must be unique for the variable. For
%   other kind of tables, it could not be unique (i.e. a hash value)
%   at least if ==/2 is finally used to compare mutable identifiers).

:- include(.(absint__interface)).

:- use_module(compiler(errlog)).
:- use_module(compiler(list_common), [repeat_n/3]).
:- use_module(library(dict)).

:- use_module(.(idet_analyzer)). % Used for some properties
:- use_module(.(sht_analyzer)).
:- use_module(.(ptoc__props)).

:- attr defines_module :: any.
:- attr metadatas :: any.
:- attr pred_name_lookup_dic :: any.

:- public data expanded_decl/3.
:- pred expanded_decl(MF, A, PubProps).

:- public data expanded_import/3.
:- pred expanded_import(MF, A, PubProps).

% only when the predicate is imported via reexports (needed for lpdoc)
:- public data expanded_import_from/3.
:- pred expanded_import_from(MF, A, IM).

% TODO: document...
:- public data expanded_assertion/1.
:- pred expanded_assertion(Assertion).

:- public data native_include_c_source/1.
:- pred native_include_c_source(Spec).
:- public data native_include_c_header/1.
:- pred native_include_c_header(Spec).
:- public data native_inline/1.
:- pred native_inline(Decl).
:- public data priv_props/3.
:- pred priv_props(MF, A, Props).
:- public data ptoc_type/2.
:- pred ptoc_type(Type, Def).
:- public data ptoc_typeprop/3.
:- pred ptoc_typeprop(Type, Op, Name).
:- public data ptoc_imptype_c/2.
:- pred ptoc_imptype_c(Name, Def).
:- public data trust_entry/4.
:- pred trust_entry(MF, A, AbsInt, Lambda).
:- public data pragma/1.
:- pred pragma(Pragma).

:- pred counter/1 # "Counter to name auxiliary predicates".
:- data counter/1.
:- pred subatom_counter/2 # "Counter to name auxiliary predicates with a prefix".
:- data subatom_counter/2.
:- pred subatom_root/2.
:- data subatom_root/2.

:- public data uses_hiord/0.
:- pred uses_hiord/0 # "The module does some high-order calls
   (analysis assumes then that any predicate can be called with
   arbitrary call patterns).".

% TODO: improve detection of what predicates can be called from high-order calls (see 'metacast predn' in mexpand)
:- public data uses_hiord_pred/2.
:- pred uses_hiord_pred/2 # "The predicate can be called from unknown
   program points (with an arbitrary call pattern)".

%:- extends serializable.

:- constructor new_/0.
new_.

% ---------------------------------------------------------------------------
% Register

% :- include(.(memoize__callback)).
% element__save(expand__src, Exp, Name) :-
%       trust(Exp instance_of module_exp),
% 	Exp.write_to_file(Name).
% element__restore(expand__src, Name, Exp) :- module_exp:read_from_file(Name, Exp).
% ---------------------------------------------------------------------------

% TODO: remove 'table' names... use context instead of passing the Exp argument

% TODO: differentiate between argument properties and predicate properties... add discontiguous predicates to specify the default values for each of them

% TODO: declare regtypes for each compiler object? do it automatically?
% :- public static module_exp/1.
% :- regtype module_exp(X) # "@var{X} is the expanded program.".
% module_exp(_).

:- public get_typedef/2.
:- pred get_typedef(+Name, -Def) # "@var{Def} is the definition of type @var{Name}".
get_typedef(Name) := _ :- var(Name), !,
	errlog:bug(['get_typedef/2 with wrong arguments']).
get_typedef(Name) := Def :-
	( ptoc_type(Name, Def) ->
	    true
	; % TODO: add type checkins in previous steps so that if we get here this is because there is a bug
          errlog:bug(['get_typedef/2: undefined type ', Name])
	).

:- public new_atom/1.
% TODO: it has to be deprecated in favor of subatom
new_atom := Atom :-
	( counter(Counter0) -> true ; Counter0 = 0 ),
	Counter1 is Counter0 + 1,
	del(counter(_)),
	add(counter(Counter1)),
	% TODO: do not use module concat, user files need a unique identifier for each file
	atom_concat(~defines_module, ':#', Prefix),
	Atom = ~concat_auxnum(Prefix, Counter0).

:- static concat_auxnum/3.
concat_auxnum(Prefix, N) := ~atom_concat(Prefix, ~atom_concat('#', N2)) :-
	atom_codes(N2, ~number_codes(N)).

:- public new_subatom/2.
% TODO: is this optimal?
new_subatom(RootAtom0) := Atom :-
	( subatom_root(RootAtom0, RootAtom) -> true ; RootAtom = RootAtom0 ),
	( subatom_counter(RootAtom, Counter0) -> true ; Counter0 = 0 ),
	Counter1 is Counter0 + 1,
	del(subatom_counter(RootAtom, _)),
	add(subatom_counter(RootAtom, Counter1)),
	Atom = ~concat_auxnum(RootAtom, Counter0),
	add(subatom_root(Atom, RootAtom)).

clone_props(OrigName, Name) :-
	OrigName = OrigMF/OrigA,
	( expanded_import(OrigMF, OrigA, OrigPubProps) -> true
	; expanded_decl(OrigMF, OrigA, OrigPubProps) -> true
	),
	PubProps = OrigPubProps,
	Name = MF/A,
	add(expanded_decl(MF, A, PubProps)),
        del(priv_props(MF, A, _)),
        ( priv_props(OrigMF, OrigA, Props) ->
	    add(priv_props(MF, A, Props))
	; true
	).

:- public class predicate_x {
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    % TODO: define ':- mut' attributes here
    :- constructor new_/1.
    new_(Name) :-
	~self = ~'$mut__init'('$pn'(Name, no_code, [], _EmptyPropDic)).

    :- public name/1.
    name := Name :-
	'$pn'(Name, _, _, _) = ~'$mut__value'(~self).

    :- public set_code/1.
    set_code(Code) :-
	'$pn'(Name, _, MemoDic, PropDic) = ~'$mut__value'(~self),
	'$mut__assign'(~self, '$pn'(Name, Code, MemoDic, PropDic)).

    :- public code/1.
    code := Code :-
	'$pn'(_, Code, _, _) = ~'$mut__value'(~self).

    :- public memodic/1.
    memodic := MemoDic :-
	'$pn'(_, _, MemoDic, _) = ~'$mut__value'(~self).

    :- public set_memodic/1.
    set_memodic(MemoDic) :-
	'$pn'(Name, Code, _, PropDic) = ~'$mut__value'(~self),
	'$mut__assign'(~self, '$pn'(Name, Code, MemoDic, PropDic)).

    :- public propdic/1.
    propdic := PropDic :-
	'$pn'(_, _, _, PropDic) = ~'$mut__value'(~self).

    :- public set_propdic/1.
    set_propdic(PropDic) :-
	'$pn'(Name, Code, MemoDic, PropDic) = ~'$mut__value'(~self),
	'$mut__assign'(~self, '$pn'(Name, Code, MemoDic, PropDic)).

    {
    :- fluid exp :: module_exp.
    :- public get_prop/2.
    % TODO: get name for cswitchcfun!!! so that we can define more than one...
    % TODO: Def is the public set of properties, Prop is the private set of properties!! use better names!
    get_prop(Prop) := Value :-
	'$pn'(Name, _, _, _) = ~'$mut__value'(~self),
	predid :: predicate_x <- ~self,
	name :: any <- Name,
	props :: any,
	Value = ~exp.prop_extval(Prop).
    }.

    {
    :- fluid exp :: module_exp.
    :- public set_props/1.
    set_props([]).
    set_props([Prop=Value|Xs]) :-
	set_prop(Prop, Value),
	set_props(Xs).

    :- public set_prop/2.
    set_prop(Prop, Value0) :- !,
        norm_prop(Prop, Value0, Value),
        set_prop0(Prop, Value).

    set_prop0(Prop, Value) :-
	'$pn'(Name, Code, MemoDic, PropDic0) = ~'$mut__value'(~self),
	PropDic = ~dic_replace(PropDic0, Prop, Value),
	'$mut__assign'(~self, '$pn'(Name, Code, MemoDic, PropDic)).
    }.

    {
    :- fluid exp :: module_exp.
    :- public meta_predicate new_sub(?, out(predicate_x)).
    % create a subpredicate
    new_sub(NewArity) := NewId :-
	FromName = ~name,
	MF = ~exp.new_atom,
	NewName = MF/NewArity,
	exp.get1_pred_def(FromName, Def),
	% TODO: this may not be true!
	PubProps0 = ~pub_props__default,
	PubProps = ~pub_props__set_def(PubProps0, Def),
	exp.add(expanded_decl(MF, NewArity, PubProps)),
	NewId = ~predicate_x.reg_new(NewName).
    }.

    {
    :- fluid exp :: module_exp.
    % Create/query a new registered predicate
    :- public static meta_predicate reg_new(?, out(predicate_x)).
    % lookup a PredId identified by Name in the preddic symbol table
    reg_new(Name) := PredId :-
	Dic = ~exp.pred_name_lookup_dic,
	( dic_get(Dic, Name, PredId0) ->
	    true
%	    errlog:trace([found_created_new(Name)]),
%	    PredId0 = ~predicate_x.new(Name)
	; dic_lookup(Dic, Name, PredId0),
%	  errlog:trace([not_found_creating_new(Name)]),
	  PredId0 = ~predicate_x.new(Name)
	),
	PredId = PredId0.
%        PredId = ~predicate_x.new(Name).
    }.

    {
    :- fluid exp :: module_exp.
    :- public meta_predicate clone(?, out(predicate_x)).
    % Clone predicate definition of @{Name}
    % TODO: rename? only clones the table entry... (props)
    clone(NewProps) := NewPredId :-
	Name = ~name,
	% Get a new name
	Name = Atom/Arity,
	AuxAtom = ~exp.new_subatom(Atom),
	AuxName = AuxAtom/Arity,
	% Update props
%	clone_props(Name, AuxName),
%	set_merged_props(AuxName, [register=false|NewProps]),
%	%
%	NewPredId = ~predicate_x.new(AuxName).
	NewPredId = ~clone_and_set_props(AuxName, [register=false|NewProps]).
    }.

    {
    :- fluid exp :: module_exp.
    clone_and_set_props(AuxName, NewProps) := NewPredId :-
	NewPredId = ~predicate_x.new(AuxName),
	Name = ~name,
	exp.clone_props(Name, AuxName),
	exp.set_merged_props(AuxName, NewProps),
	NewPredId.set_propdic(~dic_copy(~self.propdic)),
	NewPredId.set_props(NewProps).
    }.

    % copy a dictionary (same elements and keys)
    :- static dic_copy/2.
    dic_copy(Node, _) :- var(Node), !. % new leaf
    dic_copy(dic(Key,Val,L0,R0), dic(Key,Val,L,R)) :-
	dic_copy(L0, L),
	dic_copy(R0, R).

    {
    :- fluid exp :: module_exp.
    :- public clone_nullin/2.
    % Clone predicate definition of @{Name} removing input arguments
    clone_nullin(NewProps) := NewPredId :-
        % TODO: keep param arguments?
        % remove input arguments
	% TODO: remove sht_usermemo if possible...
	% TODO: use that or sht_usermemo prop?
        call((
	  intr :: absint <- sht_analyzer,
	  AbsDef0 = ~intr.get_usermemo(~self),
	  trust(AbsDef0 instance_of shtdef)		 
        )),
	Modes0 = ~get_prop(argmodes),
	Mems0 = ~get_prop(argmems),
	ImpTypes0 = ~get_prop(argimptypes),
	Derefs0 = ~get_prop(argderefs),
	Unboxs0 = ~get_prop(argunboxs),
	CallTypes0 = ~AbsDef0.get_call_types,
	CallTypes = ~filter_mode(CallTypes0, Modes0, out),
	( AbsDef0.is_bottom ->
	    AbsDef = ~shtdef.new_bottom(CallTypes)
	; ExitTypes0 = ~AbsDef0.get_exit_types,
	  ExitTypes = ~filter_mode(ExitTypes0, Modes0, out),
	  AbsDef = ~shtdef.new(CallTypes, ExitTypes)
	),
	Modes = ~filter_mode(Modes0, Modes0, out),
	Mems = ~filter_mode(Mems0, Modes0, out),
	ImpTypes = ~filter_mode(ImpTypes0, Modes0, out), % TODO: apply to any found prop!!!!
	Derefs = ~filter_mode(Derefs0, Modes0, out),
	Unboxs = ~filter_mode(Unboxs0, Modes0, out),
	Props1 = ~prop_merge(NewProps,
	                     [sht_usermemo = AbsDef,
		             argmodes = Modes,
		             argmems = Mems,
		             argimptypes = ImpTypes,
		             argderefs = Derefs,
		             argunboxs = Unboxs,
			     register = false]),
	% TODO: inefficcient? take new arity from prop_nullin?
	% TODO: remove out arguments from the 'meta' declaration!!!
	% Get a new name
	Name = ~name,
	Name = Atom/_,
	AuxAtom = ~exp.new_subatom(Atom),
	AuxArity = ~length(Modes),
	AuxName = AuxAtom/AuxArity,
%	exp.clone_props(Name, AuxName),
%	exp.set_merged_props(AuxName, Props1),
%	%
%	NewPredId = ~predicate_x.new(AuxName).
	NewPredId = ~clone_and_set_props(AuxName, Props1).
    }.

    {
    :- fluid exp :: module_exp.
    :- public clone_chain/3.
    % Clone predicate definition of @{Name} removing input arguments and replacing them by output arguments of PrevName
    clone_chain(PrevId, NewProps) := NewPredId :-
	trust(PrevId instance_of predicate_x),
	Name = ~name,
	% Get out props of PrevId
        call((
	  intr :: absint <- sht_analyzer,
	  PrevAbsDef0 = ~intr.get_usermemo(PrevId),
	  trust(PrevAbsDef0 instance_of shtdef)
        )),
	PrevModes0 = ~PrevId.get_prop(argmodes),
	PrevMems0 = ~PrevId.get_prop(argmems),
	PrevImpTypes0 = ~PrevId.get_prop(argimptypes),
	PrevDerefs0 = ~PrevId.get_prop(argderefs),
	PrevUnboxs0 = ~PrevId.get_prop(argunboxs),
	( PrevAbsDef0.is_bottom ->
	    errlog:bug(['pred_chain cannot chain a predicate call that fails ', ~PrevId.name]),
	    fail
	; PrevExitTypes0 = ~PrevAbsDef0.get_exit_types,
	  PrevExitTypes = ~filter_mode(PrevExitTypes0, PrevModes0, out)
	),
	PrevModes = ~filter_mode(PrevModes0, PrevModes0, out),
	PrevMems = ~filter_mode(PrevMems0, PrevModes0, out),
	PrevImpTypes = ~filter_mode(PrevImpTypes0, PrevModes0, out), % TODO: apply to any found prop!!!!
	PrevDerefs = ~filter_mode(PrevDerefs0, PrevModes0, out),
	PrevUnboxs = ~filter_mode(PrevUnboxs0, PrevModes0, out),
	% Get out props of PredId
        % TODO: keep params arguments?
        call((
          intr :: absint <- sht_analyzer,
          AbsDef0 = ~intr.get_usermemo(~self),
	  trust(AbsDef0 instance_of shtdef)	  
        )),	
	Modes0 = ~get_prop(argmodes),
	Mems0 = ~get_prop(argmems),
	ImpTypes0 = ~get_prop(argimptypes),
	Derefs0 = ~get_prop(argderefs),
	Unboxs0 = ~get_prop(argunboxs),
	CallTypes0 = ~AbsDef0.get_call_types,
	OutCallTypes = ~filter_mode(CallTypes0, Modes0, out),
%	( AbsDef0.is_bottom ->
%	    OutExitTypes = ~AbsDef0.get_exit_types
%	; ExitTypes0 = ~AbsDef0.get_exit_types,
%         OutExitTypes = ~filter_mode(ExitTypes0, Modes0, out)
%	),
	OutModes = ~filter_mode(Modes0, Modes0, out),
	OutMems = ~filter_mode(Mems0, Modes0, out),
	OutImpTypes = ~filter_mode(ImpTypes0, Modes0, out),
	OutDerefs = ~filter_mode(Derefs0, Modes0, out),
	OutUnboxs = ~filter_mode(Unboxs0, Modes0, out),
	% output of PrevName is the input of Name
	InCount = ~length(PrevModes),
	CallTypes = ~append(PrevExitTypes, OutCallTypes),
	( AbsDef0.is_bottom ->
	    AbsDef = ~shtdef.new_bottom(CallTypes)
	; % TODO: exit types of PrevExitTypes is not known!
	  ExitTypes = ~append(~repeat_n(InCount, ~type_norm(any)), OutCallTypes),
	  AbsDef = ~shtdef.new(CallTypes, ExitTypes)
	),
	Modes = ~append(~repeat_n(InCount, in), OutModes),
	Mems = ~append(PrevMems, OutMems), % sure?
	ImpTypes = ~append(PrevImpTypes, OutImpTypes), % sure?
	Derefs = ~append(PrevDerefs, OutDerefs), % sure?
	Unboxs = ~append(PrevUnboxs, OutUnboxs), % sure?
	%
	Props1 = ~prop_merge(NewProps,
	                     [sht_usermemo = AbsDef,
		             argmodes = Modes,
		             argmems = Mems,
		             argimptypes = ImpTypes,
		             argderefs = Derefs,
		             argunboxs = Unboxs,
			     register = false]),
	% TODO: inefficcient? take new arity from prop_nullin?
	% TODO: remove out arguments from the 'meta' declaration!!!
	% Get a new name
	Name = Atom/_,
	AuxAtom = ~exp.new_subatom(Atom),
	AuxArity = ~length(Modes),
	AuxName = AuxAtom/AuxArity,
%	exp.clone_props(Name, AuxName),
%	exp.set_merged_props(AuxName, Props1),
%	%
%	NewPredId = ~predicate_x.new(AuxName).
	NewPredId = ~clone_and_set_props(AuxName, Props1).
    }.

    {
    % Obtain a new auxiliar 'proceed' predicate for this predicate.
    % Precondition: the predicate ~self cannot fail/have failure property?
    % Note that there can be multiple 'proceed' in the program.
    :- fluid exp :: module_exp.
    :- public new_proceed/1.
    new_proceed := NewPredId :-
	Modes = ~get_prop(argmodes),
	AbsDef = ~get_prop(sht_usermemo),
	trust(AbsDef instance_of shtdef),
	( AbsDef.is_bottom ->
	    % never succeeds
	    OutTypes = [],
	    OutMems = [],
	    OutDerefs = [],
	    OutUnboxs = []
	; Modes = ~get_prop(argmodes),
	  Mems = ~get_prop(argmems),
	  Derefs = ~get_prop(argderefs),
	  Unboxs = ~get_prop(argunboxs),
	  ExitTypes = ~AbsDef.get_exit_types,
	  OutTypes = ~filter_mode(ExitTypes, Modes, out),
	  OutMems = ~filter_mode(Mems, Modes, out),
	  OutDerefs = ~filter_mode(Derefs, Modes, out),
	  OutUnboxs = ~filter_mode(Unboxs, Modes, out)
	),
	Imp = ~get_prop(imp),
        % Insert new predicate in table
        ProceedAtom = ~exp.new_atom,
        Arity = ~length(OutTypes),
        ProceedName = ProceedAtom/Arity,
	% TODO: modes are in, it works bc it is the default, but put them here
	ProceedProps = [imp = Imp,
	                sht_usermemo = ~shtdef.new(OutTypes, OutTypes), % TODO: I know that this is strange... indeed the proceed pred does not exit 
		        argmems = OutMems,
		        argderefs = OutDerefs,
		        argunboxs = OutUnboxs,
		        heap_usage = max(0),
		        frame_usage = max(0),
		        is_proceed = true],
	new_pred(ProceedName, ptoc, ProceedProps),
	%
	NewPredId = ~predicate_x.reg_new(ProceedName),
	NewPredId.set_props(ProceedProps).

    % TODO: include more public properties
    :- static new_pred/3.
    new_pred(Name, Def, Props) :-
	Name = MF/A,
	PubProps0 = ~pub_props__default,
	PubProps = ~pub_props__set_def(PubProps0, Def),
	exp.add(expanded_decl(MF, A, PubProps)),
        exp.add(priv_props(MF, A, Props)).
    }.
}.

% properties:
% - imp: predicate implementation scheme
% - is_proceed: (TODO: it should mean 'no returns' property + executes success continuation)
% - argmodes:
% - argderefs:
% - argunboxs:
% - noderefmod:
% - should_trim_frame: recursive and the frame should be trimmed (because recursion consumes stack)
% (TODO: split recursive and trim_frame properties?)

% G needs liveness information
{
:- fluid predid :: predicate_x.
:- fluid name :: any.
:- fluid props :: any.
prop_extval(needs_liveness_info) := Value :- !,
	info = ~prop_extval(heap_usage),
	Value = true.
% G has no side effects (i.e. (G, fail) is equivalent to (fail))
prop_extval(nosideeff) := Value :- !,
	Value = ( ~name = MF/A, nosideeff__2(MF, A) ? true
		| ~prop_val(nosideeff)
		).
prop_extval(uses_successcont) := Value :- !,
	Imp = ~prop_extval(effective_imp),
	( Imp = nondet ),
	Value = true.
prop_extval(nosideeffdet) := Value :- !,
	Value = ( ~name = MF/A, nosideeffdet__2(MF, A) ? true
		| ~prop_val(nosideeffdet)
		).
% G does not preserves the temporary registers
prop_extval(clear_regs) := Value :- !,
	SaveRegs = ~prop_extval(saveregs),
	\+ SaveRegs = all,
	Value = true.
% 'max(Size)' or 'info' or 'unknown'
prop_extval(heap_usage) := HeapUsage :- !,
	( Def = ~prop_val(def),
	  ( Def = builtin(_,MemUsage) ->
	      ( MemUsage = m(HeapUsage0, _) ->
		  true
	      ; HeapUsage0 = MemUsage
	      ),
	      HeapUsage = max(HeapUsage0)
	  ; Def = func(_,MemUsage) ->
	      HeapUsage = ~funcmem__heap(MemUsage)
	  ; Def = funcre(_,MemUsage) ->
	      HeapUsage = ~funcmem__heap(MemUsage)
	  ) ->
	    true
	; HeapUsage = ~prop_val(heap_usage)
	).
prop_extval(frame_usage) := FrameUsage :- !,
	( Def = ~prop_val(def),
	  ( Def = builtin(_,_) ->
	      FrameUsage = max(0)
	  ; Def = func(_,_) ->
	      FrameUsage = max(0)
	  ; Def = funcre(_,_) ->
	      FrameUsage = max(0)
	  ) ->
	    true
	; FrameUsage = ~prop_val(frame_usage)
	).
prop_extval(trail_usage) := TrailUsage :- !,
	( Def = ~prop_val(def),
	  ( Def = builtin(_,MemUsage),
	    MemUsage = m(_, TrailUsage0) ->
	      TrailUsage = max(TrailUsage0)
	  ; Def = func(_,MemUsage),
	    \+ MemUsage = yes ->
	      TrailUsage = max(0)
	  ; Def = funcre(_,MemUsage),
	    \+ MemUsage = yes ->
	      TrailUsage = max(0)
	  ) ->
	    true
	; TrailUsage = ~prop_val(trail_usage)
	).
prop_extval(uses_trail) := Value :- !,
	TrailUsage = ~prop_extval(trail_usage),
	\+ TrailUsage = max(0),
	Value = true.
prop_extval(uses_failcont) := Value :- !,
	Imp = ~prop_extval(effective_imp),
	( Imp = nondet ),
	Value = true.
% G modifies choice on success (does a cut or creates choice points)
prop_extval(modifies_choice_on_success) := Value :- !,
	Imp = ~prop_extval(effective_imp),
	( Imp = nondet -> true
	; Imp = detcut -> true
	; fail
	),
	Value = true.
% G needs all the choices be complete
% TODO: define the negated (is easier to understand)
prop_extval(needs_neck) := Value :- !,
	( HeapUsage = ~prop_extval(heap_usage),
	  HeapUsage = max(0),
	  TrailUsage = ~prop_extval(trail_usage),
	  TrailUsage = max(0),
	  SaveRegs = ~prop_extval(saveregs),
	  SaveRegs = all ->
	    Value = false % do not need a complete choice point
	; Value = true
	).
% G needs all the frames be complete
% TODO: hmmm why?
prop_extval(needs_cframe) := Value :- !,
	HeapUsage = ~prop_extval(heap_usage),
	HeapUsage = info,
	Value = true.
% TODO: merge impcode and impreg in a single property... callmethod? or use a different imp for impreg?
% TODO: define a safe imp and then give other solutions via analysis?
% TODO: note that imp property talks about a coding scheme, not determinism!
% TODO: try to simplify...
prop_extval(impreg) := Value :- !,
	% should the predicate be called using the predicate table entry?
	( Def = ~prop_val(def),
	  ( Def = ptoc_macro
	  ; Def = ptoc_builtin
	  ; Def = ptoc
	  ; Def = cswitchcfun
	  ; Def = implicit_cbool(_)
	  ; Def = implicit_cinsnp(_)
	  ; Def = builtin(_,_)
	  ; Def = func(_,_)
	  ; Def = funcre(_,_)
	  ) ->
	    Value = false
	; Value = true
	).
prop_extval(impcode) := Value :- !,
	( Def = ~prop_val(def),
	  ( Def = implicit_cbool(C) -> Value = C
	  ; Def = implicit_cinsnp(C) -> Value = C
	  ; Def = builtin(CName,_) -> Value = CName
	  ; Def = func(CName,_) -> Value = CName
	  ; Def = funcre(CName,_) -> Value = CName
	  ) ->
	    true
	; Value = ~prop_val(impcode)
	).
% 'imp' effective in compilation (i.e. nondet for bytecode)
% TODO: make it cleaner
prop_extval(effective_imp) := Value :- !,
%	Def = ~prop_val(def),
%	( Def = bytecode -> Value = nondet
%	; Value = ~prop_extval(imp)
	Value = ~prop_extval(imp).
%	).
prop_extval(imp) := Value :- !,
	Def = ~prop_val(def),
	( Def = implicit_cbool(_) -> Value = semidet
	; Def = implicit_cinsnp(_) -> Value = nondet
	; Def = builtin(_,_) -> Value = semidet
	; Def = func(_,_) -> Value = det
	; Def = funcre(_,_) -> Value = semidet_re
	; ( Value0 = ~prop_val(imp) ->
	      Value = Value0
	  ; Def = ptoc, IDetDef = ~prop_val(idet_usermemo) ->
	      idet__absdef_to_imp(IDetDef, Imp2),
	      Value = Imp2
%	      errlog:trace([impnondetshouldbe(Name,Imp2)]),
%	      Value = nondet
	  ; Value = nondet
	  )
	).
prop_extval(saveregs) := Value :- !,
	( Def = ~prop_val(def),
	  ( Def = builtin(_,_)
	  ; Def = func(_,_)
	  ; Def = funcre(_,_)
	  ) ->
	    Value = all
	; Value = ~prop_val(saveregs)
	).
prop_extval(should_trim_frame) := Value :- !,
	( Def = ~prop_val(def),
	  ( Def = builtin(_,_) -> true
	  ; Def = func(_,_) -> true
	  ; Def = funcre(_,_) -> true
	  ) ->
	    Value = false
	; Value = ~prop_val(should_trim_frame)
	).
prop_extval(argmems) := Value :- !,
	( Def = ~prop_val(def),
	  ( Def = builtin(_,_)
	  ; Def = func(_,_)
	  ; Def = funcre(_,_)
	  ) ->
	    ~name = _/Arity,
	    Value = ~cvar_argmems(Arity)
	; Value = ~prop_val(argmems)
	).
prop_extval(argmodes) := Value :- !,
	( Def = ~prop_val(def),
	  ( Def = func(_,_)
	  ; Def = funcre(_,_)
	  ) ->
	    ~name = _/Arity,
	    Value = ~func_argmodes(Arity)
	; Value = ~prop_val(argmodes)
	).
% the predicate requires analysis using sht_analyzer
prop_extval(req_sht) := Value :- !,
	( Def = ~prop_val(def),
	  ( Def = ptoc ) ->
	    Value = true
	; Value = ~prop_val(req_sht)
	).
% the predicate do not require a default entry in analysis (trust that analyzer will find all call patterns) % TODO: temporal!?
/*
prop_extval(nodefentry) := Value :- !,
	( Def = ~prop_val(def),
	  ( Def = ptoc ->
	      ( vs_public = ~prop_val(visibility) ->
		  Value = false
	      ; Value = true
	      )
	  ; Def = ptoc_macro -> Value = true
	  ; Def = ptoc_builtin -> Value = true
	  ) ->
	    true
	; fail
	).
*/
% compilation mode for the predicate % TODO: temporal!?
prop_extval(compmode) := Value :- !,
	( Def = ~prop_val(def),
	  ( def__bytecode(Def) -> Value = bytecode
	  ; def__needs_bytecode_hook(Def) -> Value = bytecodehook
	  ; Def = ptoc -> Value = lowcomp
	  ) ->
	    true
	; Value = ~prop_val(compmode)
	).
% the predicate code is dynamic (can be changed at runtime)
prop_extval(impdyn) := Value :- !,
	( Def = ~prop_val(def),
	  ( def__dyncode(Def) -> Value = true
	  ) ->
	    true
	; Value = ~prop_val(impdyn)
	).
prop_extval(Prop) := Value :- !,
	Value = ~prop_val(Prop).

prop_val(Prop, Value) :-
	PropDic = ~predid.propdic,
	( dic_get(PropDic, Prop, Value0) ->
	    Value = Value0
	; Value = ~prop_val1(Prop),
	  dic_lookup(PropDic, Prop, Value)
	).
prop_val1(Prop, Value) :-
	% load props lazily
	Props = ~props,
	( nonvar(Props) ->
	    true
	; Name = ~name,
	  Name = MF/A,
	  ( priv_props(MF, A, PrivProps) -> true ; PrivProps = [] ),
	  Def = ~get1_pred_def(Name),
	  Visibility = ~get1_pred_visibility(Name),
	  Props = [def=Def,visibility=Visibility|PrivProps]
	),
	%
	( prop_val0(Prop, Props, Value) ->
	    true
	; ~name = _/A,
	  prop_default(Prop, A, Value)
	).
}.

:- static prop_val0/3.
prop_val0(Prop, Props, Value) :-
        member((Prop = Value0), Props), !,
	Value = Value0.

% TODO: do not export!!! all properties shoud be accessed using get_prop
get1_pred_visibility(MF/A) := Visibility :-
	( expanded_import(MF, A, PubProps) -> true
	; expanded_decl(MF, A, PubProps) -> true
	),
	PubProps = pub_props(_, _, Visibility, _, _).

% ---------------------------------------------------------------------------

% TODO: some arithmetic predicates with predicate code are not included here... (check)
:- public static def__bytecode/1.
def__bytecode(bytecode) :- !.

:- public static def__needs_bytecode_hook/1.
% TODO: builtin and and func needs bytecode hooks...
% TODO: base on other properties... e.g. the blt number, something from the absmach definition, etc.
def__needs_bytecode_hook(builtin(_, _)).
def__needs_bytecode_hook(func(_, no)). % TODO: why second arg is 'no'?
def__needs_bytecode_hook(funcre(_, no)). % TODO: why second arg is 'no'?

% pub_props's def of predicates with code that can be changed at runtime
% TODO: find a better name
:- public static def__dyncode/1.
def__dyncode(dynamic).
def__dyncode(data).
def__dyncode(concurrent).

:- static prop_default/3.
prop_default(saveregs, _) := none :- !.
prop_default(should_trim_frame, _) := true :- !.
prop_default(heap_usage, _) := unknown :- !.
prop_default(frame_usage, _) := unknown :- !.
prop_default(trail_usage, _) := unknown :- !.
prop_default(argmodes, A) := ~repeat_n(A, in) :- !.
prop_default(argderefs, A) := ~repeat_n(A, false) :- !.
prop_default(argmems, A) := ~repeat_n(A, anymem) :- !.
prop_default(argimptypes, A) := ~repeat_n(A, tagged) :- !.
prop_default(argunboxs, A) := ~repeat_n(A, false) :- !.
% TODO: rename indexed by specindex
prop_default(indexed, _) := true :- !.
prop_default(do_not_check_events, _) := false :- !.

% TODO: extract this list as a property | nosideeff in the sense of the WAM... 
:- static nosideeff__2/2.
nosideeff__2('term_basic:$instance', 2) :- !.
nosideeff__2('term_basic:$unify', 2) :- !.
nosideeff__2('term_typing:var', 1) :- !.
nosideeff__2('attributes:get_attribute', 2) :- !.
nosideeff__2('term_typing:nonvar', 1) :- !.
nosideeff__2('term_typing:atomic', 1) :- !.
nosideeff__2('term_typing:atom', 1) :- !.
nosideeff__2('term_typing:number', 1) :- !.
nosideeff__2('basiccontrol:$caller_choice', 1) :- !.

% TODO: extract this list as a property, define in terms of nosideeff and other properties?
% TODO: by 'det' we want to say that we can remove the operation if the outputs are not needed and the computation will not be affected (so it does not bind variables, does not cut, does not create choice points, does not fail)???
:- static nosideeffdet__2/2.
%nosideeffdet__2('term_typing:var', 1) :- !.
nosideeffdet__2('attributes:get_attribute', 2) :- !.
%nosideeffdet__2('term_typing:nonvar', 1) :- !.
%nosideeffdet__2('term_typing:atomic', 1) :- !.
%nosideeffdet__2('term_typing:atom', 1) :- !.
%nosideeffdet__2('term_typing:number', 1) :- !.
nosideeffdet__2('basiccontrol:$caller_choice', 1) :- !.

:- use_module(library(lists), [append/3, length/2]).

:- static func_argmodes/2.
func_argmodes(N) := [in| ~func_argmodes(N1)] :- N > 1, !, N1 is N - 1.
func_argmodes(1) := [out].

:- static cvar_argmems/2.
cvar_argmems(N) := [cvar| ~cvar_argmems(N1)] :- N >= 1, !, N1 is N - 1.
cvar_argmems(0) := [].

:- static funcmem__heap/2.
funcmem__heap(yes) := info :- !.
funcmem__heap(no) := max(0) :- !.
funcmem__heap(_) := fixme_what. % TODO: ein?

:- public set_prop/3.
% TODO: optimize
set_prop(Name, Prop, Value) :-
	NewProps = [Prop = Value],
	set_merged_props(Name, NewProps).

set_merged_props(Name, NewProps) :-
	% TODO: should the props of imported predicates be redefinable?
	Name = MF/A,
	( expanded_import(MF, A, _) -> true
	; expanded_decl(MF, A, _) -> true
	; errlog:bug(['setting properties of undefined predicate ', Name])
	),
        ( priv_props(MF, A, Props0) ->
	    true
	; Props0 = []
	),
	call((
	  exp :: module_exp <- ~self,
	  Props = ~prop_merge(NewProps, Props0)
        )),
        del(priv_props(MF, A, _)),
        add(priv_props(MF, A, Props)).

% ---------------------------------------------------------------------------

% Clean the memodic slot of all predicates
:- public static pred_list__clean_memodic/1.
pred_list__clean_memodic([]).
pred_list__clean_memodic([PredId|Xs]) :-
	trust(PredId instance_of predicate_x),
	PredId.set_memodic([]),
	pred_list__clean_memodic(Xs).

% Ensure that the input predicates are in the predicate name lookup table
:- public preddic__register_preds/1.
preddic__register_preds([]).
preddic__register_preds([PredId|Xs]) :-
	preddic__register(PredId),
	preddic__register_preds(Xs).

% Ensure that the input predicate is in the predicate name lookup table
:- public preddic__register/1.
preddic__register(PredId) :-
	trust(PredId instance_of predicate_x),
	Name = ~PredId.name,
	Dic = ~pred_name_lookup_dic,
	( dic_get(Dic, Name, PredId0) ->
	    ( PredId0 == PredId -> true
	    ; errlog:bug(['warning: different predicate definitions for predicate named as ', Name])
	    )
	; dic_lookup(Dic, Name, PredId)
	).

:- public preddic__lookup/2.
% TODO: Unused?
% lookup a PredId identified by Name in the preddic symbol table
preddic__lookup(Name) := PredId :-
	% TODO: a copy_term may be required!
	Dic = ~pred_name_lookup_dic,
	dic_get(Dic, Name, PredId).

% ---------------------------------------------------------------------------
% TODO: use the previous preds? (only if efficiency is the same)
:- public is_ipred/1.
% there is an optimized instruction to call the predicate with definition Def
% TODO: good name? ipred is a predicate that can be called in the bytecode using special instructions
is_ipred(Atom/Arity) :-
	is_ipred__2(Atom, Arity), !.
is_ipred(Name) :-
	Def = ~get1_pred_def(Name),
	is_ipred__def(Def).

:- static is_ipred__2/2.
is_ipred__2('basiccontrol:$caller_choice', 1) :- !.
is_ipred__2('basiccontrol:$cut', 1) :- !. 
is_ipred__2('term_basic:$instance', 2) :- !.
is_ipred__2('term_basic:$unify', 2) :- !.

get1_pred_def(MF/A) := Def :-
	( expanded_import(MF, A, PubProps) -> true
	; expanded_decl(MF, A, PubProps) -> true
	),
	PubProps = pub_props(Def, _, _, _, _).

:- static is_ipred__def/1.
is_ipred__def(builtin(_, _)).
is_ipred__def(func(_, _)).
is_ipred__def(funcre(_, _)).

:- public ipred__uses_heap/1.
ipred__uses_heap(Name) :-
	Def = ~get1_pred_def(Name),
	( Def = func(_, yes) -> true
	; Def = funcre(_, yes) -> true
	).

:- public ipred__imp/2.
ipred__imp(Name) := Imp :-
	Def = ~get1_pred_def(Name),
	Imp = ( Def = func(_, _) ? det
	      | Def = funcre(_, _) ? semidet_re
	      | Def = builtin(_, _) ? semidet
	      ).

:- public ipred__heap/2.
ipred__heap(Name) := Heap :-
	Def = ~get1_pred_def(Name),
	( Def = func(_, MemUsage) -> true
	; Def = funcre(_, MemUsage) -> true
	; Def = builtin(_, MemUsage0) ->
	    MemUsage = max(MemUsage0)
	),
	MemUsage = max(M),
        ( M = m(Heap,_Trail) -> true ; M = Heap ).

% ---------------------------------------------------------------------------

% TODO: syncronize bits with absmach_def:defbits/2.
:- public pred__defbits/2.
pred__defbits(Name) := Bits :-
	Name = MF/A,
	expanded_decl(MF, A, PubProps),
	PubProps = pub_props(Def0, Meta, Visibility, _, Context),
	% TODO: access this property using get_prop
	Def1 = ( def__needs_bytecode_hook(Def0) ? bytecode | Def0 ),
	get_bits(Def1, Meta, Visibility, Context, Bits).

% TODO: get those definitions from the abstract machine description (including the RtBits one)
:- static get_bits/5.
get_bits(Def, Meta, Visibility, Context, Bits) :-
	defbits__def(Def, DefBits),
	defbits__visibility(Visibility, VisibilityBits),
	RtBits = ( Meta == 0, Context == none ? 0 | 2'1000 ),
	Bits is VisibilityBits \/ DefBits \/ RtBits.

% TODO: share with rt_exp... and definitions of prolog_sys
:- static defbits__def/2.
% special def bits
defbits__def(dynamic,     2'01) :- !.
defbits__def(data,        2'01) :- !.
defbits__def(concurrent,  2'11) :- !.
% no special def bits for others
defbits__def(_,           2'00).

:- static defbits__visibility/2.
defbits__visibility(vs_public,     2'000).
defbits__visibility(vs_private,    2'000).
defbits__visibility(vs_multifile,  2'100).

% ---------------------------------------------------------------------------
% Annotated terms

:- compilation_fact(use_backend(bc)).
:- include(compiler(annotated_terms)).
