:- doc(section, "Flattening of modules/classes to kernel code").

% TODO: Rename, not precisely a flattening (as modules are not
%       resolved)

% This translation generates kernel code for special module/class
% methods:
%
%  - constructors (including allocators) and initializers
%  - accessors for module/class state fields
%  - checks for 'instance_of' 

% TODO: Apply this translation to both module_s and module_s_itf.
%
%   Currently, translation is done as:
%
%         <Module>
%            |    
%            v      itf-projection
%         <FlatMod> -------------->  <FlatItf>
%
%   But this has a big drawback: the flattening often needs
%   information about imported definitions. This is why I had to
%   introduce '$trust_statemodel'. If we change interface projection,
%   so that it is not flattened, and do flattening in the 'compile'
%   part, we get this picture:
%
%                   itf-projection
%         <Module>  -------------->  <ItfModule>
%            |                           |
%            v                           v
%         <FlatMod>                  <FlatItf>
%
%   This could solve the problems mentioned above.

% (handler for module compilation pass)
modpass__do(flatten_module, Module) :- flatten_module(Module).

{
:- extends errlog_ctx.
% Flatten special module methods
flatten_module(Module) :-
	trust(Module instance_of module_s),
	Module.is_static_or_class_or_interface,
	!,
	gen_module_virtual(Module), % TODO: may create new fields
	gen_module_field_methods(Module),
	apply_extends_to_module(Module),
	gen_module_ctors(Module),
	gen_module_instance_of(Module).
flatten_module(_).
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Flatten virtual predicates").
% TODO: Merge with interfaces

:- if(use_backend(bc)).
gen_module_virtual(Module) :-
	trust(Module instance_of module_s),
	( % (failure-driven loop)
	  Module.enum_defined_preds(Pred),
	    gen_virtual(Module, Pred),
	    fail
	; true
	).

% Given a predicate N/A, create:
%  - a field '<N>__v' that will contain the virtual entry.
%  - a predicate accessor '<N>__p' associated with the virtual.
%
% TODO: this creates a 'field'; this is not the way to implement it
gen_virtual(Module, Pred_) :-
	trust(Module instance_of module_s),
	trust(Pred_ instance_of predicate_s),
	Pred_.get_prop(virtual),
	!,
	Pred_.get_prop(owner_module_ctx(Context)),
	%
	% TODO: ugly code (hide in the class) (duplicated)
	F = ~Pred_.f, A = ~Pred_.a,
	Module.method_f(F, A, Name),
	%
	NameV = ~atom_concat(Name, '__v'),
	NameP = ~atom_concat(Name, '__p'),
	%
	Pred = ~Module.pred_ref0(NameP, A),
	Pred.set_prop(defined),
	% TODO: Clone 'Context' in 'Module' (but avoid repetition...)
	Pred.set_prop(owner_module_ctx(Context)),
	%
	% TODO: Missing ctxprj in Metatype!
	% Field that will contain the predicate abstraction
%	do_field(NameV, single(pred(A, Context))),
	Module.add1_field(NameV, 'blt__any'),
	% Entry in the class definition
	NameP2 = ~Pred.f,
	Module.add_virtual(Name, A, NameV, NameP2, Context),
	% Wrapper caller method
	functor(GoalP, NameP, A), 
	GoalP =.. [_|Args],
	Call =.. [call,Var|Args],
	NameVG =.. [NameV, Var],
	GetV = (NameVG, '$trust_metatype'(Var, primitive(pred(A, Context)))),
	Module.add_clause(GoalP, (GetV, Call)).
gen_virtual(_, _).
:- elif(use_backend(js)).
gen_module_virtual(_).
:- endif.

% ---------------------------------------------------------------------------
:- doc(subsection, "Flatten module/class fields (accessors for module/class state)").

:- if(use_backend(bc)).
{
:- extends errlog_ctx. % TODO: This should not be necessary (only for class_has_mutable_statemodel)
gen_module_field_methods(Module) :-
	trust(Module instance_of module_s),
	\+ Module.is_class_or_interface, !.
gen_module_field_methods(Module) :-
	trust(Module instance_of module_s),
        % TODO: do not treat this differently!
	VField = ~findall(vfield(FName, Class), Module.field(FName, Class)),
	DField = ~findall(dfield(N, A), Module.data(_ModuleF, A, N)),
	append(VField, DField, Fields),
	( \+ Module.is_top_module, \+ DField = [] ->
	    throw(data_not_yet_allowed_in_nested)
	; true
	),
        Count = ~length(Fields),
	Module.add_instance_arity(Count),
	%
	( \+ Module.is_top_module ->
	    call((
              m :: module_s <- Module,
	      count :: any <- Count,
	      pos :: m_int <- 1,
	      %
	      gen_module_field_methods_(Fields)
	    ))
	; true
	).
	
% TODO: create a data structure for lists-with-length (which can be deforested for performance)
{
:- fluid m :: module_s.
:- fluid pos :: m_int + u.
gen_module_field_methods_([]).
gen_module_field_methods_([vfield(Name, Class)|Fields]) :-
	Visibility = ( m.exported_sym(Name) ? public | private ),
	gen_module_field_methods__get(Name, Class, Visibility),
	( new_modread_ctx(~m, class_has_mutable_statemodel(Class)) ->
	    gen_module_field_methods__set(Name, Class, Visibility)
	; true
	),
	pos.inc(1),
	gen_module_field_methods_(Fields).

% TODO: do not write getter and setters, add special builtins (and optimize)
gen_module_field_methods__get(Name, Class, Visibility) :-
	Module = ~m,
	trust(Module instance_of module_s),
	Get0 = ~atom_concat('$get_', Name),
	new_modread_ctx(Module, getter_props(Get0, Class, Visibility)),
	GetHead =.. [Get0, IValue, Value],
	module_instance_functor(Module, IName, IArity),
	functor(IValue, IName, IArity),
	arg(~pos, IValue, Value),
        Module.add_clause(GetHead, true).

gen_module_field_methods__set(Name, _Class, Visibility) :-
	Module = ~m,
	trust(Module instance_of module_s),
	Set0 = ~atom_concat('$set_', Name),
	new_modread_ctx(Module, setter_props(Set0, Visibility)),
	SetHead =.. [Set0, IValue, Value, IValue2],
	module_instance_functor(Module, IName, IArity),
	functor(IValue, IName, IArity),
	replace_arg(IValue, ~pos, Value, IValue2),
        Module.add_clause(SetHead, true).
}.
}.

{
:- extends modread_ctx.
% Properties of the getter method (no ctx):
%   [:- export(<Getter>).]
%   :- meta_predicate <Getter>(?, out(Metatype))
getter_props(Get0, Class, Visibility) :-
	Pred = ~envmod.pred_ref(Get0, 2),
	Pred.set_prop(owner_module_ctx(none)), % TODO: fix! implement a query_none_ctx in module_s which creates it, use pred_ref inside it
	( Visibility = public -> do_export(Get0/2) ; true ),
	Metatype = ~class_to_metatype(Class),
	( Metatype = ? ->
	    true
	; GetMeta =.. [Get0, ?, out(Metatype)],
	  do_meta_predicate(GetMeta)
	).
}.

{
:- extends modread_ctx.
% Properties of the setter method (no ctx):
%   [:- export(<Setter>).]
setter_props(Set0, Visibility) :-
	Pred = ~envmod.pred_ref(Set0, 3),
	Pred.set_prop(owner_module_ctx(none)), % TODO: fix! implement a query_none_ctx in module_s which creates it, use pred_ref inside it
	( Visibility = public -> do_export(Set0/3) ; true ).
}.

% Str is Str0 with argument number ArgI replaced by Arg
replace_arg(Str0, ArgI, Arg, Str) :-
	functor(Str0, N, A),
	functor(Str, N, A),
	i :: any <- ArgI,
	arg :: any <- Arg,
	str0 :: any <- Str0,
	str :: any <- Str,
	%
	replace_arg__2(1, A).

% TODO: use a loop
{
    :- fluid i :: any.
    :- fluid arg :: any.
    :- fluid str0 :: any.
    :- fluid str :: any.
    :- static replace_arg__2/2.
    replace_arg__2(I, A) :- I > A, !.
    replace_arg__2(I, A) :-
            arg(I, ~str0, X0),
	    X = ( I = ~i ? ~arg | X0 ),
            arg(I, ~str, X),
            I1 is I + 1,
            replace_arg__2(I1, A).
}.
:- elif(use_backend(js)).
gen_module_field_methods(_).
:- endif.

% ---------------------------------------------------------------------------
:- doc(subsection, "Flatten 'instance_of__' definitions").

{
:- extends errlog_ctx. % TODO: It should not be necessary.
gen_module_instance_of(Module) :-
	trust(Module instance_of module_s),
	( Module.get_prop(def_instance_of) ->
	    gen_module_instance_of_(Module)
	; true
	).

:- if(use_backend(bc)).
gen_module_instance_of_(Module) :-
	trust(Module instance_of module_s),
        Name = 'instance_of__',
	% TODO: split in several defs (id, associated type, etc.)?
	% TODO: too complicated, simplify
	% resolve the constructor and get its context
	new_modread_ctx(Module, instance_of_props(Name)),
	%
	% [[ instance_of__(X) :- var(X), !, fail ]]
	HeadVar =.. [Name, X],
	CodeVar = ~list_to_conj([var(X), !, fail]),
	Module.add_clause(HeadVar, CodeVar),
	% [[ instance_of__(<<f>>(_,...,_)) :- true ]]
	module_instance_functor(Module, IN, IA),
	functor(ModuleData, IN, IA),
	HeadOther =.. [Name, ModuleData],
	CodeOther = ~list_to_conj([]),
	Module.add_clause(HeadOther, CodeOther).
{
:- extends modread_ctx.
instance_of_props(Name) :-
	M = ~envmod,
	trust(M instance_of module_s),
	A = 1,
	_ = ~M.pred_ref(Name, A), % (register)
	do_export(Name/A),
	symmodif__set(static, Name/A),
	%
	Metatype = ~class_to_metatype(~M.get_name), % TODO: store resolved?
	MetaSpec =.. [Name, out(Metatype)],
	do_meta_predicate(MetaSpec).
}.
:- elif(use_backend(js)).
% TODO: merge with 'instance_of__'
% Add a static method '$check'(X) that succeeds X is an object of this
% class (for classes)
gen_module_instance_of_(Module) :-
	trust(Module instance_of module_s),
	Pred = ~Module.pred_ref('$check', 1),
	Pred.set_det(semidet),
	Pred.set_prop(specialdef([_], classtest(Module))),
	Pred.set_code(icode(a, [A], or([['\6\predid_apply'(Pred, [A])]]))).
:- endif.
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Flatten class/module constructors and initializers").

:- if(use_backend(bc)).
{
:- extends errlog_ctx.
gen_module_ctors(Module) :-
	trust(Module instance_of module_s),
	\+ Module.is_class_or_interface, !.
gen_module_ctors(Module) :-
	trust(Module instance_of module_s),
	get_constructors(Module, Ctors),
	m :: module_s <- Module,
	emit_ctors(Ctors).

get_constructors(Module, Ctors) :-
	trust(Module instance_of module_s),
	findall(constr(Name, Arity, FunName), Module.constr(Name, Arity, FunName), Ctors).

{
:- fluid m :: module_s.

emit_ctors([]).
emit_ctors([Ctor|Ctors]) :-
	emit_constr(Ctor),
	emit_ctors(Ctors).

% Emit the external static constructor function
emit_constr(constr(Name, A, FunName)) :-
%	display(user_error, constr(Name, A, FunName)), nl(user_error),
	A1 is A + 1,
	%
	New0 = FunName,
	Module = ~m,
	trust(Module instance_of module_s),
	% TODO: too complicated, simplify
	%
	% Resolve the constructor, get its context, use the
	% constructor context so that the wrapper has the same ctx.
	Pred = ~Module.pred_ref(Name, A),
	ConsContextR = ~Pred.envmod_ctx,
	new_modread_ctx(ConsContextR, constr_props(New0, A1)),
	%
	functor(Cons, Name, A),
	Cons =.. [_|Args],
	append(Args, [Res], ArgsNew),
	New =.. [New0|ArgsNew],
	call((
	  code :: accum(Code),
	  ClassName = ~Module.get_name,
	  Metatype = ~class_to_metatype(ClassName), % TODO: store resolved class?
	  ( Module.is_top_module ->
	      module_instance_functor(Module, IN, IA), NA = IN/IA,
	      code.add('$inst_new'(NA, This)),
	      code.add('$trust_metatype'(This, Metatype)),
	      Ret = (This = Res)
	  ; This = ~auxself,
	    code.add(This :: ClassName), % TODO: store resolved?
	    ( Module.get_prop(raw_state) -> % for raw_state % TODO: sure?
	        true
	    ; module_instance_functor(Module, IN, IA), NA = IN/IA,
	      functor(ModuleData, IN, IA),
	      % TODO: call functional expansion directly here, to avoid requiring fsyntax in every module using classes
	      code.add(~funcall(This) = ModuleData)
	    ),
	    Ret = (~funcall(This) = Res)
	  ),
	  ( Module.get_prop(needs_init_virtual) ->
	      code.add(~mcall(This, 'init_virtual__'))
	  ; true
	  )
	)),
	Code1 = ~list_to_conj(Code),
	Module.add_clause(New, (Code1, ~mcall(This, Cons), Ret)).
}.
}.

{
:- extends modread_ctx.
% Properties of the constructor predicate
constr_props(New0, A1) :-
	M = ~envmod,
	trust(M instance_of module_s),
	DefM = ~M.find_def_enclosing,
	ClassName = ~DefM.get_name,
	( ( DefM.is_top_module
	  ; TopM = ~M.top_enclosing_module,
	    TopM.exported_sym(ClassName) % TODO: lookup in the parent
	  ) ->
	    decl__treat(redefining(New0/A1)),
	    do_export(New0/A1)
	; true % TODO: we should always export (in the class)
	),
	%
	_ = ~M.pred_ref(New0, A1), % (register)
	% the wrapper is static
	symmodif__set(static, New0/A1),
	%
	A is A1 - 1,
	repeat_n(A, '?', MetaAs0),
	Metatype = ~class_to_metatype(ClassName), % TODO: store resolved class?
	append(MetaAs0, [out(Metatype)], MetaAs),
	MetaNew =.. [New0|MetaAs],
	do_meta_predicate(MetaNew).
}.

% TODO: partially duplicated in mexpand -- this one is using module_s instead of module_x
class_to_metatype(Class) := Class = 'blt__m_any' ? (?)
	                  | Class = 'blt__any' ? (?)
                          | Class.
:- elif(use_backend(js)).
{
:- extends errlog_ctx.
% Create the 'new__' method (that invokes 'cons__' if necessary)
% TODO: missing initialization of the static part of a class
% TODO: fix uses of get_super (many are not classes)
gen_module_ctors(Module) :-
	trust(Module instance_of module_s),
        % Do not emit any constructor or module initializer in those cases
	\+ Module.get_prop(static_noself),
	Base = ~Module.get_extends,
	( Base = ~module_s.lookup_module(basal_base) -> true
	; Base = ~module_s.lookup_module(basalnv_base) -> true
	; Base = ~module_s.lookup_module(nonvar_base) -> true
	; Base = ~module_s.lookup_module(var_base) -> true
	; Module.get_prop(simple_box(_CompareOperator)) -> true
	),
	!.
gen_module_ctors(Module) :-
	trust(Module instance_of module_s),
        % TODO: merge 'static_noself' case with the others
        ( Module.get_prop(static_noself) ->
	    emit_static_new_method(Module)
	; % '$alloc' method
	  emit_alloc_method(Module),
	  % initialization of attributes
	  emit_attr_init(Module),
	  %
	  emit_new_method(Module)
	),
	( \+ Module.get_prop(static_noself) ->
	    emit_module_id_wrapper(Module)
	; true
	).

% Wrapper to access this module from the enclosing module
% TODO: Wrong! This is sometimes generated at the 'root' module!
% TODO: We need a much better (and direct) way to do this -> see tests/js_backend/var_mods/var_mods.pl
% TODO: Avoid 'function wrapper for module'; it should be an atom
% - Function wrapper for the module
%   For non-instantiable modules:
%     :- global mod. ... mod = mod:'new__'(...)
%   for instantiable modules (classes):
%     mod(...) := mod:'new__'(...).
emit_module_id_wrapper(Module) :-
	trust(Module instance_of module_s),
	ModName = ~Module.get_name, % this is the source name of the module
	( Module.get_prop(static) ->
            % Wraps the instance 
	    AtModule = ~Module.enclosing_module,
	    AtModule.do_mod_attr(ModName, new_obj(ModName))
	; % Wraps the constructor
	  ( Module.has_constructor(CtorArity) -> true
	  ; CtorArity = 0
	  ),
	  CtorArity1 is CtorArity + 1,
	  NewAs = ~vars(CtorArity1),
	  Cons =.. [ModName|NewAs], % (NewAs comes from code above)
	  NewMethod =.. ['new__'|NewAs], % (NewAs comes from code above)
	  %
	  % TODO: 'ModName' should be a path from '\6\root' to avoid ambiguity
	  % TODO: injecting code into the enclosing module may not be a good idea
	  AtModule = ~Module.enclosing_module,
	  new_modread_ctx(AtModule, treat_clause(Cons, ':'(ModName, NewMethod), [], []))
	).

emit_static_new_method(Module) :-
	trust(Module instance_of module_s),
	constructor_head_and_init__static_noself(Module, MaybeSelf, NewMethod, InitCall),
	call((
	  code :: accum(InitCode0),
          mod_attr_inits(Module, MaybeSelf),
	  code.add(InitCall)
        )),
	list_to_conj(InitCode0, InitCode),
	Module.add_clause(NewMethod, InitCode).
}.

emit_alloc_method(Module) :-
	trust(Module instance_of module_s),
	Alloc = ~Module.pred_ref('$alloc', 1),
	Alloc.set_prop(static),
	Module.set_native_pred(Alloc, detfun, [box], [], [proceed_out1(new(ctor_lookup0(class, Module), []))]).

emit_attr_init(Module) :-
	trust(Module instance_of module_s),
	call((
	  code :: accum(InitCode0),
          mod_attr_inits(Module, yes(NewSelf))
        )),
	list_to_conj(InitCode0, InitCode),
	Module.add_clause('internal_cons__'(NewSelf), InitCode).

{
:- extends errlog_ctx.
emit_new_method(Module) :-
	trust(Module instance_of module_s),
	constructor_head_and_init(Module, NewSelf, NewSelf0, NewMethod, InitCall),
	call((
          code :: accum(NewCode0),
          code.add('$alloc'(NewSelf0)),
	  Base = ~Module.base,
	  ( Base = ~module_s.lookup_module(':'('basiccontrol', 'nonvar')) ->
	      true
	  ; % TODO: BaseName incorrectly set
	    code.add(':'(~Module.get_extends, 'internal_cons__'(NewSelf0)))
	  ),
	  code.add('internal_cons__'(NewSelf0)),
	  code.add(InitCall),
	  code.add((NewSelf = NewSelf0)) % TODO: is temporal unification necessary?
        )),
	list_to_conj(NewCode0, NewCode),
	% 'new__' method
	new_modread_ctx(Module, treat_clause(NewMethod, NewCode, [], [])).
}.

% TODO: allow more than one constructor arity
constructor_head_and_init(Module, NewSelf, NewSelf0, NewMethod, InitCall) :-
	trust(Module instance_of module_s),
        ( Module.has_constructor(CtorArity) ->
	    functor(InitMethod, 'cons__', CtorArity),
	    InitMethod =.. [_|NewAs],
	    NewMethod =.. ['new__'|(~append(NewAs, [NewSelf]))],
	    InitCall = ~mcall(NewSelf0, InitMethod)
        ; NewMethod = 'new__'(NewSelf), InitCall = true
	).

{
:- extends errlog_ctx.
constructor_head_and_init__static_noself(Module, MaybeSelf, NewMethod, InitCall) :-
	trust(Module instance_of module_s),
        MaybeSelf = no,
	( Module.has_constructor(CtorArity) ->
	    ( CtorArity =\= 0 ->
	        ft_error(no_arguments_allowed_in_static_noself_initializers(Module))
	    ; true
	    ),
	    InitCall = 'cons__'
	; InitCall = true
	),
	NewMethod = 'static_noself_new__'.
}.

% Initialize the module/class attributes
{
:- fluid code :: accum.
mod_attr_inits(Module, MaybeSelf) :-
	trust(Module instance_of module_s),
        Inits = ~Module.all_mod_attr_inits,
        mod_attr_inits__2(Inits, MaybeSelf).

mod_attr_inits__2([], _).
mod_attr_inits__2([attr_init(Init, AttrInitVal)|As], MaybeSelf) :-
        code.add(~maybe_mcall(MaybeSelf,Init)), % (low-level creation of attribute)
        ( AttrInitVal = new_obj(ModuleName) ->
            code.add(~funcall(~maybe_mcall(MaybeSelf,ModuleName)) = ~funcall(':'(ModuleName, 'new__')))
	; AttrInitVal = no ->
	    true
	; throw(bug_bad_attr_init(AttrInitVal))
        ),
	mod_attr_inits__2(As, MaybeSelf).
}.

maybe_mcall(yes(X), Y) := ~mcall(X, Y).
maybe_mcall(no, Y) := Y.
:- endif.

% ---------------------------------------------------------------------------
:- doc(subsection, "Apply 'extends' to modules").

:- if(use_backend(bc)).
{
:- extends errlog_ctx.
apply_extends_to_module(Module) :-
	trust(Module instance_of module_s),
	m :: module_s <- Module,
	Bases0 = ~Module.all_extends, % (unresolved)
	new_modread_ctx(Module, Bases = ~pli_resolve_classes(Bases0)),
        maplist((''(X) :- module_extend(X)), Bases).
{
:- fluid m :: module_s.

% TODO: This extend should be done in the 'compile' part, not here.
module_extend(BaseModule) :-
	trust(BaseModule instance_of module_s),
	m.add_extendsR(~BaseModule.get_id),
	( ~BaseModule.get_id = 'serializable' ->
	    extends_serializable
	; BaseModule.is_class ->
	    fill_virtual_entries(BaseModule)
	; BaseModule.is_interface ->
	    add_interface_methods(BaseModule)
	; % TODO: bug or error?
	  ft_error(bug(cannot_extend(~m.get_id, ~BaseModule.get_id)))
	).
}.
}.

{
:- extends errlog_ctx.
:- fluid m :: module_s.
% Fill the virtual entries of BaseName
% TODO: This is a kludge, implement at a lower level
:- meta_predicate fill_virtual_entries(module_s).
fill_virtual_entries(BaseModule) :-
	% TODO: wrong! 'base' must be searched (we may override a predicate deeper in the inheritance chain)
	base :: module_s <- BaseModule,
	Overrides = ~all_overrides,
	cvar :: any <- ~auxself,
	call(( goals :: accum(Goals0), fill_virtual_entries__2(Overrides) )),
	list_to_conj(Goals0, Goals),
	BaseName = ~BaseModule.get_name, % TODO: find better way to do unexpansion (or better, avoid it)
	Body = (~cvar :: BaseName, ~funcall(~cvar) = ~funcall('self'), Goals),
%	display(user_error, o(Body)), nl(user_error),
	%
	Module = ~m,
	trust(Module instance_of module_s), % TODO: this should not be necessary
	Module.set_prop(needs_init_virtual),
	new_modread_ctx(Module, treat_clause(init_virtual__, Body, [], [])).

{
:- fluid base :: module_s.
all_overrides := All :-
	Module = ~m,
	Base = ~base,
	All = ~findall(o(N,A,N2), find_override(N, A, Module, Base, N2)).
}.

% (nondet)
% Find methods that are virtual in the base
find_override(F, A, Module, Base, NF) :-
	trust(Module instance_of module_s),
	trust(Base instance_of module_s),
	Module.method(F, A, NF),
	Pred = ~Base.pred_ref_noreg(F, A),
	Pred.get_prop(virtual).

{
:- fluid base :: module_s.
:- fluid cvar :: any.
:- fluid goals :: accum.
fill_virtual_entries__2([]).
fill_virtual_entries__2([X|Xs]) :-
	fill_virtual_entry(X),
	fill_virtual_entries__2(Xs).

fill_virtual_entry(o(N, A, N2)) :-
	% TODO: wrong! BaseName must be searched
	Base = ~base,
	trust(Base instance_of module_s),
	( Pred = ~Base.pred_ref_noreg(N, A),
	  Pred.get_prop(virtual) ->
	    AttrV = ~atom_concat(N, '__v'),
	    Pred.get_prop(owner_module_ctx(Context))
	; ft_error(bug(not_virtual(N, A, ~m.get_id))) % TODO: bug or error?
	),
	L = ~funcall(~mcall(~cvar, AttrV)),
	R = ~funcall('$meta_exp'(primitive(pred(A, Context)), N2)),
	goals.add(L = R).
}.
}.

{
:- fluid m :: module_s.
% TODO: This is not the right solution for virtual methods and interfaces
add_interface_methods(BaseModule) :-
	trust(BaseModule instance_of module_s),
	findall(m(F0, A), BaseModule.method(F0, A, _), Ms),
	maplist((''(M) :- add_interface_method(M, BaseModule)), Ms).

% Create a jump method
add_interface_method(m(F0, A), BaseModule) :-
	Module = ~m, trust(Module instance_of module_s),
	functor(Head0, F0, A),
	% SelfGet = '$self'(<InstanceFunctor>(...))
	module_instance_functor(Module, IN, IA),
	functor(ModuleData, IN, IA),
	SelfName = ~self_name,
	SelfGet0 =.. [SelfName, ModuleData],
	( \+ Module.is_class_or_interface ->
	    SelfGet = SelfGet0
	; % Add cast (necessary)
	  ModuleId = ~Module.get_id,
	  SelfGet = (SelfGet0, '\6\fluid_class'(SelfName, ModuleId))
	),
	% Do SelfGet and then jump to Head0
	% TODO: strange, Head0 is added to BaseModule but resolved in Module (Head0 is not qualified)
	Body = (SelfGet, Head0),
	% note: this bypass assigning a context to the predicates
	trust(BaseModule instance_of module_s),
	BaseModule.add_clause(Head0, Body).
}.

% Name of a temporal cvar
% TODO: think in alternatives that do not use cvars, this may capture other cvars!
auxself := '$auxself'.

:- elif(use_backend(js)).
apply_extends_to_module(_).
:- endif.

% ---------------------------------------------------------------------------

:- if(use_backend(bc)).
% Term encoding the module state

% TODO: [URGENT] For instdata, create a 'data' array instead of
%       multiple entries. There is no additional cost when the array
%       is accessed from C and it will make state descriptions *much*
%       smaller.
%
%       Steps:
%        - add 1 if there is some instdata and 0 if not
%        - create and destroy all instdata at once

% The term that stores the instance of a class
% TODO: Generalize for other representations (including 'raw_state' and attributes as data)
:- meta_predicate module_instance_functor(module_s, ?, ?).
module_instance_functor(Module, Name, Arity) :-
	( Module.instance_arity(Count) -> true
	; Count = 0
	),
	Arity = Count,
	Name = ~Module.get_id. % TODO: unify with get_objfunctor in dynlink.c
:- elif(use_backend(js)).
:- endif.

% ---------------------------------------------------------------------------
:- doc(subsection, "'serializable' mixin").
% TODO: Move to the source level
% TODO: Only works for the outmost class, fix.

:- if(use_backend(bc)).
{
:- extends errlog_ctx.
:- fluid m :: module_s.
extends_serializable :-
	Module = ~m,
	trust(Module instance_of module_s),
	( \+ Module.is_top_module ->
	    ft_error(bug(only_top_module_is_serializable))
	; true
	),
	new_modread_ctx(Module, extends_serializable_).
}.

{
:- extends modread_ctx.
extends_serializable_ :-
	do_export(write_to_file/1),
	treat_clause(write_to_file(Name), ('$self'(This), '$inst_write'(Name, This)), [], []),
	do_export(read_from_file/2),
	symmodif__set(static, read_from_file/2),
	Module = ~envmod,
	trust(Module instance_of module_s),
	module_instance_functor(Module, IN, IA), NA = IN/IA,
	treat_clause(read_from_file(Name, This), ('$inst_read'(Name, NA, This)), [], []).
}.
:- endif.
