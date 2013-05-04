:- module(_, [], [pure, compiler(complang)]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(basic_props)).
:- use_module(engine(term_basic)).
:- use_module(compiler(store), [denormalized_spec/2]).

% ===========================================================================

:- public runtime_error/3.
runtime_error(not_defined(F, N, M), warning,
        ['Predicate ',~~(F/N),' undefined in module ',M]) :- !.
runtime_error(not_imported(F, N, M, QM), warning,
	['Module qualification of ',~~(F/N),
	 ' ignored, module ',M,
	 ' does not import the predicate from module ',QM]) :- !.
runtime_error(Error, Type, Message) :- !,
	error(Error, Type, Message).

% ===========================================================================
% The errors and warnings of the different compiler passes (including
% runtime errors).

% TODO: make multifile and discontiguous?
:- public error/3.

% ---------------------------------------------------------------------------
% Internal errors
error(internal(Msg), error,
        ['Internal error: ', Msg]). % undocumented internal errors
error(bug(Msg), error, % TODO: define bug/1?
        ['Bug: ', Msg]). % bugs in the compiler
% ---------------------------------------------------------------------------
% Parser
error(syntax_error(Msg, ErrorLoc), error,
        ['Syntax error: ',[](Msg),'\n'|ErrorLoc]).
% ---------------------------------------------------------------------------
% Module system (modules and their contents)
error(bad_module_name(Modifier, M), error,
        ['Bad name ',M,' in ', ~~(~modifier_to_keyword(Modifier)), ' declaration']).
error(module_modifier_mismatch(WantedModifier, FoundModifier), error, % TODO: JS-backend (integrate in both directions)
	['Expecting a ', ~~(~modifier_to_keyword(WantedModifier)),
	 ', found ', ~~(~modifier_to_keyword(FoundModifier)), ' declaration']). % TODO: translate modifier to keyword
error(bad_module_decl_name(Modifier, WantedN, FoundN), error, % TODO: JS-backend (integrate in both directions)
	['Name mismatch in ', ~~(~modifier_to_keyword(Modifier)),
	 ' declaration, expected \'', ~~(WantedN), '\' but found \'', ~~(FoundN), '\'']).
error(nonstarting(F,A), error,
        ['Declaration ',~~(F/A),' not starting file']).
error(bad_export(SymSpec), error,
        ['Bad symbol indicator ',~~(SymSpec),' in export']).
error(bad_use_module(Spec), error,
        ['Bad/unreadable file ',~denormalized_spec(Spec),' in use_module declaration']).
error(all_user, error,
        ['Attempt to import all user predicates']). % TODO: unused...
error(bad_import_list(Bad), error,
        ['Bad import/reexport list ',~~(Bad)]).
error(bad_import_spec(Predspec), error,
        ['Bad predicate indicator ',~~(Predspec),' in import/reexport']).
error(bad_import(Module), error,
        ['Bad module ',Module,' in import declaration']).
error(not_exported(IM,F/A), error,
        ['imported predicate ',~~(F/A),' not exported by ',IM]).
error(module_redefined(OtherSpec,M), error,
        ['Module ',M,' already defined in source ',~denormalized_spec(OtherSpec)]).
error(should_import(F,A,Module), error,
        ['This module should import ',~~(F/A),' from ',Module]).
error(not_defined(F, N,_M), error,
        ['Predicate ',~~(F/N),' undefined in source']).
error(pred_not_found_in_scope(Sym, Scope), error, % TODO: JS-backend (integrate in both directions)
        ['Predicate ', ~~(Sym), ' not found in ', Scope]).
error(not_imported_mod(F, N,_M, QM), error,
        ['Predicate ',~~(F/N),
         ' not imported from module ',QM]).
error(not_imported_path(F, N,_M, Path), error,
        ['Predicate ',~~(F/N),
         ' not imported from access-path ',Path]).
error(imported_needs_qual(F, N, M), warning,
        ['Unqualified predicate call to ',~~(F/N),
         ' assumed to local version, calls to predicate imported from ',M,
         ' must be qualified']).
error(imported_needs_qual(F, N, M0, M), warning,
        ['Unqualified predicate call to ',~~(F/N),
         ' assumed to module ',M,', calls to predicate imported from ',M0,
         ' must be qualified']).
error(exported_not_defined(F/A), warning,
        ['Exported predicate ',~~(F/A),' is not defined in this module']).
error(missing_module_decl, warning,
        ['Source used as module without module declaration']).
error(export_multifile(F/A), warning,
	['No need to export multifile predicate ',~~(F/A)]).
error(redefine_pred(F, A), error,
	['Attempt to redefine ',''(F/A)]).
error(already_defined(F/A, A0), warning,
        ['Predicate ',~~(F/A), ' is already defined with arity ',A0]).
error(discontiguous(F/A), warning,
        ['Clauses of ',~~(F/A), ' are discontiguous']).
error(usermod_import(Spec, yes, no), warning,
	['User file ', ~denormalized_spec(Spec),
	 ' was used as module (use ensure_loaded instead)']) :- !.
error(usermod_import(Spec, no, yes), warning,
        ['Module ', ~denormalized_spec(Spec), 
         ' was imported with ensure_loaded (use use_module instead)']) :- !.
error(runtime_expansions, warning,
	['Module use runtime expansions, which can affect performance and disable most analyses and optimizations in the module, use pragma allow_runtime_expansions to disable this warning or consider using meta_predicate declarations if possible']) :- !.
error(runtime_expansion_culprit(Term, KnownMetatype, ExpectedMetatype), warning,
	['Term ', Term, ', with known metatype ', KnownMetatype, ', will be checked/expanded at runtime as metatype ', ExpectedMetatype]) :- !.
% ---------------------------------------------------------------------------
% Errors for other extensions
error(bad_fun_eval_spec(Spec), error,
        ['Bad indicator ',~~(Spec),' in fun_eval declaration']).
% ---------------------------------------------------------------------------
% Module system extensions (classes, instances, contexts, etc.)
error(base_class_not_found_in_scope(Sym, Scope), error, % TODO: JS-backend (integrate in both directions)
        ['Base class ', ~~(Sym), ' not found in ', Scope]).
%%%
error(bad_class_ctor_name(M, F0, A), error,
	['Constructor ', ~~(F0/A), ' of class ', M, ' should end in underscore (\'_\')']).
% TODO: deprecate
error(bad_predicate_context(F, A, Context), error,
	['Bad context ', ~~(Context), ' for predicate ', ~~(F/A)]).
% (contexts: quite low level; use mixins, scopes, and fluids instead)
error(bad_binder_head(Head), error,
	['Bad binder head ', ~~(Head)]).
error(bad_module_statemodel(X), error,
	['Bad state model ', ~~(X)]).
error(decl_needs_class(DeclSpec), error,
	['Declaration (:- ', ~~(DeclSpec), ') can only appear in a class']).
error(bad_metatype(Metatype), error,
        ['Bad metatype ',~~(Metatype), ' (meta_predicate specification)']).
error(cannot_connect_ctx(Where, RequiredContext, CurrentContext), error,
        ['In ',~~(Where),
	 ', cannot connect the required context:\n  ', ~~(RequiredContext),
	 '\nwith the current context:\n  ', ~~(CurrentContext)]).
% Special scope variables (seen as predicates, for global variables,
% class attributes, fluid variables, etc.).
% TODO: simplify this part
error(bad_ctx_var_name(Var), error,
	['\'', ~~(Var), '\' is not a valid ctx_var name']).
error(ctx_var_not_in_scope(Var), error, 
	['\'', ~~(Var), '\' is not a ctx_var in scope']).
error(ctx_var_not_updatable(Var), error, 
	['\'', ~~(Var), '\' is not an updatable ctx_var in scope']).
% scope variables and invokation of predicate abstractions
error(pa_ctx_var_not_compatible(Var, PrevClass, Class), error,
        ['In call of predicate abstraction, ctx_var ',~~(Var),
	 ' class ', ~~(PrevClass), ' is not compatible with ',
	 ' class ', ~~(Class), ' is not compatible with ']).
error(pa_ctx_var_not_in_scope(Var), error, 
	['In call of predicate abstraction, required ctx_var \'', ~~(Var),
	 '\' is not in scope']).
% Iterators and scope modifiers
error(bad_ctx_enter(Ctor), error, 
	['Unrecognized scope modifier or iterator \'', ~~(Ctor), '\'']).
error(unknown_ctx_enter(Ctor), error, 
	['Unknown scope modifier or iterator \'', ~~(Ctor), '\'']).
error(bad_iterator(Iter), error,
        ['Bad for_each iterator ',~~(Iter)]).
error(bad_spec_in_maplistn(Lists, Elems), error,
        ['Bad maplist iterator ',~~(Lists),' ', ~~(Elems)]).
% ---------------------------------------------------------------------------
% Custom declarations
error(bad_newdecl(Predspec, Visibility), error,
        ['Bad new declaration definition ',~~(Predspec),' with visibility ', Visibility]).
error(incompatible_decl(F/A,Decl,Decl2), error,
        ['predicate ',~~(F/A),' is being defined ',Decl,
                       ' but it was already defined ',Decl2]).
error(decl_failed(Decl), error,
	['Declaration processing failed ',~~(Decl)]).
error(unknown_decl(Decl), error,
	['Unknown declaration ',~~(Decl)]).
error(decl_failed_warning(Decl), warning,
	[Decl,' - declaration failed']).
% ---------------------------------------------------------------------------
% Predicates and predicate properties
% (consider 'module' errors for those related to a predicate in its
% definition context)
error(bad_redefining(SymSpec), error,
	['Bad symbol indicator ',~~(SymSpec),' in redefining']).
error(bad_symspec_decl(SymSpec), error,
        ['Bad symbol indicator ',~~(SymSpec),' in declaration']).
error(bad_meta_predicate(Predspec), error,
        ['Bad meta_predicate specification ',~~(Predspec)]).
error(conflicting_meta_predicate(Predspec), error,
        ['Conflicting meta_predicate specification ',~~(Predspec)]).
error(incompatible_multifile(F/A,T,T1,M1), error,
        ['multifile predicate ',~~(F/A),' is defined ',T,
         ' while in module ',M1,' is defined ',T1]).
% TODO: impnat=_ syntax is not nice
error(deprecated_impl_defined(F, A), warning,
	['impl_defined is deprecated: remove it for foreign predicates, or replace it by properties impnat=intrinsic or impnat=indefinable for predicate ',~~(F/A)]).
error(malformed_body(F, A), error,
	['Malformed body in ',''(F/A)]).
error(illegal_clause, error,
	['Illegal clause']).
error(bad_singletons(BadSingletons, F/A), warning,
        [BadSingletons,' - singleton variables in ',~~(F/A)]).
% ---------------------------------------------------------------------------
% Predicate abstractions
error(bad_pred_abs(PA), error,
        ['Bad predicate abstraction ',~~(PA),
         ' : head functor should be ''''']).
error(big_pred_abs(PA,N), error,
        ['Predicate abstraction ',~~(PA),
         ' has too many arguments: should be ',N]).
error(short_pred_abs(PA,N), error,
        ['Predicate abstraction ',~~(PA),
         ' has too few arguments: should be ',N]).
% ---------------------------------------------------------------------------
% Packages
error(bad_package_file(F), error,
        ['Bad package file ',~~(F)]).
% ---------------------------------------------------------------------------
% Compilation (the compilation process itself)
error(reexport_loop(Loop), error,
	['Reexportation loop: ', Loop,' -- aborting']).
% TODO: needed?
error(compiler_loop(File), error,
        ['COMPILER LOOP: ',File,' uses for compilation a file which ',
           'depends on it -- aborting']).
error(bad_file(File), error,
        ['Bad/unreadable file ',~~(File),' in declaration']).
error(not_found(Action), error,
        ['not found ',~action_desc(Action)]).
error(unable_to_create(File), error,
	['Unable to create ',File,' - aborting...']).
error(permission_error(File), error,
        ['Cannot open ',File,' - aborting...']).
error(clause_not_expanded(H, B), error,
	[H,' :- ',B,' - clause failed to expand']).
error(load_compilation_module_failed(Spec), error,
	['could not load compilation module ', ~denormalized_spec(Spec)]).
% ---------------------------------------------------------------------------
% Linker errors
error(main_not_exported(Spec), error,
	['main/0 or main/1 not exported by ', ~denormalized_spec(Spec)]).
error(warn_early_load(M), warning,
	['module ', M, ' will be loaded eagerly']).
error(executable_generation_aborted, error,
	['Executable generation aborted']).
error(refuse_to_overwrite_source(PlName), error,
	['Cowardly refuse to overwrite source ', PlName, ' with its executable']).
% ---------------------------------------------------------------------------
% Experimental -- (jfinst)
% TODO: review
error(instantiable_from_incompatible(F, A), error,
	['Experimental: calling instantiable predicate ',~~(F/A),' from incompatible predicate']).
% ---------------------------------------------------------------------------
% Assertions
error(assertion_syntax(AssrtType, UBody), error,
	[AssrtType, ' assertion syntax: ', UBody]).
error(assertion_syntax_for(AssrtType, PD), error,
	[AssrtType, ' assertion syntax for ', PD]).
error(mode_not_sufficiently_instantiated(Call), error,
	['Principal functor not sufficiently instantiated in mode: ', Call]).
error(arity_mismatch(F, A), error,
	['Arity mismatch in declaration for ', ~~(F/A)]).
error(not_prop(F, A), error,
	[~~(F/A), ' is not a property']).
% ---------------------------------------------------------------------------
% Foreign interface
error(dup_foreign(PredName), error,
	['More than one foreign or native property for ', ~~(PredName)]).
error(invalid_foreign_name(PredName), error,
	['Invalid foreign/native function name for ', ~~(PredName)]).
error(invalid_foreign_arg(PredName), error,
	['Invalid argument name in ', ~~(PredName)]).
error(invalid_foreign_ret(PredName), error,
	['returns/2 with invalid argument in predicate ', ~~(PredName)]).
error(dup_returns(PredName), error,
	['More than one returns/2 property in predicate ', ~~(PredName)]).
error(not_foreign_output(VarName, PredName), error,
	[VarName, ' is not an output argument in predicate ', ~~(PredName)]).
error(invalid_foreign_size_of(PredName), error,
	['Invalid size_of property in predicate ', ~~(PredName)]).
error(not_unique_foreign_size_of(VarName, ~~(PredName)), error,
	['Variable ', VarName, ' in predicate ', ~~(PredName), ' needs a (only one) size_of/3 property']).	
error(invalid_do_not_free(PredName), error,
	['Invalid do_not_free/2 property in predicate ', ~~(PredName)]).
error(invalid_foreign_status(PredName), error,
	['Assertions of foreign predicate ', ~~(PredName), ' cannot be checked']).
% ---------------------------------------------------------------------------
% Analysis
error(invalid_call(PredName, CallTypes, ActualTypes), error,
	['Predicate ', ~~(PredName), ' requires ', CallTypes, ' but is called with ', ActualTypes]).
% ---------------------------------------------------------------------------
% Architecture dependant compilation
error(cc_failed(Action), error,
	['Failed compilation of '| ~action_output_name(Action)]).
% ---------------------------------------------------------------------------
% Toplevel
error(declaration_failed(X), warning,
        [X,' - declaration failed']).
error(directive_not_allowed_in_toplevel(F, A), warning,
        [~~(F/A),' directive not allowed in toplevel']).

% ===========================================================================

:- public action_message/2.
action_message(split(Spec)) := ['Reading ', ~denormalized_spec(Spec)].
action_message(compile(Spec)) := ['Compiling (noarch) ', ~denormalized_spec(Spec)].
action_message(archcompile(Spec)) := ['Compiling (arch) ', ~denormalized_spec(Spec)].
action_message(load(Spec)) := ['Loading ', ~denormalized_spec(Spec)].
action_message(use_package(Spec)) := ['Using package ', ~denormalized_spec(Spec)].
action_message(include(Spec)) := ['Including ', ~denormalized_spec(Spec)].
action_message(usemod(Spec)) := ['Using module ', ~denormalized_spec(Spec)].

:- public action_output_name/2.
action_output_name(compile__c(Spec)) := ['autogenerated C code for ', ~denormalized_spec(Spec)].
action_output_name(Action) := [Action]. % TODO: Finish or emit error?

% ===========================================================================

% TODO: Move to its right place.
modifier_to_keyword(mod_static, 'module').
modifier_to_keyword(mod_class, 'class').
modifier_to_keyword(mod_mixin, 'mixin').
modifier_to_keyword(mod_interface, 'interface').

% ===========================================================================

% TODO: Find better name (it should be 'name of the object specified by Action')
action_desc(prolog_source(Spec)) :=
	[](['source ', ~denormalized_spec(Spec)]) :- !.
action_desc(prolog_package(Spec)) :=
	[](['package file ', ~denormalized_spec(Spec)]) :- !.
action_desc(Action) := []([Action]). % TODO: write more...
