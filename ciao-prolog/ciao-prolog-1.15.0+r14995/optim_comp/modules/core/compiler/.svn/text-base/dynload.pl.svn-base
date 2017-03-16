% TODO: document: implementation of dynamic use_module (note that this is more complicated than simply loading a module using 'memoize')
:- module(_, [], [compiler(complang)]).

:- use_module(compiler(memoize)).
:- use_module(compiler(store)).
:- use_module(compiler(errlog)).
:- use_module(compiler(global_pass)).
:- use_module(compiler(module_ideps)).

% TODO: this code is not very clean, check the code that takes Mod from Spec, probably it does not work with user modules
% TODO: check...
:- use_module(engine(rt_exp)).
:- use_module(engine(dynlink)).

% ---------------------------------------------------------------------------
% Initialization of the dynamic loader

% TODO: use global vars to store memoize? to do that initialization code must not be followed by fail
%:- use_module(engine(internals), ['$global_vars_set'/2, '$global_vars_get'/2]).

% Default memoize object
:- data memo/1. 

:- initialization(init).

init :-
	Errs = ~errlog.new,
	Errs.add(verbose(off)),
	Memo = ~memoize.new(Errs),
%	'$global_vars_set'(5, Memo).
	asserta_fact(memo(Memo)).

:- public get_memo/1.
%:- meta_predicate get_memo(out(memoize)). % TODO: bug, does not find 'memoize'
get_memo(Memo) :-
%	'$global_vars_get'(5, Memo).
	memo(Memo).

% ---------------------------------------------------------------------------
% Dynamic module loading 

:- public use_module/1.
:- '$context'(use_module/1, module).
use_module(Spec) :-
	'$module'(ByThisModule),
	usemod(Spec, all, ByThisModule, no).

:- public use_module/2.
:- '$context'(use_module/2, module).
use_module(Spec, Imports) :-
	'$module'(ByThisModule),
	usemod(Spec, Imports, ByThisModule, no).

:- public use_module/3.
:- '$context'(use_module/3, module).
% TODO: strange... avoid a name clash (the previous use_module
%       predicate is expanded with an extra argument...
% TODO: use special names for context-expanded predicates!
use_module(Spec, Imports, ByThisModule) :-
	usemod(Spec, Imports, ByThisModule, no).

:- public ensure_loaded/1.
:- '$context'(ensure_loaded/1, module).
ensure_loaded(Spec) :-
	'$module'(ByThisModule),
	usemod(Spec, all, ByThisModule, yes).

:- public ensure_loaded/2.
% TODO: strange... avoid a name clash (the previous predicate is expanded with an extra argument... todo: use special names for context-expanded predicates!!!)
:- '$context'(ensure_loaded/2, module).
ensure_loaded(Spec, ByThisModule) :-
	usemod(Spec, all, ByThisModule, yes).

usemod(Uspec, Imports, ByThisModule, LoadedAsUsermod) :-
	store:find_source(Uspec, relpath('.'), Spec),
	get_memo(Memo),
	trust(Memo instance_of memoize),
	Memo.enter,
	Errs = ~Memo.errs,
        Ok = ( Errs.protect(usemod(Spec), usemod__2(Spec, Imports, ByThisModule, LoadedAsUsermod, Memo)) ?
	         yes
	     | no
	     ),
	Memo.leave,
	Ok = yes.

% TODO: document, or change by a prolog_flag (it is used in comp.pl)
:- public '$only_static'/0.
:- dynamic '$only_static'/0.

% TODO: calling usemod__2 twice may be faster than calling usemod... because it does not clean the memoize cache
usemod__2(ImportedSpec, Imports, ByThisModule, LoadedAsUsermod, Memo) :-
	trust(Memo instance_of memoize),
	% Load imported and related modules
	% TODO: this is not as clean as it could be...
	( '$only_static',
	  \+ static_spec(ImportedSpec) ->
	    errlog:temperror(['dynamic module loading of ', ImportedSpec, ' failed - this feature is disabled in this compiler executable'])
	; true
	),
	% Load code and related modules
        % (cut the tree search at static modules (it is correct since the 
        %  modules imported by static modules are also static modules))
	( call((
            memo :: any <- Memo,
	    global_pass:transitive_compile_stop([ImportedSpec], static_spec, Specs)
          )) ->
	    ( load_modules(Specs, Memo) ->
	        true
	    ; % TODO: unload any module???
	      % TODO: we should have a module reference count here to unload correct modules that are not needed by anyone
	      % remove dyn imports is not done if the imported module has not been loaded % TODO: correct???
	      ( spec_to_key(ImportedSpec, ImportedSpecKey),
		current_speckey_module(ImportedSpecKey, ImportedModule) ->
		  remove_dyn_imports(ByThisModule, ImportedModule)
	      ; true
	      ),
	      fail
	    )
	; fail
	),
	% Initialize newly loaded modules
	% TODO: unload if any initialization failed?? (can initializations fail currently?)
	( spec_to_key(ImportedSpec, ImportedSpecKey),
	  current_speckey_module(ImportedSpecKey, ImportedModule) ->
	    true
	; % TODO: unload modules when this is found!! (problem: 'module' garbage collection)
	  errlog:temperror(['spec mismatch for ', ImportedSpec]),
	  fail
	),
        do_initialization(ImportedModule),
	% note: from here, this predicate may fail and the module code won't be removed
        % Check and update imports
        % TODO: share some code with definition in 'compile' part of compiler/frontend.pl??
        check_update_dyn_imports(ByThisModule, Imports, ImportedModule, Memo),
	% Check that the module is imported with the correct directive
	% TODO: share some code with definition in the 'compile' part of compiler/frontend.pl??
	IsUsermod = ( '$user_module_id'(ImportedModule) ? yes | no ),
	( LoadedAsUsermod = IsUsermod ->
	    true
	; Errs = ~Memo.errs,
	  Errs.compiler_error(usermod_import(ImportedSpec, IsUsermod, LoadedAsUsermod))
	).

% Check and update imports
% TODO: to be completely correct shouldn't we check that ALL dynamic imports are satisfied (and not only the ones of ByThisModule)? (I know it's very costly...)
check_update_dyn_imports(M, Imports, IM, Memo) :-
	trust(Memo instance_of memoize),
	Errs = ~Memo.errs,
	% Check imports
	check_import_list(Imports, IM, Errs),
	\+ Errs.get1_module_error,
	% Update metadata
        update_dyn_imports(M, Imports, IM).

% Check errors in the imported list
check_import_list(all, _ImportedModule, _Errs) :- !.
check_import_list(Xs, ImportedModule, Errs) :- list(Xs), !,
	check_import_list__2(Xs, ImportedModule, Errs).
check_import_list(Xs, _ImportedModule, Errs) :-
	trust(Errs instance_of errlog),
	Errs.compiler_error(bad_import_list(Xs)).

% TODO: the errors are a bit strange... what line number???
check_import_list__2([], _IM, _Errs).
check_import_list__2([X|Xs], IM, Errs) :-
	trust(Errs instance_of errlog),
	( X = F/A, atom(F), number(A) ->
	    ( exported(F/A, IM) -> % TODO: sometimes poor indexing...
	        true
	    ; Errs.compiler_error(not_exported(IM, F/A))
	    ),
	    check_import_list__2(Xs, IM, Errs)
	; Errs.compiler_error(bad_import_spec(X))
	).

% TODO: can user modules be static?? (this won't work)
% TODO: hmm use modules instead of specs?
static_spec(Spec) :-
	spec_to_key(Spec, SpecKey),
	current_speckey_module(SpecKey, Module),
	!,
	static_module(Module),
	!.

load_modules([], _Memo).
load_modules([Spec|Specs], Memo) :-
	load_module(Spec, Memo),
	load_modules(Specs, Memo).

load_module(Spec, Memo) :-
	trust(Memo instance_of memoize),
	Memo.eval0(load(Spec)).

% ---------------------------------------------------------------------------
% Module unload

% TODO: bad name...
% TODO: only unload when there is no module reference...
% TODO: offer a version which does not use the relpath ('.')
:- public unload/1.
:- '$context'(unload/1, module).
unload(Uspec) :-
	store:find_source(Uspec, relpath('.'), Spec),
	spec_to_key(Spec, SpecKey),
	current_speckey_module(SpecKey, Module),
	module_timestamp(Module, _), !, % TODO: replace by current_module?
	'$module'(ByThisModule),
	remove_dyn_imports(ByThisModule, Module),
	unload_module__nocheck(Module).
