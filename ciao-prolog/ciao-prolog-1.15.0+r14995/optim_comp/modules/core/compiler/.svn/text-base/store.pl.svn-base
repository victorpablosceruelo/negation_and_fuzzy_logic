% Store interface for source files and intermediate compilation results
% 
% --jfran
%
% store n.: 
%   2. A stock or supply reserved for future use: a squirrel's store of acorns.
%   4. A place where commodities are kept; a warehouse or storehouse.
:- module(_, [], [pure, compiler(complang)]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(term_compare)).
:- use_module(engine(arithmetic)).
:- use_module(engine(atomic_basic)).
:- use_module(engine(data_facts)).
:- use_module(engine(system_info)).
:- use_module(engine(prolog_flags)).
:- use_module(engine(basic_props)).

:- use_module(compiler(errlog)).
:- use_module(compiler(memoize)).

% TODO: Deprecated?
:- multifile file_search_path/2.
:- dynamic file_search_path/2.

% TODO: Deprecated?
:- multifile library_directory/1.
:- dynamic library_directory/1.

% TODO: Deprecated?
% TODO: strange, someone may want to retract this fact
file_search_path(library, Lib) :- library_directory(Lib).

% element_path(Type,Root,Encoding,Path)
%
%   Type : the type of files provided by this entry
%   Root : root spec (e.g. [], or [library], or [engine]) 
%   Encoding : 'concat' (e.g. Path/library.web.xml)
%              'dir' (e.g. Path/library/web/xml)
%              (does not include the root part)
%   Path : parent directory
:- data element_path/4.

:- public data element_custom_dir/2.

:- use_module(library(system), [getenvstr/2]).
:- use_module(engine(exceptions)).

:- initialization(setup_paths).
:- public setup_paths/0.
setup_paths :-
	% Here is the library configuration part
	% TODO: this should be parametric to the compilation step: one must allow a component path for the compiler different from the component path that the compiler uses to compile

	% Paths for 'current' compilation
	( getenvstr('CIAOCCONFIG', ConfigurationDir0) ->
	    atom_codes(ConfigurationDir, ConfigurationDir0)
	; errlog:bug(['cannot run without a definition for CIAOCCONFIG']),
	  halt(3)
	),
	assertz_fact(element_custom_dir(used_configuration, ConfigurationDir)),

	% Paths for both 'next' and 'current' compilation
        ciao_lib_dir(Path0),

	% TODO: find a better solution... 'general resource' inclusion??
	atom_concat(Path0, '/ciao/version', VersionDir),
	assertz_fact(element_custom_dir(version, VersionDir)),

	( getenvstr('CIAOCACHE', CacheDir0) ->
	    atom_codes(CacheDir, CacheDir0)
	; errlog:bug(['cannot run without a definition for CIAOCACHE']),
	  fail
	),
	atom_concat(CacheDir, '/tmp', CacheDirRoot),
	atom_concat(CacheDir, '/mods-noarch', CacheDirModsNoarch),
	atom_concat(CacheDir, '/mods-noarch/engine', CacheDirModsNoarchEngine),
	atom_concat(CacheDir, '/mods-arch', CacheDirModsArch),
	atom_concat(CacheDir, '/user-noarch', CacheDirUserNoarch),
	atom_concat(CacheDir, '/user-arch', CacheDirUserArch),
	assertz_fact(element_custom_dir(nativeh, CacheDirModsNoarch)), % TODO: change name? move to other directory? choose other encoding?
        assertz_fact(element_path(source,['__tmp__'],concat,CacheDirRoot)),
	%
	add_source_path(~atom_concat(Path0, '/optim_comp/modules/core')),
	add_source_path(~atom_concat(Path0, '/optim_comp/modules/extra')),
	add_source_path(~atom_concat(Path0, '/optim_comp/modules/contrib')),
	%
	assertz_fact(element_path(source,[engine],subdir,~atom_concat(Path0, '/ciao/lib/engine'))),
	assertz_fact(element_path(source,[library],subdir,~atom_concat(Path0, '/ciao/lib'))),
	assertz_fact(element_path(source,[library],subdir,~atom_concat(Path0, '/ciao/library'))),
	assertz_fact(element_path(source,[library],subdir,~atom_concat(Path0, '/ciao/contrib'))),
	%
	assertz_fact(element_path(source,[''],subdir,'')),
	assertz_fact(element_path(portable_header,[engine],concat,CacheDirModsNoarchEngine)),
	%
	% TODO: change directory, use something like... modulenameconcat (do not use the full spec)?
	% TODO: distinction between portable_object and nonportable_object is nto really important in the end
	assertz_fact(element_path(portable_header,[],concat,CacheDirModsNoarch)),
	assertz_fact(element_path(portable_object,[],concat,CacheDirModsNoarch)),
	assertz_fact(element_path(nonportable_object,[],concat,CacheDirModsArch)),
	assertz_fact(element_path(portable_object,[],concat,CacheDirUserNoarch)),
	assertz_fact(element_path(nonportable_object,[],concat,CacheDirUserArch)).

:- public add_source_path/1.
add_source_path(Path) :-
	assertz_fact(element_custom_dir(src, Path)),
	assertz_fact(element_path(source,[],subdir,Path)).

% (Temporal? adds custom sources for comp_js.pl)
:- public adda_source_path/1.
adda_source_path(Path) :-
	asserta_fact(element_custom_dir(src, Path)),
	asserta_fact(element_path(source,[],subdir,Path)).

% (Temporal? removes custom sources for comp_js.pl)
:- public del_source_path/1.
del_source_path(Path) :-
	retractall_fact(element_custom_dir(src, Path)),
	retractall_fact(element_path(source,[],subdir,Path)).

:- public addr_new/2.
% Get a filename for the given Spec
addr_new(Action, Filename) :-
	functor(Action, Type, 1),
	arg(1, Action, Spec),
	% Choice 1: select the path
	element_path(PType, RootMs, Encode, Path),
	% Filter entries that match (Type, Spec)
	append(RootMs, RelMs, Spec),
	filetype__kind(Type, PType),
	!,
	encode_ms(Encode, RelMs, Path, Basename),
	filetype__ext(Type, Ext),
	atom_concat(Basename, Ext, Filename).

:- use_module(engine(io_basic)).

:- public addr/2.
% Search a file (and get the filename) that exists for the given Spec
addr(Action, Filename) :-
	% special case for source files with subdir encoding
	functor(Action, Type, 1),
	arg(1, Action, Spec),
        % Find a res where module speficied by Spec has at least one file
	% Choice 1: select the path
	element_path(PType, RootMs0, Encode, Path),
	% Filter entries that match (Type, Spec)
	append(RootMs0, RelMs0, Spec),
	filetype__kind(Type, PType),
	% Find the file
	( PType = source, Encode = subdir ->
	    findfile_src(Encode, Path, RelMs0, Type, Filename0)
	; findfile(Encode, Path, RelMs0, Type, Filename0)
	),
	!,
	Filename = Filename0.

% TODO: encode in C?
% TODO: for source files
findfile_src(Encode, Path, RelMs0, Type, Filename) :-
	( % path/m0/m1/.../mn.ext
	  encode_ms(Encode, RelMs0, Path, Basename),
	  filetype__ext(Type, Ext0),
	  atom_concat(Basename, Ext0, Filename0)
	; % path/m0/m1/.../mn/mn.ext
	  append(_, [M], RelMs0), append(RelMs0, [M], RelMs1),
	  encode_ms(Encode, RelMs1, Path, Basename),
	  filetype__ext(Type, Ext0),
	  atom_concat(Basename, Ext0, Filename0)
	; % path/m0/m1/.../mn (if ext is optional)
          RelMs1 = RelMs0,
	  encode_ms(Encode, RelMs1, Path, Basename),
	  filetype__accept_noext(Type),
	  % accept absolute paths of .pl files without extension
	  Filename0 = Basename
	), 
	exists_regular(Filename0),
	!,
	Filename = Filename0.

% TODO: encode in C?
% TODO: for internal or target types...
findfile(Encode, Path, RelMs0, Type, Filename) :-
	% path/m0/m1/.../mn.ext
	encode_ms(Encode, RelMs0, Path, Basename),
	filetype__ext(Type, Ext0),
	atom_concat(Basename, Ext0, Filename0),
	exists_regular(Filename0),
	Filename = Filename0.
	        
encode_ms(subdir, RelMs, Path0, Basename) :- !,
	list_to_path(RelMs, 0'/, 0'/, Path0, Basename).
encode_ms(concat, [''|RelMs], Path0, Basename) :- !,
	list_to_path(['__root__'|RelMs], 0'/, 0'., Path0, Basename). % hack to avoid hidden files in unix filesystems
encode_ms(concat, RelMs, Path0, Basename) :- !,
	list_to_path(RelMs, 0'/, 0'., Path0, Basename).

% TODO: implemented in C, put documentation here?
:- '$props'(list_to_path/5, [impnat=cbool(prolog_list_to_path)]).

:- public spec_equal/2.
spec_equal(SpecA, SpecB) :-
	spec_to_key(SpecA, KeyA),
	spec_to_key(SpecB, KeyB),
	KeyA = KeyB.

:- public spec_to_default_module/2.
% Give the module name of a spec (last name)
spec_to_default_module(Spec, Module) :-
	append(_, [Module], Spec), !.

:- public spec_to_key/2.
spec_to_key(Spec, Key) :-
	list_to_path(Spec, 0'/, 0'., '$id', Key).

:- public find_sources/3.
find_sources([], _, []) :- !.
find_sources([Spec0|Specs0], Rel, [Spec|Specs]) :-
	find_source(Spec0, Rel, Spec),
	find_sources(Specs0, Rel, Specs).

:- public find_source/3.
% find_source(Uspec, Rel, Spec): finds the source Uspec, relative to Rel
%   and returns a unique identifier Spec for that source
% 
% related: addr/2, addr_new/2
%
% examples of Uspec: a(b(c(d(...)))), a(b('c/d/...')), '/home/user/foo.pl'
% 
% Rel may be relpath(...) (relative to a fs path) or relspec(...) (relative
% to a previous Spec)
%
% Spec is a list of atoms.
%
% TODO: it should be equivalent to 'resolve module spec', which however
%   is different from resolving a module name in the sense that it 
%   is syntactical
% TODO: can module specs be used for modules packages, foreign C files, etc?
%   (other vision: they are modules, but written in different files <- good!) 
%   ((merging packages and modules would be feasible in this way))
% TODO: optimize
% TODO: $find_file does not search, it just expands... right? 
:- use_module(library(system), ['$find_file'/8]).
find_source(Uspec, Rel, Spec) :-
	( atom(Uspec),
	  getrelpath(Rel, RelPath),
	  '$find_file'(RelPath, Uspec, '', '.pl', _, Uspec2, _, _),
	  file_exists(Uspec2) -> % TODO: use the .pl name!; it won't work if there is no source... % TODO: necessary? see find_file code
	    name_to_element(Uspec2, Ms, UspecR),
	    normalized_spec__1(UspecR, Spec0),
	    append(Ms, Spec0, Spec)
	; normalized_spec__1(Uspec, Spec0),
	  ( Rel = relspec(FromSpec) ->
	      spec_compose(FromSpec, Spec0, Spec)
	  ; Spec = Spec0
	  )
	).

% pre: Name is an absolute file name
name_to_element(Name, Ms, UspecR) :- % TODO: incomplete!!
	element_path(source,Ms,subdir,Path),
	atom_concat(Path, '/', Path2), 
	atom_concat(Path2, Mod0, Name), !,
	( atom_concat(UspecR, '.pl', Mod0) ->
	    true
	; UspecR = Mod0
	).
name_to_element(Name0, _, _) :-
	% TODO: error?, not in module path; use better error handling
	errlog:bug(['not in module path ', Name0]),
	fail.

getrelpath(relpath(RelPath), RelPath).
getrelpath(relspec(FromSpec), RelPath) :-
	addr(prolog_source(FromSpec), FromName),
	'$find_file'('', FromName, '', '.pl', _, _, _, RelPath).

normalized_spec__1(Uspec, Ms) :-
	normalized_spec__2(Uspec, Ms0),
	( append(Ms1, [M, M], Ms0) ->
	    % remove last name duplication
	    append(Ms1, [M], Ms)
	; Ms = Ms0
	).

normalized_spec__2(Uspec, _) :-
	var(Uspec), !, fail.
normalized_spec__2(Uspec, [M|Ms]) :-
	functor(Uspec, M, 1), !,
	arg(1, Uspec, SubUspec),
	normalized_spec__2(SubUspec, Ms).
normalized_spec__2(Uspec, Ms) :-
	atom(Uspec),
	normalized_terminal_spec(Uspec, Ms).

% split 'a/b/c/d' into [a,b,c,d]
normalized_terminal_spec(Uspec, Ms) :-
	atom_codes(Uspec, Codes),
	normalized_terminal_spec_1(Codes, Ms).

normalized_terminal_spec_1(Codes, Ms) :-
	normalized_terminal_spec_2(Codes, Xs, Xs, Ms).

normalized_terminal_spec_2(Codes, Ys, [], [M|Ms]) :- Codes = "/"||Codes0, !,
	atom_codes(M, Ys),
	normalized_terminal_spec_1(Codes0, Ms).
normalized_terminal_spec_2([X|Xs], Ys, [X|Zs], Ms) :- !,
	normalized_terminal_spec_2(Xs, Ys, Zs, Ms).
normalized_terminal_spec_2([], Ys, [], [M]) :- !,
	atom_codes(M, Ys).

spec_compose(FromSpec, RelSpec, Spec) :-
	( RelSpec = [+|RelSpec2] ->
	    element_spec_concat(FromSpec, RelSpec2, Spec)
	; is_relative_spec(RelSpec) ->
	    element_spec_concat(FromSpec, ['..'|RelSpec], Spec)
	; Spec = RelSpec
	).

is_relative_spec(['.'|_]) :- !.
is_relative_spec(['..'|_]) :- !.

% concat a relative spec to a base spec 
% (relative spec may contain '.' and '..')
element_spec_concat(BaseSpec, RelSpec, Spec) :-
	reverse(BaseSpec, RevSpec), % reverse from spec
	element_spec_concat__2(RelSpec, RevSpec, Spec).

element_spec_concat__2([X|RelSpec], RevSpec, Spec) :-
	( X = '.' ->
	    RevSpec2 = RevSpec % nothing
	; X = '..' ->
	    RevSpec = [_|RevSpec2] % pop 
	; RevSpec2 = [X|RevSpec] % push X
	),
	element_spec_concat__2(RelSpec, RevSpec2, Spec).
element_spec_concat__2([], RevSpec, Spec) :- % finish
	reverse(RevSpec, Spec). % reverse reversed spec

reverse(Xs, Ys) :- reverse__2(Xs, [], Ys).
reverse__2([], Ys, Ys).
reverse__2([X|Xs], Ys, Zs) :- reverse__2(Xs, [X|Ys], Zs).

:- public denormalized_spec/2.
% [a,b,...] ==> a(b(...))
% TODO: use library/lists or library(lists) ?
denormalized_spec([''|Specs], Spec3) :- !,
	list_to_path(Specs, 0'/, 0'/, '', Spec3).
denormalized_spec(Specs, Spec3) :-
	denormalized_spec__2(Specs, Spec3).

denormalized_spec__2([Spec], Spec) :- !.
denormalized_spec__2([Spec|Specs], Spec2) :-
	functor(Spec2, Spec, 1),
	arg(1, Spec2, Spec3),
	denormalized_spec__2(Specs, Spec3).

:- use_module(library(system)).

% accept anything but directories
exists_regular(Path) :-
        prolog_flag(fileerrors, OldFE, off),
        Ok = ( file_properties(Path, T, [], [], [], []),
	       \+ T = directory ? yes | no ),
        set_prolog_flag(fileerrors, OldFE),
	Ok = yes.

append([], Xs, Xs).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).

% ---------------------------------------------------------------------------

% Find a package
% (the 'library' prefix in packages can be omitted)
% TODO: include in find_source more than one relative spec? will that make find_source enought to implement this predicate?
:- public find_package/3.
find_package(Spec0, RelSpec, Spec) :-
	% Note: check for 'library' is done last (otherwise it may use
	% wrong libraries in optim_comp)
	find_source(Spec0, RelSpec, Spec1),
        addr(prolog_package(Spec1), _Name),
	!,
	Spec1 = Spec.
find_package(Spec0, _RelSpec, Spec) :-
	find_source(+(Spec0), relspec([library]), Spec1),
        addr(prolog_package(Spec1), _Name),
	!,
	Spec1 = Spec.

% ---------------------------------------------------------------------------
% Callback for file types (required to load/save/restore elements)

:- include(compiler(store__callback)).

% ===========================================================================
% Common source file type
% TODO: is this the right place?

% source code of a module
filetype__ext(prolog_source, '.pl').
filetype__accept_noext(prolog_source).
filetype__kind(prolog_source, source).
% package
% TODO: rename to .plh? include in the .pl?
filetype__ext(prolog_package, '.pl').
filetype__accept_noext(prolog_package).
filetype__kind(prolog_package, source).
% (directory) within config scripts
filetype__ext(prolog_cfg, '.cfg').
filetype__kind(prolog_cfg, source).

% ===========================================================================
% Exported to 'memoize' (defined in memoize__callback)
% TODO: is this the right place?

:- include(.(memoize__callback)).

:- use_module(engine(dynlink)).
:- use_module(engine(rt_exp)).

% (export)
% TODO: include other frozen elements... (for example, pre-compiled modules)
% TODO: improve support for frozen elements
action__frozen(Action) :-
	( Action = load(Spec) ->
	    spec_to_key(Spec, SpecKey),
	    current_speckey_module(SpecKey, Module),
	    dynlink:static_module(Module)
% TODO: rudimentary frozen...	
%	; Spec = [library|_] -> true
%	; Spec = [engine|_] -> true
%	; Spec = [compiler|_] -> true
%	; Spec = [shell|_] -> true
	; fail
	).

% (export)
% Fails if element does not exist
action__timestamp(load(Spec), Time) :- !,
	spec_to_key(Spec, SpecKey),
	current_speckey_module(SpecKey, Module),
	dynlink:module_timestamp(Module, Time).
action__timestamp(Action, Time) :-
	addr(Action, Name),
	modif_time(Name, Time).

% (export)
action__terminal(prolog_source(_)).
action__terminal(prolog_package(_)).

% (export)
action__save(Action, Ref) :-
	functor(Action, Type, 1),
	addr_new(Action, Name),
	element__save(Type, Ref, Name).

% (export)
action__restore(Action, Ref) :-
	addr(Action, Name),
	functor(Action, Type, 1),
	element__restore(Type, Name, Ref).

% (export)
action__key(Action, Key) :-
	functor(Action, Type, 1),
	arg(1, Action, Spec),
	list_to_path([Type|Spec], 0'/, 0'., '$sa', Key).

% ===========================================================================

{
:- fluid memo :: memoize.

:- public eval_file/2.
% Update and get the file of Action
% note: fails on error
eval_file(Action, File) :-
	memo.eval0(Action),
	store:addr(Action, File).

:- public eval_file/3.
% Update and get a sub-file of Action
% note: fails on error
eval_file(Action, Suffix, File) :-
	Action =.. [F0|Args],
	atom_concat(F0, '__', F1),
	atom_concat(F1, Suffix, F),
	Action2 =.. [F|Args],
	memo.eval0(Action),
	store:addr(Action2, File).
}.
