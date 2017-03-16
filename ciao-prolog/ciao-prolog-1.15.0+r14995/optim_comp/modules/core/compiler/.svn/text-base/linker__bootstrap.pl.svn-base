:- module(_, [], [compiler(complang)]).

:- use_module(library(aggregates)).
:- use_module(library(read)).
%:- use_module(library(format)).
:- use_module(library(streams)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(compiler(store)).

:- use_module(compiler(memoize)).
:- use_module(compiler(frontend), [compiler_version/1]).
:- use_module(compiler(linker__bytecode)).

ensure_directory(C) :- file_exists(C), file_property(C, type(directory)), !.
ensure_directory(C) :- make_directory(C).

concat([]) := ''.
concat([X|Xs]) := ~atom_concat(X, ~concat(Xs)).

{
:- fluid memo :: memoize.
:- public make_bootstrap/2.
% Emit the contents of a executable package (except .po's...)
% TODO: delete if errors are found
make_bootstrap(Specs, ExecName) :-
	% Prevent disasters...
	\+ atom_concat(_, '.pl', ExecName),
	atom_concat(ExecName, '.car', PkgExecName),
	ensure_directory(PkgExecName),
	make_noarch(PkgExecName, Specs, native(Require64, NativeSpecs, NativeHSpecs, CNativeSpecs, HNativeSpecs)),
	emit_c_code(Require64, NativeSpecs, NativeHSpecs, HNativeSpecs, CNativeSpecs, PkgExecName),
	emit_version(PkgExecName),
	emit_scripts(PkgExecName).

emit_c_code(Require64, NativeSpecs, NativeHSpecs, IncludedCHeaderSpecs, CNativeSpecs, PkgExecName) :-
	% Create the package subdirectory to write C files
	PkgC = ~atom_concat(PkgExecName, '/c'),
	ensure_directory(PkgC),
	PkgCEng = ~atom_concat(PkgC, '/engine'),
	ensure_directory(PkgCEng),

	% TODO: think again... do not use system/1
	copy__native(NativeSpecs, PkgCEng, NativeModules1),
	copy__native_h(NativeHSpecs, PkgCEng, _),
	copy__included_c_source(CNativeSpecs, PkgCEng, NativeModules2),
	copy__included_c_header(IncludedCHeaderSpecs, PkgCEng, _IncludedCHeaderModules),
	append(NativeModules1, NativeModules2, NativeModules),

	write_atoms_to_file(~atom_concat(PkgExecName, '/native_modules'), NativeModules),

        % emit the used compiler version
	frontend:compiler_version(CompilerVersion),
        write_string_to_file(~atom_concat(PkgExecName, '/compiler_version'), ~number_codes(CompilerVersion)),
        % emit source code requirements
        write_string_to_file(~atom_concat(PkgExecName, '/require64'), ~atom_codes(Require64)).

copy__native([], _, []) :- !.
copy__native([NativeSpec|NativeSpecs], ToDir, [NativeModule|NativeModules]) :- !,
	store:addr(compile__c(NativeSpec), NativeName),
	get_spec_module(NativeSpec, NativeModule0),
	NativeModule = ~atom_concat(NativeModule0, '.native'),
	copyfile(NativeName, ~concat([ToDir, '/', NativeModule, '.c'])),
	copy__native(NativeSpecs, ToDir, NativeModules).

copy__native_h([], _, []) :- !.
copy__native_h([NativeSpec|NativeSpecs], ToDir, [NativeModule|NativeModules]) :- !,
	store:addr(compile__h(NativeSpec), NativeName),
	get_spec_module(NativeSpec, NativeModule0),
	NativeModule = ~atom_concat(NativeModule0, '.native'),
	copyfile(NativeName, ~concat([ToDir, '/', NativeModule, '.h'])),
	copy__native_h(NativeSpecs, ToDir, NativeModules).
}.

copy__included_c_source([], _, []) :- !.
copy__included_c_source([Spec|Specs], ToDir, [Module|Modules]) :- !,
	store:addr(c_source(Spec), Name),
	spec_to_default_module(Spec, Module),
	copyfile(Name, ~concat([ToDir, '/', Module, '.c'])),
	copy__included_c_source(Specs, ToDir, Modules).

copy__included_c_header([], _, []) :- !.
copy__included_c_header([Spec|Specs], ToDir, [Module|Modules]) :- !,
	store:addr(c_header(Spec), Name),
	spec_to_default_module(Spec, Module),
	copyfile(Name, ~concat([ToDir, '/', Module, '.h'])),
	copy__included_c_header(Specs, ToDir, Modules).

% ---------------------------------------------------------------------------
% ---------------------------------------------------------------------------
% Generation of the engine C code (a-la make_po)

{
:- fluid memo :: memoize.
make_noarch(PkgExecName, Specs, NativeInfo) :-
	set_prolog_flag(executables, static),
	atom_concat(PkgExecName, '/noarch', NoArch),
	linker__bytecode:link(Specs, NoArch, NativeInfo).
}.

% TODO: share? this code appears in several files
write_string_to_file(File, String) :-
	OutStream = ~open_output(File),
	'$display_string'(String), nl,
	close_output(OutStream).

write_atoms_to_file(File, Atoms) :-
	OutStream = ~open_output(File),
	write_atoms_to_file__2(Atoms),
	close_output(OutStream).

write_atoms_to_file__2([]).
write_atoms_to_file__2([X|Xs]) :-
	display(X), nl,
	write_atoms_to_file__2(Xs).

% ---------------------------------------------------------------------------
% ---------------------------------------------------------------------------
% Scripts (methods of the pkgexec object)

emit_scripts(PkgExecName) :-
        copy_script(PkgExecName, compile_native),
        copy_script(PkgExecName, clean),
        copy_script(PkgExecName, run),
        copy_script(PkgExecName, debug),
	emit_configure_script(PkgExecName).

% TODO: fix hardcoded .car method scripts
copy_script(PkgExecName, Script) :-
	store:addr(sh_script([compiler, scripts, Script]), Name),
	ScriptName = ~concat([PkgExecName, '/', Script]),
	copyfile(Name, ScriptName),
	execperms(ScriptName).

% TODO: fix hardcoded configure script
% TODO: copydir copies .svn hidden files!! fix it
emit_configure_script(PkgExecName) :-
	% TODO: addr_new must be replaced by addr, however addr only succeeds if the file is a regular file and we are searching for a directory... replace the ocnfiguration directory by a single shell script?
	store:addr_new(prolog_cfg([engine, internals]), CfgName),
	copyall(CfgName, ~concat([PkgExecName, '/configure'])).

% TODO: read that info from a lpdoc generated file?? (that can be fetch from subversion, for example)
emit_version(PkgExecName) :-
	OutDir = ~concat([PkgExecName, '/version']),
	ensure_directory(OutDir),
	element_custom_dir(version, VersionDir),
	% TODO: include revision info (merge with trunk/)
%	copyfile(~concat([VersionDir, '/svnrev']), OutDir),
%	copyfile(~concat([VersionDir, '/GlobalBranch']), OutDir).
	copyfile(~concat([VersionDir, '/GlobalVersion']), OutDir).

% ---------------------------------------------------------------------------
% TODO: do not use system/1

copyfile(From, To) :-
	system(~concat(['cp ', From, ' ', To])).
copytodir(From, ToDir) :-
	system(~concat(['cp ', From, ' ', ToDir])).
execperms(File) :-
	system(~concat(['chmod a+x ', File])).
copyall(From, To) :- % note: does not copy directories
	system(~concat(['mkdir -p ', To])),
	system(~concat(['cp ', From, '/* ', To, '/'])).
