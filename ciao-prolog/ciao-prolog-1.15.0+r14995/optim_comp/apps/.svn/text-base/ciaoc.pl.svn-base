:- module(ciaoc, [main/1], [assertions]).

:- use_module(library(libpaths), [get_alias_path/0]).
:- use_module(library(compiler(exemaker)), 
        [make_exec/2, make_actmod/2, force_lazy/1, dynamic_search_path/1]).
:- use_module(library(compiler), [make_po/1, use_module/3]).
:- use_module(engine(internals), ['$bootversion'/0]).

:- include(.(ciaoc_documentation)).

main(Args) :-
        get_alias_path,
        handle_args(Args).

handle_args([A|As]) :-
        handle_args_(A, As).
handle_args([]) :-
        usage,
        halt(1).

handle_args_('-h', _) :- !,
        usage.
handle_args_('-v', Args) :- !,
        set_prolog_flag(verbose_compilation, on),
        handle_args(Args).
handle_args_('-ri', Args) :- !,
        set_prolog_flag(itf_format, r),
        handle_args(Args).
handle_args_('-x', Args) :- !,
%        set_prolog_flag(check_libraries, on),
	% TODO: fix
	todo fix
        handle_args(Args).
handle_args_('-u', [CFile|Args]) :- !,
%        use_module(CFile, all, c_itf),
	% TODO: fix
	todo fix
        handle_args(Args).
handle_args_('-u', []) :-
        usage.
handle_args_('-c', Args) :- !,
        verbose_version,
        make_po(Args).
handle_args_('-s', Args) :- !,
        set_prolog_flag(executables, static),
        handle_args(Args).
handle_args_('-S', Args) :- !,
        set_prolog_flag(executables, static),
        get_os(Os),
        get_arch(Arch),
        atom_concat(Os,Arch,Target),
        set_prolog_flag(self_contained,Target),
        handle_args(Args).
handle_args_('-SS',[Target|Args]) :- !,
        set_prolog_flag(executables, static),
        set_prolog_flag(self_contained,Target),
        handle_args(Args).
handle_args_('-SS',[]) :- !,
        usage.
handle_args_('-z', Args) :- !,
        set_prolog_flag(compress_exec,yes),
        handle_args(Args).
handle_args_('-zl',Args) :- !,
        set_prolog_flag(compress_lib,yes),
        handle_args(Args).
handle_args_('-e', Args) :- !,
        set_prolog_flag(executables, eagerload),
        handle_args(Args).
handle_args_('-l', Args) :- !,
        set_prolog_flag(executables, lazyload),
        handle_args(Args).
handle_args_('-ll', [Module|Args]) :- !,
        set_prolog_flag(executables, lazyload),
        force_lazy(Module),
        handle_args(Args).
handle_args_('-ll', []) :- !,
        usage.
handle_args_('-d', [Path|Args]) :- !,
        assertz_fact(dynamic_search_path(Path)),
        handle_args(Args).
handle_args_('-d', []) :- !,
        usage.
handle_args_('-o', [ExecName,File|Files]) :- !,
        verbose_version,
        make_exec([File|Files], ExecName).
handle_args_('-o', _) :- !,
        usage.
handle_args_('-a', [PublishMod,Module]) :- !,
        verbose_version,
        make_actmod(Module, PublishMod).
handle_args_('-a', _) :- !,
        usage.
handle_args_('--bootstrap', [ExecName|Files]) :- Files = [_|_], !,
        make_bootstrap(ExecName, Files).
handle_args_('--bootstrap', _) :- !,
        usage.
handle_args_(File, Files) :-
        verbose_version,
        make_exec([File|Files],_ExecName).

verbose_version :-
        current_prolog_flag(verbose_compilation, on), !,
        '$bootversion'.
verbose_version.

usage :-
    '$bootversion',
    usage_message(Usage),
    message(['Usages:\n',$$(Usage)]).

usage_message("\
ciaoc <MiscOpts> <ExecOpts> [-o <execname>] <file> ...

  Make an executable from the listed files.  If there is
  more than one file, they must be non-module, and the
  first one must include the main predicate.  The -o
  option allows generating an arbitrary executable name.

ciaoc <MiscOpts> <ExecOpts> -a <publishmod> <module>

  Make an active module executable from <module> with
  address publish module <publishmod>.

ciaoc <MiscOpts> -c  <file> ...

  Compile listed files (make .po objects).

<MiscOpts> can be: [-v] [-ri] [-u <file>]

-v  verbose mode

-ri generate human readable .itf files

-u  use <file> for compilation

<ExecOpts> can be: [-s|-S|-SS <target>|-z|-zl|-e|-l|(-ll <module>)*]
                   (-d <alias>)* [-x]

-s  make a static executable (otherwise dynamic files are not included)

-S  make standalone executable for the current OS and architecture

-SS make standalone executable for <target> OS and architecture
    valid <target> values may be: LINUXi86, SolarisSparc...

    (both -S and -SS imply -s)

-z  generate executables with compressed bytecode

-zl generate libraries with compressed bytecode - any library (re)compiled
    as consequence of normal executable compilation will also be affected

-e  make executable with eager load of dynamic files at startup (default)

-l  idem with lazy load of dynamic files (except insecure cases)

-ll force <module> to be loaded lazily,  implies -l

-d  files using this path alias are dynamic (default: library)

-x  Extended recompilation: only useful for Ciao standard library developers

default extension for files is '.pl'

---------------------------------------------------------------------------

Experimental options:

ciaoc --bootstrap <pkgexename> <file> ...

  Make a bootstrap executable package from the listed files.

").
