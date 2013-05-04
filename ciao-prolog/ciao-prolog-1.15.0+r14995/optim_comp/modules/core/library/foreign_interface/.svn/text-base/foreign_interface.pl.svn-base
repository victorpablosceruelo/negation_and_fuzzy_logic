:- package(foreign_interface).

:- use_package([assertions,basicmodes,regtypes]).

:- '$pragma'(treat_assertions).
:- '$pragma'(gluecode_options).
:- '$native_weak_inline'(include('engine/engine__ciao_gluecode.h')).

:- use_module(library(foreign_interface(foreign_interface_properties))).

:- new_declaration(use_foreign_source/1, private).
:- new_declaration(use_foreign_source/2, private).

:- new_declaration(use_foreign_library/1, private).
:- new_declaration(use_foreign_library/2, private).

:- new_declaration(use_compiler/1, private).
:- new_declaration(use_compiler/2, private).

:- new_declaration(extra_compiler_opts/1, private).
:- new_declaration(extra_compiler_opts/2, private).

:- new_declaration(extra_linker_opts/1, private).
:- new_declaration(extra_linker_opts/2, private).

:- new_declaration(use_linker/1, private).
:- new_declaration(use_linker/2, private).

:- new_declaration(ttr_def/2, private).
:- new_declaration(ttr_match/2, private).

:- include(engine(foreign_interface__ttrs)).

:- load_compilation_module(library(foreign_interface(foreign_interface__syntax))).
:- add_sentence_trans(foreign_interface__syntax/3, 1010).
