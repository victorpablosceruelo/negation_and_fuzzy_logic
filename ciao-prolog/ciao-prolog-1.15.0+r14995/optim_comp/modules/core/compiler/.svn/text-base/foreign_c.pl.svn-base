:- doc(section, "C Foreign Code").

% ---------------------------------------------------------------------------

% (C foreign interface)
filetype__ext(c_source, '.c').
filetype__kind(c_source, source).
%
filetype__ext(c_header, '.h').
filetype__kind(c_header, source).
%
filetype__ext(sh_script, '.sh').
filetype__kind(sh_script, source).

% ---------------------------------------------------------------------------

% '$native_include_c_source'/1 - internal declaration % TODO: document
decl__treatDom('$native_include_c_source'(_)).
decl__treat('$native_include_c_source'(Spec)) :- !,
        do_native_include_c_source(Spec).

{
:- extends modread_ctx.
do_native_include_c_source(Spec0) :-
	Spec = ~find_source(Spec0),
	Module = ~top_envmod, % TODO: use def_envmod
	trust(Module instance_of module_s),
	Module.add1_native_dep(c_source, Spec). % TODO: poor indexing
}.

% '$native_include_c_header'/1 - internal declaration % TODO: document
decl__treatDom('$native_include_c_header'(_)).
decl__treat('$native_include_c_header'(Spec)) :- !,
        do_native_include_c_header(Spec).

{
:- extends modread_ctx.
do_native_include_c_header(Spec0) :-
	Spec = ~find_source(Spec0),
	Module = ~top_envmod, % TODO: use def_envmod
	trust(Module instance_of module_s),
	Module.add1_native_dep(c_header, Spec). % TODO: poor indexing
}.

% '$native_inline'/1 - internal declaration % TODO: document
decl__treatDom('$native_inline'(_)).
decl__treat('$native_inline'(Inline)) :- !,
	Module = ~top_envmod, % TODO: use def_envmod
	trust(Module instance_of module_s),
	Module.add_native_inline(Inline).

% '$native_weak_inline'/1 - internal declaration % TODO: document
decl__treatDom('$native_weak_inline'(_)).
decl__treat('$native_weak_inline'(Inline)) :- !,
	Module = ~top_envmod, % TODO: use def_envmod
	trust(Module instance_of module_s),
	Module.add_native_inline(weak(Inline)).

:- include(.(frontend_assertions)).

