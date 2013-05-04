:- doc(section, "Application Resources").
% (do not confuse with computational resources)
% TODO: This should be a package!

% TODO: Include dependencies?

:- static multifile get_out_dir/1. % TODO: find a better way (comp_js.pl) (use store.pl)
:- '$ctxprj'(get_out_dir/1, []).

decl__treatDom(resource(_)).
decl__treat(resource(Name0)) :- !,
	Spec = ~find_source(Name0),
	%
%	trace(resspec(Spec)),
	% TODO: This is not a prolog_source, but it works because sources without .pl extensions are accepted
	%       To solve this TODO, create expandable modules for sources in store.
	eval_file(prolog_source(Spec), ResName),
%	trace(resname(ResName)),
	%
	get_out_dir(OutDir),
	copytodir(ResName, OutDir).

decl__treatDom(inline_resource(_, _)).
decl__treat(inline_resource(Name0, String)) :- !,
	get_out_dir(OutDir),
	OutName = ~atom_concat(OutDir, ~atom_concat('/', Name0)),
	'$open'(OutName, w, Stream),
	write_string(Stream, ~string_codes(String)),
	close(Stream).

% Copy the source file of this module as a resource too
% TODO: This is similar to including symbolic information in the output
decl__treatDom(source_as_resource).
decl__treat(source_as_resource) :- !,
%	Name = ~envmod.get_name,
	store:addr(prolog_source(~envmod.mod_spec), PlName),
	% TODO: emit error if this is not a top module
	get_out_dir(OutDir),
	copytodir(PlName, OutDir).

% Imports for handling resources (data defined in external files)
:- use_module(library(strings), [write_string/2]).
:- use_module(library(string_type(string_type_rt))).

:- use_module(.(ptojs__aux)).

