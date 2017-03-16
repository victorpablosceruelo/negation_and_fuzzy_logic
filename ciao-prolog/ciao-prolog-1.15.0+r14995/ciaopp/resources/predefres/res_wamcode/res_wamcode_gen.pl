:- module(res_wamcode_gen, [res_wamcode_gen/0],
	    [assertions, nortchecks, hiord, fsyntax]).

:- use_module(library(aggregates)).
:- use_module(library(sort)).
:- use_module(library(write)).
:- use_module(plindent(autoindent), [plindent_file/1]).

:- use_module(res_wamcode(res_wamcode_basic)).

:- use_module(predefres(res_common_gen)).

res_wamcode_gen :-
	absolute_file_name(predefres(res_wamcode), FileName),
	atom_concat(Dir, 'res_wamcode.pl',                FileName),
	atom_concat(Dir, 'res_wamcode_decl_auto.pl',      ResWamcodeDecl),
	atom_concat(Dir, 'res_wamcode_assr_auto.pl',      ResWamcodeAssr),
	atom_concat(Dir, 'res_wamcode_comp_decl_auto.pl', ResWamcodeComp),
	atom_concat(Dir, 'res_wamcode_res_auto.pl',        ResWamcodeRt),
	output_to_file(write_res_wamcode_decl,      ResWamcodeDecl),
	output_to_file(write_res_wamcode_assr,      ResWamcodeAssr),
	output_to_file(write_res_wamcode_comp_decl, ResWamcodeComp),
	output_to_file(write_res_wamcode_res,        ResWamcodeRt),
	list([ResWamcodeDecl, ResWamcodeAssr, ResWamcodeRt], plindent_file).

write_res_wamcode_res :-
	write_res_common_rt(current_wamcode_resource_predicates).

write_res_wamcode_decl :-
	write_res_common_decl(current_wamcode_resource_decl).

write_res_wamcode_assr :-
	write_res_common_assr(current_wamcode_resource_assr).

% Update this list, selecting all the wamcodes, and running the calibrator, and
% then executing:

% not_null_resources(A), member(A0, A), current_wamcode_resource(WamCode, A0), display('\t'), display(WamCode), display(' |\n'), fail.

% selected_wamcode := ~wamcode.

% Notes: with the current calibrators:
% allocate/call/deallocate/init/proceed are l. d.
% get_structure(_, _) is l. d. with the rest.
% get_nil and get_nil_x0 are counted only few times.

selected_wamcode :=
%	allocate |
	call(_, _)|
%	deallocate |
	execute(_)|
	get_list(_)|
	get_list_x0|
%	get_nil(_) |
%	get_nil_x0 |
%	get_structure(_,_) |
%	init(_) |
	neck(_)|
%	proceed |
	put_x_value(_, _)|
	unify_constant(_)|
	unify_structure(_)|
	unify_void|
	unify_x_value(_)|
	unify_x_variable(_).

selected_wamcode_resource_func(WamCode, Resource, ResourceFunc) :-
	selected_wamcode_resource(WamCode, Resource),
	resource_func(Resource, ResourceFunc).

resource_func(Resource, ResourceFunc) :-
	atom_concat('f_', Resource, ResourceFunc).

selected_wamcode_resource(WamCode, Resource) :-
	selected_wamcode(WamCode),
	wamcode_resource(WamCode, Resource).

write_res_wamcode_comp_decl :-
	display_autogen_message,
	findall(ComposedResource, wamcode_resource_compound(_, _,
		ComposedResource), ComposedResources0),
	sort(ComposedResources0, ComposedResources),
	(
	    member(ComposedResource, ComposedResources),
	    findall(Resource,
		wamcode_resource_compound(_, Resource, ComposedResource),
		Resources),
	    portray_clause(( :- compound_resource(ComposedResource,
			Resources) )),
	    fail
	;
	    true
	).

current_wamcode_resource_assr(Assertion) :-
	member(AssertionType, [head_cost, literal_cost]),
	current_common_resource_assr(AssertionType,
	    selected_wamcode_resource_func, Assertion).

current_wamcode_resource_predicates(Predicates) :-
	selected_wamcode_resource_func(WamCode, Resource, ResourceFunc),
	Head =.. [ResourceFunc, LitInfo, Cost],
	Predicates = [
	    (resource_wamcode(Resource, WamCode)),
	    (Head :- wamcode_instr(WamCode, LitInfo, Cost))
	].

current_wamcode_resource_decl(Assertion) :-
	current_common_resource_decl(Assertion, selected_wamcode_resource).
