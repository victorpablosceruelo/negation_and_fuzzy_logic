:- module(res_common_gen, [
		current_common_resource_assr/3,
		current_common_resource_decl/2,
		display_autogen_message/0,
		write_res_common_rt/1,
		write_res_common_decl/1,
		write_res_common_assr/1],
	    [assertions, hiord]).

:- use_module(library(format)).
:- use_module(library(write)).
:- reexport(library(file_utils), [output_to_file/2]).

display_autogen_message :-
	format("%% WARNING: This file was auto generated.\n\n", []).

:- meta_predicate write_res_common_rt(pred(1)).

write_res_common_rt(CommonResourcePredicates) :-
	display_autogen_message,
	(
	    CommonResourcePredicates(Predicates),
	    list(Predicates, portray_clause),
	    fail
	;
	    true
	).

:- meta_predicate write_res_common_decl(pred(1)).

write_res_common_decl(CommonResourceDeclarations) :-
	display_autogen_message,
	(
	    CommonResourceDeclarations(Assertions),
	    list(Assertions, portray_clause),
	    fail
	;
	    true
	).

:- meta_predicate write_res_common_assr(pred(1)).

write_res_common_assr(CommonResourceAssertions) :-
	display_autogen_message,
	(
	    CommonResourceAssertions(Assertions),
	    list(Assertions, portray_clause),
	    fail
	;
	    true
	).

:- meta_predicate current_common_resource_assr(?, pred(3), ?).

current_common_resource_assr(ResourceAssrt, CurrentCommonResource,
	    Assertion) :-
	CurrentCommonResource(_, Resource, ResourceFunc),
	member(Approx, [ub, lb]),
	Prop =.. [ResourceAssrt, Approx, Resource, ResourceFunc],
	Assertion = [(:- Prop)].

:- meta_predicate current_common_resource_decl(?, pred(2)).
current_common_resource_decl(Assertion, CurrentCommonResource) :-
	CurrentCommonResource(_, Resource),
	Assertion = [(:- resource(Resource))].
