:- module(infertime_lib,
	    [
		get_platform/2,
		compound_resource/2,
		platform_constants/4
	    ],
	    [assertions, regtypes]).

:- doc(title,  "Time assertions processing library.").
:- doc(author, "Edison Mera").

:- doc(module, "This program defines the predicates that are used
	for writing programs that process platform dependent
	assertions (i. e., assertions related with the time).").

:- use_module(library(aggregates)).
:- use_module(resources(resources_basic)).
:- use_module(program(clause_db), [source_clause/3]).

% A platform declaration overrides the default value

current_platform(Platform) :-
	findall(R, current_fact(source_clause(_Key,
		    directive(platform(R)), _Dict), _Ref), Platforms),
	Platforms = [Platform].

default_platform(Platform) :-
	get_os(OS),
	get_arch(Arch),
	atom_concat(OS, Arch, Platform).

get_platform(BT, Platform) :-
	current_platform(Platform) -> true
    ;
	default_platform(Platform),
	insert_element(bp(Platform), BT).

compound_resource(CompoundResource, Resources) :-
	current_fact(source_clause(_Key, directive(
		    compound_resource(CompoundResource, Resources)),
		_Dict)).

platform_constants(Platform, CompoundResource, Approx, Constants) :-
	current_fact(source_clause(_Key,
		directive(platform_constants(Platform, CompoundResource,
			Approx, Constants)), _Dict)).
platform_constants(Platform, CompoundResource, _, Constants) :-
	current_fact(source_clause(_Key,
		directive(platform_constants(Platform, CompoundResource,
			Constants)), _Dict)).

% platform_constants(CompoundResource, BT, Approx, Constants) :-
% 	get_platform(BT, Platform),
% 	platform_constants(Platform, CompoundResource, Approx, Constants).

% vector_expr_multiply([A],    [B],     A*B) :-
% 	!.
% vector_expr_multiply([A|As], [B|Bs], (A*B + Cs)) :-
% 	vector_expr_multiply(As, Bs, Cs).
