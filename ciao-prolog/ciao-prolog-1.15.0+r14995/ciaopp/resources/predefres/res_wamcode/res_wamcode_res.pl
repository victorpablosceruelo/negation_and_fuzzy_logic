:- module(_, _, [assertions]).

:- use_package(resources(inferres_decl)).

:- use_module(program(itf_base_db), [curr_file/2]).
:- use_module(resources(resources_basic)).

:- discontiguous resource_wamcode/2.

:- use_module(res_wamcode(res_wamcode_basic)).

:- include(res_wamcode(res_wamcode_res_auto)).

init_resource(res_wamcode_res) :-
	curr_file(FileName, _Module) ->
	cleanup_clause_wamcode_db,
	atom_concat(FileBase, '.pl', FileName),
	get_wamcode(FileBase)
    ;
	true.

wamcode_instr(WamCode, LitInfo, Cost) :-
	litinfo_get_key(LitInfo, Key),
	litinfo_get_litnum(LitInfo, LitNum),
	get_lit_wamcode(Key, LitNum, WamCodes),
	count_wamcode_instr(WamCodes, WamCode, 0, Cost).

count_wamcode_instr([],                 _,        Count,  Count).
count_wamcode_instr([WamCode|WamCodes], WamCodeP, Count0, Count) :-
	(
	    \+ \+ WamCode = WamCodeP ->
	    Count1 is Count0 + 1
	;
	    Count1 is Count0
	),
	count_wamcode_instr(WamCodes, WamCodeP, Count1, Count).
