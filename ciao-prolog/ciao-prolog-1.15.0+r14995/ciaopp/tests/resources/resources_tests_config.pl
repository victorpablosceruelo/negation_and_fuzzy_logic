:- module(_, _, [fsyntax]).

type_analysis(ciaopp(examples(resources(general(power_set)))),     terms) :- !.
type_analysis(ciaopp(examples(resources(exectimehl(powset)))),     terms) :- !.
type_analysis(ciaopp(examples(resources(exectimehl(powset_2)))),   terms) :- !.
type_analysis(ciaopp(examples(resources(exectimell(powset_llm)))), terms) :- !.
type_analysis(_,                                           eterms).


% List here the examples that are not compatible with old cost
% analysis:

skip_old :=
	ciaopp(tests(resources(examples(reverse_3))))|
	ciaopp(tests(resources(examples(rtests))))|
	ciaopp(tests(resources(examples(param_1))))|
	ciaopp(tests(resources(examples(hanoi_1))))|
	ciaopp(examples(resources(general(~examples_general))))|
	ciaopp(examples(resources(exectimehl(~examples_exectimehl))))|
	ciaopp(examples(resources(exectimell(~examples_exectimell))))|
	ciaopp(examples(resources(rtchecks(~examples_rtchecks))))|
	ciaopp(examples(resources(minizinc(~examples_minizinc)))).

ciao_example := ~ciao_alias(_).

java_example := ciaopp(tests(resources(examples(~java_tests)))).

ciao_alias(M, ciaopp(tests(resources(examples(M))))) :- ciao_tests(M).
ciao_alias(M, ciaopp(examples(resources(general(M))))) :- examples_general(M).
ciao_alias(M, ciaopp(examples(resources(exectimehl(M))))) :- examples_exectimehl(M).
ciao_alias(M, ciaopp(examples(resources(exectimell(M))))) :- examples_exectimell(M).
ciao_alias(M, ciaopp(examples(resources(rtchecks(M))))) :- examples_rtchecks(M).
ciao_alias(M, ciaopp(examples(resources(minizinc(M))))) :- examples_minizinc(M).

ciao_tests :=
	hanoi_1|
	pqr_cm|
	pqr|
	pqr_2|
	fib_1|
	fib_2|
	fib_hlm|
	testbuiltin|
	append_1|
	append_hlm|
	bad_measure|
	color_map_1|
	count|
	eight_queen_1|
	flat_1|%   not working
	flat_2|%   not working
	flat_3|%   not working
	flatten_1|
	iqueen5_1|
	list_diff_1|
	list_intersect_1|
	lists_sc|
	param_1|
	partition_1|
	qsort_1|
	qsort_2|
	queen_1|
	reverse|
	reverse_1|
	reverse_2|
	reverse_3|
	rtests|
	schedule|
	subst_exp_1.

examples_general :=
	append|
	client|
	color_map|
% 	copy_files|
	eval_polynom|
	eight_queen|
	fib_3|
	flat|
	grammar|
	hanoi|
	insert_stores_paper|
	iqueen5|
	nrev|
	perm|
	power_set|
	qsort_3|
	send_files|
	subst_exp|
	zebra.

examples_exectimehl :=
	polynom|
	nrev_hlm|
	powset|
	powset_2|
	palindro|
	hanoi_hlm|
	subst_exp_hlm|
	list_intersect|
	list_diff|
	add_poly|
	prog_control.

examples_exectimell :=
	append_llm|
	polynom_llm|
	fib_llm|
	hanoi_llm|
	nreverse_llm|
	palindro_llm|
	powset_llm|
	flatten_llm|
	list_diff_llm|
	list_inters_llm|
	subst_llm.

examples_rtchecks :=
	fib_4|
	flatten_2|
	qsort_4|
	subst_exp_2.

examples_minizinc :=
	jobshop_nxn|
	langford|
	multidimknapsack_simple|
	oss|
	photo|
	queen_cp2|
	sudoku.

java_tests :=
	'BST'|
	'CellPhone'|
	'Client'|
	'Dhrystone'|
	'Fact'|
	'Fib'|
	'Files'|
	'Files_2'|
	'Join'|
	'Screen'|
	'SensorNetwork'|
	'SensorNetworkLoop'.
