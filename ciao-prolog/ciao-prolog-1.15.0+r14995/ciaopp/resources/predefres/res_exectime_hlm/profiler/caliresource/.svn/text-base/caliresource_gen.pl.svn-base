:- module(caliresource_gen, _, [assertions, regtypes]).

:- use_module(library(aggregates)).
:- use_module(library(write)).
:- use_module(library(hiordlib),   [map/3]).
:- use_module(library(file_utils), [output_to_file/2]).
:- use_module(profcost(profconfig)).

% The calibration tests must be created first of all, because they will be
% loaded statically by the calibrator.

calibration_preds(L) :-
	findall(P/A, pred_calibrate(_:P/A), L).

generate_calibration_measures(Repetitions) :-
	absolute_file_name(profcost(caliresource), File0),
	atom_concat(File1, '.pl',              File0),
	atom_concat(File1, '_measure_auto.pl', File),
	calibration_preds(CalibrationPreds),
	output_to_file(generate_module_measures(Repetitions,
		[true/0|CalibrationPreds],
		[profcost(caliresource(caliresource_bench_auto))]), File).

generate_module_measures(Repetitions, PredList, Modules) :-
	generate_clause_measures(PredList, Repetitions, Clauses),
	map(Modules, module_clause, ModClauses),
	list(
	    [
		( :- module(_, [calls_per_measure/1, measure/3],
			[assertions, inliner]) ),
		( :- use_module(library(profiler(profiler_extra)),
			[time_option/2]) ),
		( :- inline_module(library(profiler(profiler_extra)),
			[measure_0/3]) ),
		(:- use_module(library(prolog_sys)))|
		ModClauses
	    ], portray_clause),
	portray_clause(calls_per_measure(Repetitions)),
	list(Clauses, portray_clause),
	portray_clause(
	    ( measure(Pred, _, 0) :-
		display_list(['{WARNING: no measure predicate defined for ',
			Pred, '}\n']) )).

module_clause(Module, (:- use_module(Module))).

repeat(1, B, B).
repeat(N, B, (B, RB)) :-
	N1 is N - 1,
	repeat(N1, B, RB).

generate_clause_measures([],          _,           []).
generate_clause_measures([F/A|Preds], Repetitions, [Clause|Clauses]) :-
	functor(Pred, F, A),
	repeat(Repetitions, (\+ \+ Pred), RPred),
	Clause = ( measure(Pred, Option, Value) :-
	    push_prolog_flag(gc, off),
	    measure_0(Option, RPred, Value),
	    pop_prolog_flag(gc),
	    ! ),
	generate_clause_measures(Preds, Repetitions, Clauses).

generate_calibration_tests :-
	absolute_file_name(profcost(caliresource), File0),
	atom_concat(File1, '.pl',            File0),
	atom_concat(File1, '_bench_auto.pl', File),
	calibration_preds(CalibrationPreds),
	generate_calibration_tests_file(File, CalibrationPreds).

generate_calibration_tests_file(File, CalibrationPreds) :-
	open(File, write, S),
	generate_calibration_tests_stream(S, CalibrationPreds),
	close(S).

generate_calibration_tests_stream(S, CalibrationPreds) :-
	(
% 	    VarN = 7,
% 	    atom_number(VarNA,VarN),
% 	    atom_concat(vart,VarNA,VarP),
	    portray_clauses(S,
		[
		    ( :- module(_, CalibrationPreds,
			    [assertions, nortchecks, nativeprops, regtypes,
				predefres(res_all)]) ),
		    (:- regtype int_list/1),
		    (int_list([])),
		    ( int_list([A|L]) :-
			int(A),
			int_list(L) ),
% 		    (:- regtype short_int_list/1),
% 		    (short_int_list([])),
% 		    ( short_int_list([A|L]) :-
% 			int(A),
% 			short_int_list(L) ),
		    (:- entry verify_list_type(_) :int_list),
		    (verify_list_type([])),
		    ( verify_list_type([_|L]) :-
			verify_list_type(L) ),
		    (:- entry list_no_last_call_opt(_) :int_list),
		    (list_no_last_call_opt([])),
		    ( list_no_last_call_opt([_|L]) :-
			list_no_last_call_opt(L),
			dummy ),
		    (dummy),
		    ( :- entry list_many_args/9 : int_list * var * var *
			var * var * var * var * var * var ),
		    ( list_many_args([], [], [], [], [], [],
			    [], [], []) ),
		    ( list_many_args([A|B], [A|C], [A|D], [A|E], [A|F
			    ], [A|G],
			    [A|H], [A|I], [A|J]) :-
			list_many_args(B, C, D, E, F, G, H, I, J) ),
		    (:- entry unify_two_lists(_A, _B) : int_list * var),
		    (unify_two_lists([], [])),
		    ( unify_two_lists([A|As], [A|Bs]) :-
			unify_two_lists(As, Bs) ),
		    (:- entry list_var_output(_A, _B) : int_list * var),
		    (list_var_output([], [])),
		    ( list_var_output([X|Xs], [f(
				    X, X, X, X, X, X, X, X, X, X,
				    X, X, X, X, X, X, X, X, X, X,
				    X, X, X, X, X, X, X, X, X, X,
				    X, X, X, X, X, X, X, X, X, X,
				    X, X, X, X, X, X, X, X, X, X
				)|Ys]) :-
			list_var_output(Xs, Ys) ),

		    (:- entry list_var_input(_A, _B) : input_list * var),
		    (list_var_input([], [])),
		    ( list_var_input([f(
				    X00, X01, X02, X03, X04, X05, X06, X07,
				    X08, X09, X10, X11, X12, X13, X14, X15,
				    X16, X17, X18, X19, X20, X21, X22, X23,
				    X24, X25, X26, X27, X28, X29, X30, X31,
				    X32, X33, X34, X35, X36, X37, X38, X39,
				    X40, X41, X42, X43, X44, X45, X46, X47,
				    X48, X49,
				    X50, X51, X52, X53, X54, X55, X56, X57,
				    X58, X59, X60, X61, X62, X63, X64, X65,
				    X66, X67, X68, X69, X70, X71, X72, X73,
				    X74, X75, X76, X77, X78, X79, X80, X81,
				    X82, X83, X84, X85, X86, X87, X88, X89,
				    X90, X91, X92, X93, X94, X95, X96, X97,
				    X98, X99

				)|As], [g(
				    X00, X01, X02, X03, X04, X05, X06, X07,
				    X08, X09, X10, X11, X12, X13, X14, X15,
				    X16, X17, X18, X19, X20, X21, X22, X23,
				    X24, X25, X26, X27, X28, X29, X30, X31,
				    X32, X33, X34, X35, X36, X37, X38, X39,
				    X40, X41, X42, X43, X44, X45, X46, X47,
				    X48, X49,
				    X50, X51, X52, X53, X54, X55, X56, X57,
				    X58, X59, X60, X61, X62, X63, X64, X65,
				    X66, X67, X68, X69, X70, X71, X72, X73,
				    X74, X75, X76, X77, X78, X79, X80, X81,
				    X82, X83, X84, X85, X86, X87, X88, X89,
				    X90, X91, X92, X93, X94, X95, X96, X97,
				    X98, X99
				)|Bs]) :-
			list_var_input(As, Bs) ),
		    (:- regtype input_term/1),
		    (
			input_term(f(
				X00, X01, X02, X03, X04, X05, X06, X07, X08,
				X09, X10, X11, X12, X13, X14, X15, X16, X17,
				X18, X19, X20, X21, X22, X23, X24, X25, X26,
				X27, X28, X29, X30, X31, X32, X33, X34, X35,
				X36, X37, X38, X39, X40, X41, X42, X43, X44,
				X45, X46, X47, X48, X49,
				X50, X51, X52, X53, X54, X55, X56, X57, X58,
				X59, X60, X61, X62, X63, X64, X65, X66, X67,
				X68, X69, X70, X71, X72, X73, X74, X75, X76,
				X77, X78, X79, X80, X81, X82, X83, X84, X85,
				X86, X87, X88, X89, X90, X91, X92, X93, X94,
				X95, X96, X97, X98, X99
			    )) :-
			gnd(X00), gnd(X01), gnd(X02), gnd(X03), gnd(X04),
			gnd(X05), gnd(X06), gnd(X07), gnd(X08), gnd(X09),
			gnd(X10), gnd(X11), gnd(X12), gnd(X13), gnd(X14),
			gnd(X15), gnd(X16), gnd(X17), gnd(X18), gnd(X19),
			gnd(X20), gnd(X21), gnd(X22), gnd(X23), gnd(X24),
			gnd(X25), gnd(X26), gnd(X27), gnd(X28), gnd(X29),
			gnd(X30), gnd(X31), gnd(X32), gnd(X33), gnd(X34),
			gnd(X35), gnd(X36), gnd(X37), gnd(X38), gnd(X39),
			gnd(X40), gnd(X41), gnd(X42), gnd(X43), gnd(X44),
			gnd(X45), gnd(X46), gnd(X47), gnd(X48), gnd(X49),

			gnd(X50), gnd(X51), gnd(X52), gnd(X53), gnd(X54),
			gnd(X55), gnd(X56), gnd(X57), gnd(X58), gnd(X59),
			gnd(X60), gnd(X61), gnd(X62), gnd(X63), gnd(X64),
			gnd(X65), gnd(X66), gnd(X67), gnd(X68), gnd(X69),
			gnd(X70), gnd(X71), gnd(X72), gnd(X73), gnd(X74),
			gnd(X75), gnd(X76), gnd(X77), gnd(X78), gnd(X79),
			gnd(X80), gnd(X81), gnd(X82), gnd(X83), gnd(X84),
			gnd(X85), gnd(X86), gnd(X87), gnd(X88), gnd(X89),
			gnd(X90), gnd(X91), gnd(X92), gnd(X93), gnd(X94),
			gnd(X95), gnd(X96), gnd(X97), gnd(X98), gnd(X99)

		    ),
		    (:- regtype input_list/1),
		    (input_list([])),
		    ( input_list([X|L]) :-
			input_term(X),
			input_list(L) ),
		    ( :- entry list_deep_ground_input(_A, _B) : deep_list *
			var ),
		    (list_deep_ground_input([], [])),
		    ( list_deep_ground_input([
				f(
				    a(b(c(d(e(f(g(h(i(j(
						a(b(c(d(e(f(g(h(i(j(
						a(b(c(d(e(f(g(h(i(j(
						a(b(c(d(e(f(g(h(i(j(
						a(b(c(d(e(f(g(h(i(j(B
						)
						))))))))))
						))))))))))
						))))))))))
						))))))))))
					       ))))))))))|As],
			    [B|Bs]) :-
			list_deep_ground_input(As, Bs) ),
		    (:- regtype deep_term/1),
		    ( deep_term(
			    f(
				a(b(c(d(e(f(g(h(i(j(
						a(b(c(d(e(f(g(h(i(j(
						a(b(c(d(e(f(g(h(i(j(
						a(b(c(d(e(f(g(h(i(j(
						a(b(c(d(e(f(g(h(i(j(B
						)
						))))))))))
						))))))))))
						))))))))))
						))))))))))
					      ))))))))))) :-
			int(B) ),
		    (:- regtype deep_list/1),
		    (deep_list([])),
		    ( deep_list([X|L]) :-
			deep_term(X),
			deep_list(L) ),
		    ( :- entry list_flat_ground_input(_A, _B) : flat_list *
			var ),
		    (list_flat_ground_input([], [])),
		    ( list_flat_ground_input([
				f(
				    a, b, c, d, e, f, g, h, i, j,
				    a, b, c, d, e, f, g, h, i, j,
				    a, b, c, d, e, f, g, h, i, j,
				    a, b, c, d, e, f, g, h, i, j,
				    a, b, c, d, e, f, g, h, i, j, B)|As], [
				B|Bs]) :-
			list_flat_ground_input(As, Bs) ),
		    (:- regtype flat_term/1),
		    ( flat_term(
			    f(
				a, b, c, d, e, f, g, h, i, j,
				a, b, c, d, e, f, g, h, i, j,
				a, b, c, d, e, f, g, h, i, j,
				a, b, c, d, e, f, g, h, i, j,
				a, b, c, d, e, f, g, h, i, j, B)) :-
			int(B) ),
		    (:- regtype flat_list/1),
		    (flat_list([])),
		    ( flat_list([X|L]) :-
			flat_term(X),
			flat_list(L) ),
		    ( :- entry list_flat_ground_output(_A, _B) : flat_list *
			var ),
		    (list_flat_ground_output([], [])),
		    ( list_flat_ground_output([X|Xs], [f(
				    a, b, c, d, e, f, g, h, i, j,
				    a, b, c, d, e, f, g, h, i, j,
				    a, b, c, d, e, f, g, h, i, j,
				    a, b, c, d, e, f, g, h, i, j,
				    a, b, c, d, e, f, g, h, i, j)|Ys]) :-
			list_flat_ground_output(Xs, Ys) ),
		    ( :- entry list_deep_ground_output(_A, _B) : deep_list *
			var ),
		    (list_deep_ground_output([], [])),
		    ( list_deep_ground_output([X|Xs], [
				a(b(c(d(e(f(g(h(i(j(
						a(b(c(d(e(f(g(h(i(j(
						a(b(c(d(e(f(g(h(i(j(
						a(b(c(d(e(f(g(h(i(j(
						a(b(c(d(e(f(g(h(i(j(a
						)
						))))))))))
						))))))))))
						))))))))))
						))))))))))
					      )))))))))|Ys]) :-
			list_deep_ground_output(Xs, Ys) ),
% 		    (dummy_environment :- environment),
		    (:- entry environment)

% 		(:- entry nrev/2 : short_int_list * var),
% 		(nrev([],[])),
% 		(nrev([H|L],RL) :-
% 		 nrev(L,R),
% 		 append(R,[H],RL)),
%		(:- entry append/3 : int_list * int_list * var),
% 		(append([],X,X)),
% 		(append([H|X],Y,[H|Z]):- append(X,Y,Z))
% 		(:- entry vart/8 : int * int * int  * int * var * var * var * var),
% 		(vart(A1,B1,C1,D1,A2,B2,C2,D2):-doequal(A1,A2),doequal(B1,B2),
% 		 doequal(C1,C2),doequal(D1,D2)),
% 	        (doequal(A,A))

		]),
	    generate_calltest(200, lcall, CallTest),
	    portray_clauses(S, CallTest),
	    generate_environment_test(12, environment, environment0, EnvTest),
	    portray_clauses(S, EnvTest),
%	    generate_vartest(1,    vart1, VarTest1),
%	    portray_clauses(S,VarTest1),
% 	    generate_vartest(VarN, VarP, VarTest),
% 	    portray_clauses(S,VarTest),
	    true
	).

generate_environment_test(0, H1, H2, [(H1 :- H2), (H2)]) :- !.
generate_environment_test(N, H1, H2, [(H1 :- H2, B)|Cs]) :-
	N1 is N - 1,
	atom_number(NA, N),
	atom_concat(environment, NA, B),
	generate_environment_test(N1, H2, B, Cs).

generate_calltest(N, H, [(:- entry H)|Cs]) :-
	generate_calltest_(N, H, H, Cs).

generate_calltest_(0, _H0, H, [H]) :- !.
generate_calltest_(N, H0,  H, [(H :- B)|Cs]) :-
	N1 is N - 1,
	atom_number(NA, N),
	atom_concat(H0, NA, B),
	generate_calltest_(N1, H0, B, Cs).

arglist_vartest(0, _, []) :- !.
arglist_vartest(N, X, [X|Xs]) :-
	N1 is N - 1,
	arglist_vartest(N1, X, Xs).

generate_vartest(N, F, C) :-
	arglist_vartest(N, _, ArgList),
	VarTest =.. [F|ArgList],
	vararg(N, VarArg),
	functor(VarEntry, F, N),
	C = [(:- entry VarEntry : VarArg), VarTest].

vararg(1, int) :- !.
vararg(N, VarArg * var) :-
	N1 is N - 1,
	vararg(N1, VarArg).

portray_clauses(S, CL) :-
	portray_clauses_(CL, S).

portray_clauses_([],     _S).
portray_clauses_([C|Cs], S) :-
	portray_clause(S, C),
	portray_clauses_(Cs, S).
