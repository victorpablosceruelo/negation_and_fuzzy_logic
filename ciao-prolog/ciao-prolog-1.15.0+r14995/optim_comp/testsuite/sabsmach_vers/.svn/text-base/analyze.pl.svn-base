% Analyze results of sabsmach_vers

:- module(_,[],[dcg]).

:- use_module(library(read)).
:- use_module(library(strings)).
:- use_module(library(write)).

:- data optmask/1.
:- data ho_description/1.
:- data lo_description/1.
:- data ho_alias/2.
:- data lo_alias/2.
:- data optmask_ho/1.
:- data optmask_lo/1.
:- data def_opts/1.
:- data optbit/2.
:- data optcode/2.

:- data opts_to_n/3.

:- data xrange/3.

init_opts :-
	optmask_ho(HOs),
	gen_opts_to_n(HOs, ho),
	optmask_lo(LOs),
	gen_opts_to_n(LOs, lo).

gen_opts_to_n(Opts, Axis) :-
	findall(B, gen_opts_to_n__1(Opts, Axis, B), Bs),
	add_opts_to_n(Bs, Axis, 0).

add_opts_to_n([], _, _).
add_opts_to_n([B|Bs], Axis, I) :-
	assertz_fact(opts_to_n(B, Axis, I)),
	I1 is I + 1,
	add_opts_to_n(Bs, Axis, I1).

gen_opts_to_n__1(Xs, Axis, Bits) :-
	gen_opts_to_n__2(Xs, Xs2),
	opts_to_num(Xs2, N),
	( Axis = ho ->
	    opts_to_highbits(N, Bits),
	    hi_opts_with_data(_, Bits)
	; Axis = lo ->
	    opts_to_lowbits(N, Bits)
	).

gen_opts_to_n__2([], []).
gen_opts_to_n__2([X|Xs], Ys) :-
	( Ys = Zs
	; Ys = [X|Zs]
	),
	gen_opts_to_n__2(Xs, Zs).

:- export(main/1).
main(['--all', Machine, Resource]) :- !,
	read_input,
	def_opts(DefOpts0),
	opts_to_num(DefOpts0, DefOpts),
	opts_to_highbits(DefOpts, DefHO),
	opts_to_lowbits(DefOpts, DefLO),
	calc_speed_up(Machine, DefHO, DefLO, Resource),
	header('Legend'),
	opts_legend,
	header('*** All options together ***'), 
	process(Machine,_,_,Resource),
	header('*** Ordering per instruction set options ***'),
	( hi_opts_with_data(Machine, HO),
	    process(Machine,HO,_,Resource),
	    fail
	; true
	).
main(['--opts-legend']) :- !,
	read_input,
	opts_legend.
main(['--list-machines']) :- !,
	read_input,
	( machine(X),
	    display(X),
	    nl,
	    fail
	; true
	).
main(['--list-means']) :- !,
	( mean(X),
	    display(X),
	    nl,
	    fail
	; true
	).
main(['--list-tests']) :- !,
	read_input,
	( bench(X),
	    display(X),
	    nl,
	    fail
	; true
	).
main(['--list-resources']) :- !,
	read_input,
	( resource(X),
	    display(X),
	    nl,
	    fail
	; true
	).
main(['--gnuplot', Outdir]) :- !,
	read_input,
	( machine(Machine),
	  ( mean(Test) ; bench(Test) ),
	  ( resource(Resource) ),
	  ( center(Center) ),
	  process_gnuplot(Machine, Test, Resource, Center, Outdir),
	  fail
	; true
	),
	display('quit'), nl.
main(_) :- !,
	display('Use --all or --gnuplot [arithmetic|harmonic|geometric|min|max|BENCHMARK] [default|relative]'), nl.

center_msg(default, 'relative to default options').
center_msg(relative, 'relative to default options for each row').

write_with_commas([]) :- !.
write_with_commas([LO]) :- !,
	write_with_commas__2(LO).
write_with_commas([LO|LOs]) :-
	write_with_commas__2(LO),
	display(', '),
	write_with_commas(LOs).

write_with_commas__2(LO) :-
	display('\'-\' title '),
	display('\"'),
	display(LO),
	display('\"'),
	display(' with points').

read_input :-
	read_input__1,
	post_read.

read_input__1 :-
	read(X),
	( X = end_of_file ->
	    true
	; ( read_input__2(X) ->
	      true
	  ; display(user_error, failed(X)), nl(user_error)
	  ),
	  read_input__1
	).

post_read :-
	init_opts,
	% number of benchmark programs
	findall(a, bench(_), Xs),
	length(Xs, NumBenchs),
	asserta_fact(numbench(NumBenchs)).

calc_speed_up(Machine,DefHO,DefLO,Resource) :-
	% compute speed_up
	retractall_fact(speed_up(_,Machine,_,_,Resource,_)),
	( bench(X),
	    ( time(X, Machine, DefHO, DefLO, Resource, DefT) -> true ; fail ),
	    time(X, Machine, HO, LO, Resource, T),
	    T2 is DefT/T,
	    assertz_fact(speed_up(X,Machine,HO,LO,Resource,T2)),
	    fail
	; true
	).

:- data cur_opts/2.
:- data cur_bench/1.
:- data cur_machine/1.

% machine(Machine)
:- data machine/1.
% numbench(Num)
:- data numbench/1.
% bench(Bench)
:- data bench/1.
% resource(Resource)
:- data resource/1.
% hi_opts(Opts)
:- data hi_opts/1.
% lo_opts(Opts)
:- data lo_opts/1.
% time(Bench, Machine, HO, LO, Resource, Time)
:- data time/6.
% speed_up(Bench, Machine, HO, LO, Resource, Ratio)
:- data speed_up/6.
% size(Bench, HO,LO, Size)
:- data size/4.
% wam(Machine,HO,LO,Size)
:- data wam/4.

%center(Center) :-
%	  ( Center = default ; Center = relative ).
center(default).

read_input__2(machine(B)) :- !,
	( current_fact(machine(B)) ->
	    true
	; assertz_fact(machine(B))
	),
	retractall_fact(cur_machine(_)),
	asserta_fact(cur_machine(B)).
read_input__2(optmask(X)) :- !,
	assertz_fact(optmask(X)).
read_input__2(xrange(Resource,Min,Max)) :- !,
	assertz_fact(xrange(Resource,Min,Max)).
read_input__2(ho_description(X)) :- !,
	assertz_fact(ho_description(X)).
read_input__2(ho_alias(X,Alias)) :- !,
	assertz_fact(ho_alias(X,Alias)).
read_input__2(lo_description(X)) :- !,
	assertz_fact(lo_description(X)).
read_input__2(lo_alias(X,Alias)) :- !,
	assertz_fact(lo_alias(X,Alias)).
read_input__2(optmask_ho(X)) :- !,
	assertz_fact(optmask_ho(X)).
read_input__2(optmask_lo(X)) :- !,
	assertz_fact(optmask_lo(X)).
read_input__2(def_opts(X)) :- !,
	assertz_fact(def_opts(X)).
read_input__2(optbit(X,Y)) :- !,
	assertz_fact(optbit(X,Y)).
read_input__2(optcode(X,Y)) :- !,
	assertz_fact(optcode(X,Y)).
read_input__2(opt(Opts0)) :- !,
	( number(Opts0) -> % TODO: for compatibility with old reports (old general opts)
	    Opts1 = Opts0
	; opts_to_num(Opts0, Opts1)
	),
	opts_to_highbits(Opts1, HO),
	opts_to_lowbits(Opts1, LO),
	( current_fact(hi_opts(HO)) ->
	    true
	; assertz_fact(hi_opts(HO))
	),
	( current_fact(lo_opts(LO)) ->
	    true
	; assertz_fact(lo_opts(LO))
	),
	retractall_fact(cur_opts(_,_)),
	asserta_fact(cur_opts(HO,LO)).
read_input__2(try(_)) :- !.
read_input__2(test(B)) :- !,
	( current_fact(bench(B)) ->
	    true
	; assertz_fact(bench(B))
	),
	retractall_fact(cur_bench(_)),
	asserta_fact(cur_bench(B)).
read_input__2(t(_,_,Time)) :- !,
	read_input__2(resource(time,Time)).
read_input__2(resource(Resource,Time)) :- !,
	current_fact(cur_opts(HO,LO)),
	current_fact(cur_bench(B)),
	current_fact(cur_machine(M)),
	( current_fact(resource(Resource)) ->
	    true
	; assertz_fact(resource(Resource))
	),
	% save best time
	( current_fact(time(B,M,HO,LO,Resource,Time0)) ->
%	    TimeD is abs(Time - Time0),
%	    display(delta(B,TimeD)), nl,
	    ( Time0 < Time ->
	        true
	    ; retract_fact(time(B,M,HO,LO,Resource,_)),
	      assertz_fact(time(B,M,HO,LO,Resource,Time))
	    )
	; assertz_fact(time(B,M,HO,LO,Resource,Time))
	).
read_input__2(bytecode_size(Size)) :- !,
	current_fact(cur_opts(HO,LO)),
	current_fact(cur_bench(B)),
	( current_fact(size(B,HO,LO,_)) ->
	    true
	; assertz_fact(size(B,HO,LO,Size))
	).
read_input__2(funcsize(wam__2,Size)) :- !,
	current_fact(cur_opts(HO,LO)),
	current_fact(cur_machine(Machine)),
	assertz_fact(wam(Machine,HO,LO,Size)).

:- use_module(library(aggregates)).
:- use_module(library(sort)).
:- use_module(library(lists)).

line :-
	display('---------------------------------------------------------------------------'), nl.
header(X) :-
	line,
	display(X), nl,
	line.

hi_opts_with_data(Machine, HO) :-
	hi_opts(HO),
	( time(_,Machine,HO,_,_,_) -> true ; fail ).

% TODO: show best and worse numbers

process(Machine,HO,LO,Resource) :-
 	header('Sorted list of (options,wam_size):'),
 	( findall((Size,HO,LO), wam(Machine,HO,LO,Size), As),
 	  sort(As, Bs),
 	  write_list(Bs),
	  fail
 	; true
 	),
	header('Sorted list of normalized (time,options) for each benchmark:'),
	( bench(X),
	    display(X), display(': '), nl,
	    findall((T,HO,LO), speed_up(X,Machine,HO,LO,Resource,T), Es),
	    sort(Es,Fs),
	    write_list(Fs),
	    fail
	; true
	),
	( mean(Test),
	    mean_msg(Test, Msg),
	    header(Msg),
	    findall(m(C,HO,LO),
	      (hi_opts(HO),
	       lo_opts(LO),
	       test_speed_up(Test,Machine,HO,LO,Resource,C)), As),
	    sort(As, Bs),
	    write_list(Bs),
	    fail
	; true
	).

write_list([]).
write_list([A|As]) :-
	write(A), nl,
	write_list(As).

mean_msg(arithmetic, 'Arithmetic mean of benchmark set').
mean_msg(harmonic, 'Harmonic mean of benchmark set').
mean_msg(geometric, 'Geometric mean of benchmark set').
mean_msg(min, 'Worse of benchmark set').
mean_msg(max, 'Best of benchmark set').
mean_msg(bench(X), A) :- atom_concat('Benchmark ', X, A).

test_speed_up(Test,Machine,HO,LO,Resource,C) :-
	( Test = arithmetic ->
	    findall(T, speed_up(_X,Machine,HO,LO,Resource,T), As),
	    \+ As = [],
	    arithmetic_mean(As, C)
	; Test = harmonic ->
	    findall(T, speed_up(_X,Machine,HO,LO,Resource,T), As),
	    \+ As = [],
	    harmonic_mean(As, C)
	; Test = geometric ->
	    findall(T, speed_up(_X,Machine,HO,LO,Resource,T), As),
	    \+ As = [],
	    geometric_mean(As, C)
	; Test = min ->
	    findall(T, speed_up(_X,Machine,HO,LO,Resource,T), As),
	    \+ As = [],
	    sort(As, As2),
	    As2 = [C|_]
	; Test = max ->
	    findall(T, speed_up(_X,Machine,HO,LO,Resource,T), As),
	    \+ As = [],
	    sort(As, As1),
	    reverse(As1, As2),
	    As2 = [C|_]
	; Test = bench(X) ->
	    ( speed_up(X,Machine,HO,LO,Resource,T) -> true ; fail ),
	    C = T
	).

arithmetic_mean(As, C) :-
	sum(As, B),
	length(As, N),
	\+ N = 0,
	C is B/N.

sum([], 0).
sum([A|As], N) :- sum(As, M), N is M+A.

harmonic_mean(As, C) :-
	sum_inv(As, B),
	length(As, N),
	C is N/B.

sum_inv([], 0).
sum_inv([A|As], N) :- sum_inv(As, M), N is M+(1.0/A).

geometric_mean(As, C) :-
	mul(As, B),
	length(As, N),
	C is B ** (1.0/N).

mul([], 1.0).
mul([A|As], N) :- mul(As, M), N is M*A.

% write an options legend
opts_legend :-
%	optmask(OptMask),
%	display('Option mask: '),
%	write(OptMask),
%	nl,
	% Description of HO and LO options
	ho_description(HODesc),
	display('HO description: '),
	display(HODesc),
	nl,
	lo_description(LODesc),
	display('LO description: '),
	display(LODesc),
	nl,
	% List of HO and LO options
	optmask_ho(HOptMask),
	display('HO option list: '),
	write(HOptMask),
	nl,
	optmask_lo(LOptMask),
	display('LO option list: '),
	write(LOptMask),
	nl,
	% Abbreviated HO and LO options
	optmask(OptMask),
	opts_to_num(OptMask, AllOpts),
	opts_to_highbits(AllOpts, AllH),
	opts_to_lowbits(AllOpts, AllL),
	display('Abbreviated HO and LO: ('),
	display(AllH),
	display(','),
	display(AllL),
	display(')'),
	nl,
	% Aliases for HO and LO options
	display('Aliases for HO options:'),
	( ho_alias(HO, HOAlias),
	  display(' ('),
	  display(HO),
	  display(','),
	  display(HOAlias),
	  display(')'),
	  fail
	; true
	),
	nl,
	display('Aliases for LO options:'),
	( lo_alias(LO, LOAlias),
	  display(' ('),
	  display(LO),
	  display(','),
	  display(LOAlias),
	  display(')'),
	  fail
	; true
	),
	nl,
	% Default HO and LO options
	def_opts(DefOpts0),
	opts_to_num(DefOpts0, DefOpts),
	opts_to_highbits(DefOpts, DefH),
	opts_to_lowbits(DefOpts, DefL),
	display('Default options: ('),
	display(DefH),
	display(','),
	display(DefL),
	display(')'),
	nl.

opts_to_highbits(X, Y) :-
	optmask_ho(HighOpts),
	opts_to_codes(HighOpts, X, Codes, []),
	( Codes = [] ->
	    Y = '(none)'
	; atom_codes(Y0, Codes),
	  ( ho_alias(Y0, Y) ->
	      true
	  ; Y = Y0
	  )
	).

opts_to_codes([], _) --> [].
opts_to_codes([O|Os], X) -->
	optbitcode(O,X),
	opts_to_codes(Os, X).

opts_to_lowbits(X, Y) :-
	optmask_lo(LowOpts),
	opts_to_codes(LowOpts, X, Codes, []),
	( Codes = [] ->
	    Y = '(none)'
	; atom_codes(Y0, Codes),
	  ( lo_alias(Y0, Y) ->
	      true
	  ; Y = Y0
	  )
	).

%optbitcode(Opt,X) --> { optbit(Opt, Bit) }, { X /\ Bit =\= 0 }, !, "1".
%optbitcode(_,_) --> "0".

optbitcode(Opt,X) --> { optbit(Opt, Bit) }, { X /\ Bit =\= 0 }, !,
	{ optcode(Opt, C) }, [C].
optbitcode(_,_) --> "-".

opts_to_num(Xs, N) :-
	opts_to_num__2(Xs, 0, N).

opts_to_num__2([], N, N).
opts_to_num__2([X|Xs], N0, N) :-
	optbit(X, B),
	N1 is N0 \/ B,
	opts_to_num__2(Xs, N1, N).

% ---------------------------------------------------------------------------

process_gnuplot(Machine, Test0, Resource, Center, Outdir) :- !,
	display(user_error, 'Generating plot data for '),
	write(user_error, Machine-Center-Test0-Resource),
	nl(user_error),
	%display(user_error, outfile(Outfile)), nl(user_error).
	( mean(Test0) -> Test = Test0 ; Test = bench(Test0) ),
	% Plot in png
	outfile(Machine, Center, Test0, Resource, Outdir, png, OutfilePNG),
	display('set term png'), nl,
	display('set output \''), display(OutfilePNG), display('\''), nl,
	process_gnuplot__2(Machine, Test, Resource, Center),
	% Replot in eps
	outfile(Machine, Center, Test0, Resource, Outdir, eps, OutfileEPS),
	display('set term postscript eps enhanced'), nl,
	display('set output \''), display(OutfileEPS), display('\''), nl,
	process_gnuplot__2(Machine, Test, Resource, Center),
	% get_min_max_speed_up,
	%
	display('set output'), nl,
	!.

process_gnuplot__2(Machine, Test, Resource, Center) :-
	% Generate GNU Plot input
	display('set title "'), mean_msg(Test, TestMsg), display(TestMsg), display('"'), nl,
	display('set xlabel "speed-up '), center_msg(Center, CenterMsg), display(CenterMsg), display('"'), nl,
%	display('set ylabel "high opts"'), nl,
	display('set size 1,0.75'), nl,
	%
	display('set pointsize 1.0'), nl,
	display('set key left top'), nl,
        display('set ytics ('), display_ytics, display(')'), nl,
%	display('plot ['), display(Min),
%	display(':'),
%	display(Max),
%	display('] [-0.25:5.25] '),
%	display('set yrange [-0.30:5.30]'), nl,
	( current_fact(xrange(Resource, MinXRange, MaxXRange)) ->
	    true
	; display(user_error, 'no xrange defined for resource: '),
	  display(user_error, Resource),
	  nl(user_error)
	),
	display('set autoscale y'), nl,
	display('set xrange ['), display(MinXRange), display(':'),
	display(MaxXRange), display(']'), nl,
%	display('set autoscale x'), nl,
	display('set offsets 0.04,0.02,0.5,0.5'), nl,
	% generate plot points
	( lo_opts(LO),
	    retractall_fact(point(LO,_,_)),
	    ( hi_opts_with_data(Machine,HO),
	        ( Center = default ->
		    def_opts(DefOpts0),
		    opts_to_num(DefOpts0, DefOpts),
		    opts_to_highbits(DefOpts, DefHO),
		    opts_to_lowbits(DefOpts, DefLO),
		    calc_speed_up(Machine,DefHO,DefLO,Resource)
		; Center = relative ->
		    def_opts(DefOpts0),
		    opts_to_num(DefOpts0, DefOpts),
		    opts_to_lowbits(DefOpts, DefLO),
		    calc_speed_up(Machine,HO,DefLO,Resource)
		),
	        process_gnuplot_points(Test,Machine,HO,LO,Resource),
	        fail
	    ; true
	    ),
	    fail
	; true
	),
	% write plot points (only if points are found!)
	( lo_opts_with_points(_) ->
	    display('plot '),
	    findall(LO,lo_opts_with_points(LO),LOs),
	    write_with_commas(LOs),
	    nl,
	    ( lo_opts_with_points(LO),
	        write_plot(LO),
		display('e'), nl,
		fail
	    ; true
	    )
	; true
	).

lo_opts_with_points(LO) :-
	lo_opts(LO),
	( current_fact(point(LO,_,_)) -> true ; fail ).

:- data point/3.

outfile(Machine, Center, Test, Resource, Outdir, Ext, Outfile) :-
        atom_concat(Outdir, '/plot-', Outfile0),
        atom_concat(Outfile0, Machine, Outfile1),
        atom_concat(Outfile1, '-', Outfile2),
        atom_concat(Outfile2, Center, Outfile3),
        atom_concat(Outfile3, '-', Outfile4),
        atom_concat(Outfile4, Test, Outfile5),
        atom_concat(Outfile5, '-', Outfile6),
        atom_concat(Outfile6, Resource, Outfile7),
        atom_concat(Outfile7, '.', Outfile8),
        atom_concat(Outfile8, Ext, Outfile).

process_gnuplot_points(Test,Machine,HO,LO,Resource) :-
	findall(m(C,HO,LO),
	  (hi_opts(HO),
	  lo_opts(LO),
	  test_speed_up(Test,Machine,HO,LO,Resource,C)), As),
	sort(As, Bs),
	gen_plot(Bs, Resource).

% Obtain the min and max speed-ups
%get_min_max_speed_up(Resource) :-
%	findall(C,
%	  (hi_opts(HO),
%	   lo_opts(LO),
%	   test_speed_up(_Test,_Machine,HO,LO,Resource,C)), As),
%	sort(As, Bs),
%	Bs = [Min|_],
%	reverse(Bs, Bs2),
%	Bs2 = [Max|_],
%	display(user_error, mm(Min,Max)), nl(user_error).

mean(arithmetic).
mean(harmonic).
mean(geometric).
mean(min).
mean(max).

gen_plot([], _Resource).
gen_plot([m(A,HO,LO)|As], Resource) :-
	opts_to_n(HO, ho, N0),
	opts_to_n(LO, lo, N1),
	!,
	N2 is N1 mod 4,
	N is N0 + N2 * 0.08 - 0.16,
	% only show points that are within the xrange
	current_fact(xrange(Resource,Min,Max)),
	( A >= Min, A =< Max ->
	    assertz_fact(point(LO,A,N))
	; A = 0.Nan ->
	    true
	; A = 0.Inf ->
	    true
	; display(user_error, 'Point '),
	  display(user_error, m(A,HO,LO)),
	  display(user_error, ' out of range ('),
	  display(user_error, Min),
	  display(user_error, ','),
	  display(user_error, Max),
	  display(user_error, ') (modify xrange for resource: '),
	  display(user_error, Resource),
	  display(user_error, ')'),
	  nl(user_error)
	),
	gen_plot(As, Resource).

write_plot(LO) :-
	( current_fact(point(LO,A,N)),
	    display(A), display(' '), write(N), nl,
	    fail
	; true
	).

display_ytics :-
	findall((HO, N), opts_to_n(HO, ho, N), Xs),
	display_ytics__2(Xs).

display_ytics__2([]).
display_ytics__2([(HO,N)]) :- !,
	display('"'), display(HO), display('" '), display(N).
display_ytics__2([(HO,N)|Xs]) :-
	display('"'), display(HO), display('" '), display(N), display(', '),
	display_ytics__2(Xs).

