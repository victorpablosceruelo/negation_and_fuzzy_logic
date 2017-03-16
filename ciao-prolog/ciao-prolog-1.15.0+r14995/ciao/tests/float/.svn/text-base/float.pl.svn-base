:- module(float, [main/0], [assertions]).

:- use_module(library(format), [format/2, format/3]).
:- use_module(library(write)).
:- use_module(library(pretty_print)).
:- use_module(library(system), [file_exists/2, copy_file/2]).
:- use_module(library(system_extra), [readf/2, writef/2]).

%bug: number_codes(2.01601219820229E208,34,A),number_codes(B,34,A).

test_formats(["~0f~N","~1f~N","~10f~N","~100f~N","~1000f~N","~10000f~N",
	"~0g~N","~1g~N","~10g~N","~100g~N",
	"~0e~N","~1e~N","~10e~N","~100e~N"]).

%if you know more problematic numbers, please put it here.

test_numbers([0.0, 1.0, 1.0/0.0, -(1.0), -1.0, -(1.0)/0.0, -1.0/0, 0.Nan,
	1.0e1000, -1.0e1000,0.Inf,-0.Inf,1.0/7.0,-1.0/7.0,
	1.0e207/3.1416, -1.0e113/2.71828,
	2.71828/1.73205e207, -2.71828/1.73297e222,
	19.26, sin(1), cos(1), 4*atan(1.0), 1.01234212554343e208]).

test_bad_bases([-2,-1,0,1,37,38]).

test_bases([
	    2, 3, 4, 5, 6, 7, 8, 9,10,
	11,12,13,14,15,16,17,18,19,20,
	21,22,23,24,25,26,27,28,29,30,
	31,32,33,34,35,36]).

stability_test_format(Stream,Fs,Ns) :-
	member(N,Ns),
	member(F,Fs),
	format(Stream, 'format(\"~s\", ~w) = ', [F, N]),
	format(Stream,F,N),
	fail.
stability_test_format(_,_,_).

stability_test_number_codes(Stream,Bs,Ns) :-
	member(B,Bs),
	member(N,Ns),
	E is N,
	catch((
		  number_codes(E,B,S),
		  pretty_print(Stream,
		  [
		      (:- test number_codes(X, Y, Z) : (X = E, Y = B) => (Z = S)),
		      (:- test number_codes(X, Y, Z) : (Y = B, Z = S) => (X = E))
		  ], [], _)
	      ),
	     Exception,
	     (
		 write(Stream,Exception),
		 write(Stream,'.\n'),
		 write('.'))
	     ),
%	write_canonical(Stream,number_codes(E,B,S)),write(Stream,'\n'),
	fail.

stability_test_number_codes(_,_,_).


%:- push_prolog_flag(multi_arity_warnings,off).

precision_test_number_codes_avg2(N,_A,_B,N,_Base,S,S).
precision_test_number_codes_avg2(I, A, B,N, Base,S,Error) :-
%	Xa is A + I * (B - A) / N,
	Xa is exp(log(A) + I * (log(B) - log(A)) / N),
%	write('testing'), format("~g",X),
	number_codes(Xa,Base,C1),
	number_codes(X,Base,C1),

	number_codes(X,Base,C2),
	number_codes(Y,Base,C2),
% 	number_codes(X,Base,C),
% 	number_codes(Y,Base,C),
	S2 is S + abs((X - Y)/X),
	J is I + 1,!,
	precision_test_number_codes_avg2(J,A,B,N,Base,S2,Error).


precision_test_number_codes_avg(A,B,N,Base,Error) :-!,
	precision_test_number_codes_avg2(0,A,B,N,Base,0,Error1),
	Error is Error1 / N.

precision_test_number_codes_max2(M,_A,_B,N,_Base,S,S,    C,C) :- M is N + 1.
precision_test_number_codes_max2(I, A, B,N, Base,S,Error,X,Cause) :-
	Xa is exp(log(A) + I * (log(B) - log(A)) / N),
	number_codes(Xa,Base,C1),
	number_codes(X1,Base,C1),
	number_codes(X1,Base,C2),
	number_codes(Y,Base,C2),
	S1 is abs((X1 - Y)/X1),
	(   S1>S ->
	    S2=S1,
	    X2=C1
	;
	    S2=S,
	    X2=X
	),
%	max(S1,S,S2),
	J is I + 1,
	!,
	precision_test_number_codes_max2(J,A,B,N,Base,S2,Error,X2,Cause).

precision_test_number_codes_max(A,B,N,Base,Error,Cause) :-!,
	precision_test_number_codes_max2(0,A,B,N,Base,0,Error,"none",Cause).

% predicate that gets the epsilon of the system

get_epsilon_system(N,E) :-!,
	get_epsilon_system2(1.0,0,N,E).

get_epsilon_system2(E,I,N,E2) :-
	A is 1.0 + E/2,!,
	(
	    A =:= 1.0 ->
	    (
		E2 is E,
		N is I
	    );
	    (
		E1 is E / 2.0,
		J is I+1,
		get_epsilon_system2(E1,J,N,E2)
	    )
	).



%:- pop_prolog_flag(multi_arity_warnings).

get_upperbound_error(Base,Exponent,Epsilon,UpperBoundError) :-
	E is ((Exponent + 1) * log(2)/log(Base) + 1)//1,!,
	UpperBoundError is (Base//2)/(Base**E) + Epsilon.

%copy_file(Source,Dest) :-
%	readf(Source,S1),
%	writef(S1,Dest).

handle_ex(E) :-
	write(E),nl.


precision_tests(Stream,Bases,Exponent,Epsilon) :-
	member(Base, Bases),
	precision_test(Stream,Base,Exponent,Epsilon),
	fail.

precision_tests(_Stream,_Bases,_Exponent,_Epsilon).

precision_test(Stream,Base,Exponent,Epsilon) :-
	write('Testing in base '),write(Base),write(':\n'),
	format(Stream,'B=~16g~N',Base),
 	write('Max absolute error:.......................'),
 	precision_test_number_codes_max(1.0123421255434332e-208,
	    4.02335234352e208,13117,Base,MaxError,Cause),
 	write(MaxError), 
	write('\n  raised in number:.......................'),
	atom_codes(ACause,Cause),
	write(ACause), nl,
 	write('Average absolute error:...................'),
 	precision_test_number_codes_avg(1.0123421255434332e-208,
	    4.02335234352e208,13117,Base,AvgError),
% 	precision_test_number_codes_avg(1.0323421255434332e208,
%           4.02335234352e208,3,Base,AvgError),
 	write(AvgError),nl,
 	get_upperbound_error(Base,Exponent,Epsilon,UpperBoundError),

	format('Upper bound error:........................~16g~N',
	    UpperBoundError),
	format(Stream,'U=~16g~N',UpperBoundError),
	Rate is MaxError/UpperBoundError,
	format('Error Propagation Rate, must be <= 1:.....~16g~N',Rate),
	format(Stream,'R=~16g~N',Rate),!,
	(   (Rate =< 1) ->
	    write('Ok'),nl
	;
	    format("Warning: rate greater than 1 (no problem on a PowerPC)~N",
	        [])
	).

:- test main.

main:-
	open('test2.bak',write,Stream),
	test_formats(Fs),
	test_numbers(Ns),
	test_bases(Bs),

	write('Executing print floating point test.'),nl,
	write('Note: the test2.bak file will contain the results'), nl,nl,
	write('Testing predicate format..................'),nl,
	write('Doing Stability test, must not fail.......' ),
	display(Stream, 'Stability test format\n'),
	stability_test_format(Stream,Fs,Ns),write('Ok'),nl,nl,
	write('Testing predicate number_codes............'),nl,

	write('Doing Stability test in critical cases, can fail or throw'),nl,
	write('exceptions, but must not crash the system...'),nl,
	display(Stream, 'Stability test number codes (critical cases)\n'),
	catch(stability_test_number_codes(Stream,[-1,0,1,37,38],Ns),E,
	    handle_ex(E)),nl,
	write('Ok'),nl,

	write('Doing Stability test, must not fail.......' ),
	display(Stream, 'Stability test number codes\n'),
	stability_test_number_codes(Stream,Bs,Ns),write('Ok'),nl,nl,
	write('Doing Precision test\nNote: May differ between platforms'),nl,
	get_epsilon_system(Exponent,Epsilon),
	format("Epsilon of the machine:...................2**-~16g=~16g~N",
	    [Exponent,Epsilon]),
	display(Stream, 'Epsilon of the machine\n'),
	format(Stream,'N=~16g E=~16g~N',[Exponent,Epsilon]),
	precision_tests(Stream,Bs,Exponent,Epsilon),

	close(Stream),
	(   file_exists('test1.bak', 4) ->
	    write('comparing test1.bak with test2.bak........'),
	    readf('test1.bak',O1),
	    readf('test2.bak',O2),
	    (	O1=O2 ->
		write('Ok'),nl,nl
	    ;
		write('Can\'t pass the float test!!!'),nl,
		write('If you have changed the test intentionally,'),nl,
		write('remove the file test1.bak'),nl,
		write('{WARNING: Prior and current tests are not equals}'),nl
	    )
	;
	    write('copying test2.bak in test1.bak'),nl,!,
	    copy_file('test2.bak', 'test1.bak')
	),
	write('All float tests has been passed!!!').
