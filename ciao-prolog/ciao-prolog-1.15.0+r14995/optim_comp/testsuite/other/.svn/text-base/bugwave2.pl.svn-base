:- module(_,[wave_orig/0], []).

:- data emg/2.
:- data nSamples/1.
:- data sampleRate/1.

:- use_module(library(format)).

:- use_module(library(prolog_sys)).

wave_orig:-
	display(user_error, ok), nl(user_error),
        testLearning(_J, T1, T2).

elem_.

elem.

% Another way of dealing with arrays is
% through retract and asserts.
sawtooth(I, _X, _Step) :- nSamples(J), J=I, !.
sawtooth(I, X, Step).

% A fake electromyographic signal, just for test.
signal(sawtooth, NSamples) :- 
  retractall_fact(emg(_, _)),
  sampleRate(Step),
  sawtooth(0, 1.0, Step).

newA(A, I, MU, Yhat, N, M, [X|NA]) :-
    elem(A, I, AI),
   ( I > N -> X is AI ;
     emg(N,YN),
     Ni is N-I,
     emg(Ni, YNi),
     X is AI-2.0*MU*(YN-Yhat)*YNi),
    I1 is I+1,
    newA(A, I1, MU,  Yhat, N, M, NA).

yHat(A, N, M, YHAT) :- M < N, !,
   UNTIL= M,
   yHATloop(A, 0, N, UNTIL, YHAT1),
   YHAT is -YHAT1.

yHATloop(_A,  I, _N, UNTIL, YHAT) :- I>UNTIL, !, YHAT= 0.0.

test_yHat(Expected, YouGot) :-
   A= [-0.128147, -0.0928003, -0.0586378, -0.0271271],
   retractall_fact(sampleRate(_)),
   assertz_fact(sampleRate(0.2)),
   signal(sawtooth, 20),
   N is  4,
   yHat(A, N, 3, YouGot),
   Expected= 0.4937902.

% A naive test to check whether everything is OK.
test_newA(Expected, NA) :- 
   A = [0, 1, 2, 3],
   MU is 0.01,
   retractall_fact(sampleRate(_)),
   assertz_fact(sampleRate(0.2)),
   signal(sawtooth, 20),
   Yhat= 0.3,
   newA(A, 0, MU, Yhat, 2, 3, NA),
   Expected= [-0.0308, 0.9736, 1.978, 3].

% One step of the learning algorithm.
learningLoop(N, _M, _MU, A, NewA, []) :-
    nSamples(N), !, NewA= A.

length([], 0) :- !.
  
featureRetrieving(A, MU, f(NewA, YHATvector)) :-
   length(A, LenA),
   M is LenA-1,
   learningLoop(0, M, MU, A, NewA, YHATvector).

test_feature(F) :-
   A = [0, 0, 0, 0],
   MU is 0.01,
   signal(sawtooth, 20),
   featureRetrieving(A, MU, F).

errorEstimation(_I, [], Es, E) :- !,
  average(Es, 0.0, 0.0, M),
  deviation(Es, M, 0.0, 0.0, E).

average([], S, L, Avg) :- !, Avg is S/L.

deviation([], _M, S, L, E) :- !, E is S/L.

maxIter(100).
minError(0.01).

getFeatures_(J, A, MU, Features, Estimation, Iterations) :-
      featureRetrieving(A, MU, f(NewA, YHATvector)),
      errorEstimation(0, YHATvector, [], EErr),
      minError(MErr), 
      EErr > MErr, !,
      J1 is J+1,
      getFeatures_(J1, NewA, MU, Features, Estimation, Iterations).
getFeatures_(J, A, MU, Features, Estimation, Iterations) :-
     featureRetrieving(A, MU, f(Features, Estimation)),
     Iterations= J.

makeFirstA(0, []) :- !.
makeFirstA(N, [0|XS]) :- N1 is N-1, makeFirstA(N1, XS).
     
testLearning(Iterations, T1, T2) :-
   MU is 0.01,
   display(user_error, hola), nl(user_error),
   retractall_fact(sampleRate(_)),
   display(user_error, hola1), nl(user_error),
   assertz_fact(sampleRate(0.0001)),
   display(user_error, hola2), nl(user_error),
   statistics(runtime, _),
   display(user_error, hola3), nl(user_error),
   signal(sawtooth, 20000),
   display(user_error, hola4), nl(user_error),
   statistics(runtime, [_,T1]),
   display(user_error, hola5), nl(user_error),
   NFeatures= 4,
   display(user_error, hola6), nl(user_error),
   getFeatures(NFeatures, MU, _Features, Estimation, Iterations).
