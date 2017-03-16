:- module(deriv,
	[
	    seq/2,
	    par/2,
	    par_nondet/2,
	    data/1
	],
	[fsyntax, andprolog_nd]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sizes(100).
gc(100).

data([Exp,V]) :- sizes(N), prepare_gc(N,Exp,V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seq([Exp,V],X) :- d_seq(Exp,V,X).
par([Exp,V],X) :- gc(GC), d_par_gc(Exp,V,GC,X).
par_nondet([Exp,V],X) :- gc(GC), d_par_nondet_gc(Exp,V,GC,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

d(U+V,X,DU+DV)	         :- !,d(V,X,DV),d(U,X,DU).
d(U-V,X,DU-DV)	         :- !,d(V,X,DV),d(U,X,DU).
d(U*V,X,DU*V+U*DV)       :- !,d(V,X,DV),d(U,X,DU).
d(U/V,X,(DU*V-U*DV)/V^2) :- !,d(V,X,DV),d(U,X,DU).
d(U^N,X,DU*N*U^N1)       :- !,integer(N),N1 is N-1,d(U,X,DU).
d(-U,X,-DU)              :- !,d(U,X,DU).
d(exp(U),X,exp(U)*DU)    :- !,d(U,X,DU).
d(log(U),X,DU/U)         :- !,d(U,X,DU).
d(X,X,1)                 :- !.
d(_C,_X,0).

d_par_par(U+V,X,DU+DV)           :- !,d_par_par(V,X,DV) '&!' d_par_par(U,X,DU).
d_par_par(U-V,X,DU-DV)           :- !,d_par_par(V,X,DV) '&!' d_par_par(U,X,DU).
d_par_par(U*V,X,DU*V+U*DV)       :- !,d_par_par(V,X,DV) '&!' d_par_par(U,X,DU).
d_par_par(U/V,X,(DU*V-U*DV)/V^2) :- !,d_par_par(V,X,DV) '&!' d_par_par(U,X,DU).
d_par_par(U^N,X,DU*N*U^N1)       :- !,integer(N),N1 is N-1,d_par_par(U,X,DU).
d_par_par(-U,X,-DU)              :- !,d_par_par(U,X,DU).
d_par_par(exp(U),X,exp(U)*DU)    :- !,d_par_par(U,X,DU).
d_par_par(log(U),X,DU/U)         :- !,d_par_par(U,X,DU).
d_par_par(X,X,1)                 :- !.
d_par_par(_C,_X,0).

d_par_nondet(U+V,X,DU+DV)           :- !,d_par_nondet(V,X,DV) & d_par_nondet(U,X,DU).
d_par_nondet(U-V,X,DU-DV)           :- !,d_par_nondet(V,X,DV) & d_par_nondet(U,X,DU).
d_par_nondet(U*V,X,DU*V+U*DV)       :- !,d_par_nondet(V,X,DV) & d_par_nondet(U,X,DU).
d_par_nondet(U/V,X,(DU*V-U*DV)/V^2) :- !,d_par_nondet(V,X,DV) & d_par_nondet(U,X,DU).
d_par_nondet(U^N,X,DU*N*U^N1)       :- !,integer(N),N1 is N-1,d_par_nondet(U,X,DU).
d_par_nondet(-U,X,-DU)              :- !,d_par_nondet(U,X,DU).
d_par_nondet(exp(U),X,exp(U)*DU)    :- !,d_par_nondet(U,X,DU).
d_par_nondet(log(U),X,DU/U)         :- !,d_par_nondet(U,X,DU).
d_par_nondet(X,X,1)                 :- !.
d_par_nondet(_C,_X,0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions with granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Benchmarks obtained with threshold of size 100 (the expression
% generated has a size of 230 and the benchmark runs with a expression
% that is the sum of N of these expressions).

size((_,T),T).

d_seq((U+V,_),X,DU+DV)           :- !,d_seq(V,X,DV),d_seq(U,X,DU).
d_seq((U-V,_),X,DU-DV)           :- !,d_seq(V,X,DV),d_seq(U,X,DU).
d_seq((U*V,_),X,DU*V+U*DV)       :- !,d_seq(V,X,DV),d_seq(U,X,DU).
d_seq((U/V,_),X,(DU*V-U*DV)/V^2) :- !,d_seq(V,X,DV),d_seq(U,X,DU).
d_seq((U^N,_),X,DU*N*U^N1)       :- !,N=(N2,T),integer(N2),N3 is N2-1,N1=(N3,T),d(U,X,DU).
d_seq((-U,_),X,-DU)              :- !,d_seq(U,X,DU).
d_seq((exp(U),_),X,exp(U)*DU)    :- !,d_seq(U,X,DU).
d_seq((log(U),_),X,DU/U)         :- !,d_seq(U,X,DU).
d_seq((X,_),X,1)                 :- !.
d_seq(_,_,0).

d_gc(E,X,Gran,R) :- ~size(E) > Gran -> d_par_gc(E,X,Gran,R)  ; d_seq(E,X,R).
d_ngc(E,X,Gran,R):- ~size(E) > Gran -> d_par_nondet_gc(E,X,Gran,R) ; d_seq(E,X,R).

d_par_gc((U+V,_),X,Gran,DU+DV)           :- !,d_gc(V,X,Gran,DV) '&!' d_gc(U,X,Gran,DU).
d_par_gc((U-V,_),X,Gran,DU-DV)           :- !,d_gc(V,X,Gran,DV) '&!' d_gc(U,X,Gran,DU).
d_par_gc((U*V,_),X,Gran,DU*V+U*DV)       :- !,d_gc(V,X,Gran,DV) '&!' d_gc(U,X,Gran,DU).
d_par_gc((U/V,_),X,Gran,(DU*V-U*DV)/V^2) :- !,d_gc(V,X,Gran,DV) '&!' d_gc(U,X,Gran,DU).
d_par_gc((U^N,_),X,Gran,DU*N*U^N1)       :- !,N=(N2,T),integer(N2),N3 is N2-1,N1=(N3,T),d_gc(U,X,Gran,DU).
d_par_gc((-U,_),X,Gran,-DU)              :- !,d_gc(U,X,Gran,DU).
d_par_gc((exp(U),_),X,Gran,exp(U)*DU)    :- !,d_gc(U,X,Gran,DU).
d_par_gc((log(U),_),X,Gran,DU/U)         :- !,d_gc(U,X,Gran,DU).
d_par_gc((X,_),X,_,1)                    :- !.
d_par_gc(_,_,_,0).

d_par_nondet_gc((U+V,_),X,Gran,DU+DV)           :- !,d_ngc(V,X,Gran,DV) & d_ngc(U,X,Gran,DU).
d_par_nondet_gc((U-V,_),X,Gran,DU-DV)           :- !,d_ngc(V,X,Gran,DV) & d_ngc(U,X,Gran,DU).
d_par_nondet_gc((U*V,_),X,Gran,DU*V+U*DV)       :- !,d_ngc(V,X,Gran,DV) & d_ngc(U,X,Gran,DU).
d_par_nondet_gc((U/V,_),X,Gran,(DU*V-U*DV)/V^2) :- !,d_ngc(V,X,Gran,DV) & d_ngc(U,X,Gran,DU).
d_par_nondet_gc((U^N,_),X,Gran,DU*N*U^N1)       :- !,N=(N2,T),integer(N2),N3 is N2-1,N1=(N3,T),d_ngc(U,X,Gran,DU).
d_par_nondet_gc((-U,_),X,Gran,-DU)              :- !,d_ngc(U,X,Gran,DU).
d_par_nondet_gc((exp(U),_),X,Gran,exp(U)*DU)    :- !,d_ngc(U,X,Gran,DU).
d_par_nondet_gc((log(U),_),X,Gran,DU/U)         :- !,d_ngc(U,X,Gran,DU).
d_par_nondet_gc((X,_),X,_,1)                    :- !.
d_par_nondet_gc(_,_,_,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prepare(N,LExp,V) :-
	mexpr(N,LExp),
	V = x,
	!.

mexpr(0,x).
mexpr(X, Exp + Rest) :- 
	X>0,
	expression(Exp), 
	Y is X-1,
	mexpr(Y,Rest).

expression( Exp + Exp - Exp * Exp / Exp * Exp / Exp ) :- value(Exp).

value(((3*x + (4*exp(x^3)*log(x^2)) -2) / ( -(3*x) + 5/(exp(x^4)+2)))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prepare_gc(N,LExp,V) :-
	mexpr_gc(N,LExp),
	V = x,
	!.

mexpr_gc(X,Y) :-
	mexpr_gc_(X,(0,1),Y).
mexpr_gc_(0,X,X).
mexpr_gc_(X,E,FinalExp) :- 
	X>0,
	!,
	expression_gc(Exp), 
	X1 is X-1,
	mexpr_gc_(X1,(~join2(E, +, Exp)),FinalExp).

% Size of E is 230.
expression_gc(E) :-
	value_gc(Exp),
	E = (~join2((~join2(Exp,+,Exp)),-,(~join2(Exp, *, (~join2(Exp,/,(~join2(Exp, *, (~join2(Exp, *, Exp)))))))))).

join1(Op, A, (Exp, T)):-
        Exp =.. [Op, A],
        A = (_EA, TA),
        T is TA + 1.

join2(A, Op, B, (Exp, T)):-
        Exp =.. [Op, A, B],
        A = (_EA, TA),
        B = (_EB, TB),
        T is TA + TB + 1.

% Size of E is 32.
value_gc(E) :-
	N = (~join2((~join2((3,1), *, (x,1))), +, (~join2((~join2((~join2((4,1), *, (~join1(exp, (~join2((x,1), ^, (3,1))))))), *, (~join1(log, (~join2((x,1), ^, (2,1))))))), -, (2,1))))),
	M = (~join2((~join1(-, (~join2((3,1), *, (x,1))))), +, (~join2((5,1), /, (~join2((~join1(exp, (~join1(exp, (~join2((x,1), ^, (4,1))))))), +, (2,1))))))),
	E = (~join2(N, /, M)).

