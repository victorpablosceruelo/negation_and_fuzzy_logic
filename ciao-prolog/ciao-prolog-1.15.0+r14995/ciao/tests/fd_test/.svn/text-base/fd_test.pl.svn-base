:- module(fd_test, [main/0], [assertions]).

:- use_module(fdqueens).
:- use_module(smm).
:- use_module(dgr).
:- use_module(magic_square).
:- use_module(crypta).

:- test main.

main:-
        tsmm,
        tdgr,
	 tcrypta,
        tqueens,
        tmagic_square,
	!.

tsmm:-
       line,
       message(['SEND + MORE = MONEY problem.']),
       smm(L),
       message(['Solution: [S,E,N,D,M,O,R,Y] = ', L]).
tsmm.

tdgr:-
       line,
       message(['DONALD + GERALD = ROBERT problem.']),
       dgr(L),
       message(['Solution: [D,G,R,O,E,N,B,A,L,T] = ', L]).
tdgr.

tcrypta :-
	line,
	message(['Crypta problem: ', 'BAIJJAJIIAHFCFEBBJEA + DHFGABCDIDBIFFAGFEJE = GJEGACDDHFAFJBFIHEEF ', '.']),
	crypta(L),
	message(['Solution: [A,B,C,D,E,F,G,H,I,J] = ', L]).
tcrypta.

tqueens :-
	line,
	message(['All solutions to the ', 11, ' queens problem.']),
	queens(11, Qs),
	message(['Solution: [Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11] = ', Qs]).
tqueens.

tmagic_square :-
	line,
	message(['Magic square of size ', 4, '.']), nl,
	magic_square(4, [M1, M2, M3, M4]),
	message(['Solution: ']),
	message(['[M11, M12, M13, M14] = ', M1]),
	message(['[M21, M22, M23, M24] = ', M2]),
	message(['[M21, M22, M23, M24] = ', M3]),
	message(['[M21, M22, M23, M24] = ', M4]).

tmagic_square.

line:- 
	nl,
	display(
'***************************************************************************'
), 
	nl.
