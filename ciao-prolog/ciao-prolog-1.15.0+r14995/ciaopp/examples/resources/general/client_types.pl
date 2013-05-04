:- module(_, [
% regtypes
		options/1,
		byte/1,
		socket_stream/1,
		host/1,
		port/1], [assertions, regtypes]).

%% REGTYPES
:- regtype options/1.
options([Host, Port]) :-
	host(Host),
	port(Port).

:- regtype byte/1.
byte(X) :- atm(X).

:- regtype socket_stream/1.
socket_stream(X) :- atm(X).

:- regtype host/1.
host(H) :- atm(H).

:- regtype port/1.
port(P) :- num(P).
