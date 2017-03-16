:- module(client, [main/3], [assertions, nativeprops, regtypes, 
		ciaopp(tests(resources))]).

:- use_module(client_types,
	[options/1, byte/1,socket_stream/1,host/1,port/1]).

:- load_resource_module(client_res).
:- resource bits_received.
:- head_cost(ub, bits_received, head_bits_received).
:- literal_cost(ub, bits_received, lit_bits_received).

:- entry main(Options, IBuf, OBuf) : options * list(byte) * var.

:- check pred main(Options, IBuf, OBuf): options * list(byte) * var 
              + cost(ub, bits_received, exp(length(IBuf),2) ).

main([Host, Port], IBuf, OBuf) :-
	connect_socket(Host, Port, Stream),
	exchange_buffer(IBuf, Stream, OBuf),
	close_socket(Stream).

exchange_buffer([],     _,  []).
exchange_buffer([B|Bs], Id, [B0|Bs0]) :-
	exchange_byte(B, Id, B0),
	exchange_buffer(Bs, Id, Bs0).

%% SOCKET LIBRARY

:- impl_defined([connect_socket/3, close_socket/1, exchange_byte/3]).

:- trust pred connect_socket(Host, Port, Stream)
	: host * port * var
	=> host * port * socket_stream
	+ (not_fails, is_det, cost(ub, bits_received, 0)).

:- trust pred close_socket(Stream)
	: socket_stream => socket_stream
	+ (not_fails, is_det, cost(ub, bits_received, 0)).

:- trust pred exchange_byte(X, Y, Z)
	: byte * socket_stream * var
	=> byte * socket_stream * byte
	+ (not_fails, is_det, cost(ub, bits_received, 8)).
