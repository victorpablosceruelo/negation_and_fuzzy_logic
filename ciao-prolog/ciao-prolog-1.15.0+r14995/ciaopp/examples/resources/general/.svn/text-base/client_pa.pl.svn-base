:- module(_1,[main/3],[assertions,nativeprops,regtypes,ciaopp(tests(resources)),basicmodes]).

:- use_module(client_types,[options/1,byte/1,socket_stream/1,host/1,port/1]).

:- load_resource_module(client_res).

:- resource bits_received.

:- head_cost(ub,bits_received,'client:head_bits_received').

:- literal_cost(ub,bits_received,'client:lit_bits_received').

:- impl_defined([connect_socket/3,close_socket/1,exchange_byte/3]).

:- entry main(Options,IBuf,OBuf)
         : ( options(Options), list(IBuf,byte), var(OBuf) ).

:- true pred main(Options,IBuf,OBuf)
         : ( options(Options), list(IBuf,byte), term(OBuf) )
        => ( options(Options), list(IBuf,byte), list(OBuf,byte) ).

:- true pred main(Options,IBuf,OBuf)
         : ( mshare([[OBuf]]), var(OBuf), ground([Options,IBuf]) )
        => ground([Options,IBuf,OBuf]).

:- true pred main(Options,IBuf,OBuf)
         : ( options(Options), list(IBuf,byte), var(OBuf) )
        => ( options(Options), list(IBuf,byte), list(OBuf,byte) )
         + ( not_fails, covered ).

:- true pred main(Options,IBuf,OBuf)
         : ( options(Options), list(IBuf,byte), var(OBuf) )
        => ( options(Options), list(IBuf,byte), list(OBuf,byte), size(ub,Options,2), size(ub,IBuf,length(IBuf)), size(ub,OBuf,length(IBuf)) )
         + cost(ub,bits_received,8*length(IBuf)).

main([Host,Port],IBuf,OBuf) :-
        connect_socket(Host,Port,Stream),
        exchange_buffer(IBuf,Stream,OBuf),
        close_socket(Stream).

:- true pred exchange_buffer(_2,_1,_3)
         : ( list(_2,byte), socket_stream(_1), term(_3) )
        => ( list(_2,byte), socket_stream(_1), list(_3,byte) ).

:- true pred exchange_buffer(_2,_1,_3)
         : ( mshare([[_3]]), var(_3), ground([_2,_1]) )
        => ground([_2,_1,_3]).

:- true pred exchange_buffer(_2,_1,_3)
         : ( list(_2,byte), socket_stream(_1), var(_3) )
        => ( list(_2,byte), socket_stream(_1), list(_3,byte) )
         + ( not_fails, covered ).

:- true pred exchange_buffer(_2,_1,_3)
         : ( list(_2,byte), socket_stream(_1), var(_3) )
        => ( list(_2,byte), socket_stream(_1), list(_3,byte), size(ub,_2,length(_2)), size(ub,_1,size(_1)), size(ub,_3,length(_2)) )
         + cost(ub,bits_received,8*length(_2)).

exchange_buffer([],_1,[]).
exchange_buffer([B|Bs],Id,[B0|Bs0]) :-
        exchange_byte(B,Id,B0),
        exchange_buffer(Bs,Id,Bs0).


