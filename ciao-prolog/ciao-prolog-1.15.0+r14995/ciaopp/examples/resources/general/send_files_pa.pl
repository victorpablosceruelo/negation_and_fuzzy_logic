:- module(_1,[send_files/1],[assertions,regtypes,nativeprops,ciaopp(tests(resources)),library(resdefs(resources_decl)),basicmodes]).

:- resource data_read.

:- head_cost(ub,data_read,0).

:- literal_cost(ub,data_read,0).

:- impl_defined([open_/3,close_/1,read/2,write/2]).

:- entry send_files(_1)
         : list(_1,atm).

:- true pred send_files(FileList)
         : list(FileList,atm)
        => list(FileList,atm).

:- true pred send_files(FileList)
         : ground([FileList])
        => ground([FileList]).

:- true pred send_files(FileList)
         : list(FileList,atm)
        => list(FileList,atm)
         + ( not_fails, covered ).

:- true pred send_files(FileList)
         : list(FileList,atm)
        => ( list(FileList,atm), size(ub,FileList,length(FileList)) )
         + cost(ub,data_read,inf).

send_files(FileList) :-
        open_(all_files,write,OS),
        send_files_stream(FileList,OS),
        close_(OS).

:- true pred send_files_stream(_1,_OS)
         : ( list(_1,atm), int(_OS) )
        => ( list(_1,atm), int(_OS) ).

:- true pred send_files_stream(_1,_OS)
         : ground([_1,_OS])
        => ground([_1,_OS]).

:- true pred send_files_stream(_1,_OS)
         : ( list(_1,atm), int(_OS) )
        => ( list(_1,atm), int(_OS) )
         + ( not_fails, covered ).

:- true pred send_files_stream(_1,_OS)
         : ( list(_1,atm), int(_OS) )
        => ( list(_1,atm), int(_OS), size(ub,_1,length(_1)), size(ub,_OS,int(_OS)) )
         + cost(ub,data_read,inf).

send_files_stream([],_OS).
send_files_stream([F|Fs],OS) :-
        open_(F,read,IS),
        read(IS,E),
        write(OS,E),
        close_(IS),
        send_files_stream(Fs,OS).


