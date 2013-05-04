:- module(send_files, [send_files/1], [assertions, regtypes, nativeprops,
		ciaopp(tests(resources)),
		library(resdefs(resources_decl))]).

:- resource data_read.
:- head_cost(ub, data_read, 0).
:- literal_cost(ub, data_read, 0).

:- entry send_files/1 : list(atm).

send_files(FileList) :-
	open_(all_files, write, OS),
	send_files_stream(FileList, OS),
	close_(OS).

send_files_stream([],     _OS).
send_files_stream([F|Fs], OS) :-
	open_(F, read, IS),
	read(IS, E),
	write(OS, E),
	close_(IS),
	send_files_stream(Fs, OS).

:- impl_defined([open_/3, close_/1, read/2, write/2]).

:- trust pred open_(FileName, Mode, Stream) :
	(atm(FileName), atm(Mode), var(Stream))
	=> ( atm(FileName), atm(Mode), int(Stream),
	    size(FileName, size(FileName)),
	    size(Mode, size(Mode)),
	    size(Stream, int(Stream)) )
	+ (not_fails, is_det, cost(ub, data_read, 0)).

:- trust pred close_(Stream) : (int(Stream))
	+ (not_fails, is_det, cost(ub, data_read, 0)).

:- trust pred read(Stream, Data) : (int(Stream), var(Data))
	=> (int(Stream), list(Data, atm))
	+ (not_fails, is_det, cost(ub, data_read, length(Data))).

:- trust pred write(Stream, Data) : (int(Stream), list(Data, atm))
	+ (not_fails, is_det, cost(ub, data_read, 0)).
