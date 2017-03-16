:- module(bshare_utils,_,
% 	[write_pos_db/2,
% 	 write_neg_db/2,
% 	 read_db/2,
% 	 read_negdb/3,
% 	 concat_suffix/3,
% 	 % experiments
% 	 write_neg_db_stream/2],
	[assertions]).

:- use_module(library(write), [write/2]).
:- use_module(library(strings), [get_line/2]).
:- use_module(library(lists), [length/2]).
:- use_module(library(terms), [atom_concat/2]).

%----------------------------------------------------------------------------%
% write_pos_db(+Sh,+ASub)                                                
%----------------------------------------------------------------------------%
write_pos_db(FileName,Sh):-
	open(FileName,write,Stream),
	write_string_list(Sh,Stream),
	close(Stream).

%----------------------------------------------------------------------------%
% write_neg_db(+NSh,+ASub)                                             
%----------------------------------------------------------------------------%
write_neg_db(NSh,ASub):-
	open(NSh,write,Stream),
	write_length(Stream,ASub),
	write_string_list(ASub,Stream),
	close(Stream).

write_neg_db_stream(Stream,ASub):-
	write_length(Stream,ASub),
	write_string_list(ASub,Stream).

write_length(Stream,ASub):-
	get_length(ASub,Len),
	write(Stream,b),
	write(Stream,Len),
	nl(Stream).

get_length([],0).
get_length([X|_],Len):-
	length(X,Len).
	
write_string_list([],_).
write_string_list([S],Stream):-!,
	write_string(S,Stream),
	nl(Stream).
write_string_list([S|Ss],Stream):-!,
	write_string(S,Stream),
	nl(Stream),
	write_string_list(Ss,Stream).	

write_string([],_):-!.
write_string([S|Ss],Stream):-
	write(Stream,S),
	write_string(Ss,Stream).

%----------------------------------------------------------------------------%
% read_db(+FileName,-Ss)
%----------------------------------------------------------------------------%
% It reads both positive and negative database from @var{FileName} returning 
% its content in @var{Ss} as a set of sets.
%----------------------------------------------------------------------------%
read_db(FileName,Ss):-
	open(FileName,read,Stream),
	read_string_list(Stream,[],Ss),
	close(Stream).

read_string_list(Stream,Acc,Res):-
	get_number_line(Stream,S,EOF,LFlag),!,
	( EOF == 'end_of_file' ->
	  Acc = Res
	;
	  ( LFlag == 'yes' ->	   
	    read_string_list(Stream,Acc,Res)
          ;
	    read_string_list(Stream,[S|Acc],Res)
          ) 
	).
	  	
get_number_line(Stream,String,EOF,LFlag):-
	get_line(Stream,Codes_List),!,
	( Codes_List == 'end_of_file' ->
	  EOF = 'end_of_file'
	;
	  process_line(Codes_List,String,LFlag)	  
        ).

process_line([],[],_).
process_line([X|Xs],[Y|Ys],LFlag):-
	number_codes(Y,[X]),!,
	process_line(Xs,Ys,LFlag).
process_line([X|Xs],[*|Ys],LFlag):-
	atom_codes(*,[X]),!,
	process_line(Xs,Ys,LFlag).
process_line([X|Xs],[Y|Ys],yes):-
	atom_codes(Y,[X]),!,
	process_line(Xs,Ys,_).
	
concat_suffix(Atm,Id,Atm0):-
	atm(Id),!,
	atom_concat(Atm,Id,Atm0).
concat_suffix(Atm,Id,Atm0):-
	number(Id),!,
	atom_number(Id0,Id),
	atom_concat(Atm,Id0,Atm0).

% read_negdb(FileName,Vars,(NSh,Vars)):-
%      read_db(FileName,NSh).
	
read_negdb(FileName,Vars,(NSh,Vars)):-
     read_db(FileName,NSh0),
     ( NSh0 = [] ->
       %% if NDB is empty then add 0...0 (full in the positive)
       mylength(Vars,0,N),
       paddle(1,N,[],0,All_zero_string),
       insert_empty(All_zero_string,[],NSh)
     ;
       NSh = NSh0	 
     ).	 
%---------------------------------------------------------------------------%	
%              INTERMEDIATE OPERATIONS
%---------------------------------------------------------------------------%	

% paddle(Start,End,Acc,Val,Res)
paddle(_,0,S,_,S):-!.
paddle(K,N,S,Val,[Val|S]):-
     K = N,!.	
paddle(K,N,S,Val,S1):-
     K < N,!,
     K1 is K + 1,
     paddle(K1,N,[Val|S],Val,S1).
paddle(_,_,S,_,[0|S]):-!.	

mylength([],Acc,Acc).
mylength([_|Xs],Acc,L):-
	NAcc is 1 + Acc,
	mylength(Xs,NAcc,L).

% vars_to_bits(S_vars,Vars,S_bits)
vars_to_bits(_,[],[]):-!.
vars_to_bits([],Ys,Rs):-
     mylength(Ys,0,N),
     paddle(1,N,[],0,Rs),!.
vars_to_bits([X|Xs],[Y|Ys],Rs):-
     compare(Order,X,Y),
     vars_to_bits_(Order,X,Y,Xs,Ys,Rs),!.
vars_to_bits_(>,X,_,Xs,Ys,[0|Rs]):-
     vars_to_bits([X|Xs],Ys,Rs),!.
vars_to_bits_(=,_,_,Xs,Ys,[1|Rs]):-
     vars_to_bits(Xs,Ys,Rs),!.

% shvars2shbits(Ss_vars,Vars,Ss_bits)
shvars2shbits('$bottom',_Vars,'$bottom').
shvars2shbits([],_Vars,[]).
shvars2shbits([Xs|Xss],Vars,[Ys|Yss]):-
	vars_to_bits(Xs,Vars,Ys),
	shvars2shbits(Xss,Vars,Yss).

% bits_to_vars(Ss_bits,Vars,Ss_vars)
bits_to_vars('$bottom',_, '$bottom').
bits_to_vars([],_,[]).
bits_to_vars([S|Ss],Vars,[V|Vs]):-
	bits_to_vars_(S,Vars,V),
	bits_to_vars(Ss,Vars,Vs).

bits_to_vars_([],[],[]).
bits_to_vars_([0|Xs],[_|Vs],Rs):-!,
	bits_to_vars_(Xs,Vs,Rs).
bits_to_vars_([1|Xs],[V|Vs],[V|Rs]):-!,
	bits_to_vars_(Xs,Vs,Rs).

% binlist_to_atm(List,Atm)
% List is a list of 0's and 1's
binlist_to_atm(Xs,Atm):-
     binlist_to_atm_(Xs,Atm_L),
     atom_concat(Atm_L,Atm).
binlist_to_atm_([],[]).
binlist_to_atm_([0|Xs],['0'|Ys]):-
     binlist_to_atm_(Xs,Ys).
binlist_to_atm_([1|Xs],['1'|Ys]):-
     binlist_to_atm_(Xs,Ys).
	
increment(X,Y,Incr):-
     Sum is X + Y,
     Incr is Sum - X.


to_neg(0,0):- !.
to_neg(N,Res):-!,
	Res is - N.

remove_module_name(ModPred,Pred):-
	atom_concat(X,Pred,ModPred),
	atom_concat(_Mod,':',X).
remove_module_name(Pred,Pred).


insert_empty([],Ls,Ls):-!.
insert_empty(L,Ls,[L|Ls]):-!.
