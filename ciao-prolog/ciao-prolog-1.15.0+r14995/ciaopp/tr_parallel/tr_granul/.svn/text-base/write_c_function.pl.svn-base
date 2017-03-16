:- module(write_c_function, [write_c_function/6], [assertions]). 

:- doc(author,"Pedro L@'{o}pez").  

% Ciao library
:- use_module(library(write), [write/1]). 

% Ciaopp library
:- use_module(name_basic, 
	[
	    temp_name/2,
	    convert_name/2
	]). 

write_c_function(Name,AV,IT,TV,IL,ESize):-
	write_c_header(Name,AV),
	write_c_body(IL,IT,TV,ESize).

write_c_header(Name,AV):-
        convert_to_names(AV,NAV),
	write('double '),
	write(Name),
	write('('),
	write_args(NAV),
	write(')'), nl,
	write('double '),
	write_args(NAV),
	write(';'), nl.

convert_to_names(AV,_):-
	var(AV).
convert_to_names(AV,[NA|NL]):-
	nonvar(AV),
	AV = [A|L],
        convert_name(A,NA),
	convert_to_names(L,NL).

write_args(AV):-
	var(AV).
write_args(AV):-
	nonvar(AV),
	AV = [A|L],
        write(A),
        (
	    nonvar(L) -> write(', ')
	;
	    true
	),
	write_args(L).	

write_c_body(IL,IT,TV,ESize):-
	write('{'), nl,
        write_var_dec(IT,TV),
	write_c_code(IL,0), nl,
        write('return( '),
	write(ESize),
	write(' );'), nl,
        write('}'), nl.

write_var_dec(IT,TV):-
	(
	    (nonvar(IT) ; TV>0) ->
	     write('double '),
	     write_args(IT),
	     (
		 (TV>0)->
		  (
		      nonvar(IT) -> write(', ')
		  ;
		      true
		  ),
		  write_temp(0,TV)
	     ;
		 true
	     ),
	     write(';'), nl, nl
	;
	    true
	).

write_temp(TV,TV):-
	temp_name(TV,Name),
	write(Name).
write_temp(TV,Up):-
	TV < Up,
	temp_name(TV,Name),
	write(Name),write(', '),
	NTV is TV + 1,
	write_temp(NTV,Up).

write_c_code([],_).
write_c_code([Ins|IList],T):-
	write_c_inst(Ins,T),
	write(';'), nl,
	write_c_code(IList,T).

write_c_inst(assig(TV,Exp),T):-
	tab(T),
	write(TV),
	write('='),
	write(Exp).
write_c_inst(sum(IndName,TLow,TUpp,Result,TExp,IL4),T):-
	tab(T),
	write('for( '),
	write(IndName),
	write(' = '),
	write(TLow),
        write(', '),
	write(Result),
	write(' = 0;'), nl,
        T1 is T + 5,
	tab(T1),
        write(IndName ),
	write(' <= ' ),
	write(TUpp ),
	write(' ;' ), nl,
	tab(T1),
        write(Result),
	write(' += '),
	write(TExp ),
	write(' , ' ),
        write(IndName),
	write(' ++ )'),
        NT is T1 + 5,
	write_for_body(IL4,NT).
write_c_inst(prod(IndName,TLow,TUpp,Result,TExp,IL4),T):-
	tab(T),
        write('for( '),
	write(IndName),
	write(' = '),
	write(TLow),
        write(', '),
	write(Result),
	write(' = 1;'), nl,
        T1 is T + 5,
	tab(T1),
        write(IndName ),
	write(' <= ' ),
	write(TUpp ),
	write(' ;' ), nl,
	tab(T1),
        write(Result),
	write(' *= '),
	write(TExp ),
	write(' , ' ),
        write(IndName),
	write(' ++ )'),
        NT is T1 + 5,
	write_for_body(IL4,NT).
write_c_inst(fact(IndName,Result,TExp),T):-
	tab(T),
        write('for( '),
	write(IndName),
	write(' = 1, '),
	write(Result),
        write(' = 1'),
	write(' ;' ), nl,
        T1 is T + 5,
	tab(T1),
        write(IndName ),
	write(' <= ' ),
	write(TExp ),
	write(' ;' ), nl,
	tab(T1),
        write(Result),
	write(' *= '),
	write(IndName ),
	write(' , ' ),
        write(IndName),
	write(' ++ )').

write_for_body([],_).
write_for_body([Ins],T):-
	nl,
	write_c_inst(Ins,T).
write_for_body(InsList,T):-
        InsList=[_,_|_],
        tab(T),
        write('{'), nl,
	write_c_code(InsList,T),
	tab(T),
        write('}').


