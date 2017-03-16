:- module(pack_tr,_,[]).

:- use_module(dist,[dist/2]).

%dpred(end_of_file,[stored_pred('dist:dist'(X,Y),dist(X,Y)),end_of_file],_):- 
%	!.
dpred(end_of_file,[end_of_file],_):- 
	!.
dpred((Head :- Body), [(Head :- Body),(stored_clause(Head,ListBody))],_):-
	!,
	conj_to_list(Body,ListBody).
dpred(Fact, [(stored_clause(Fact,[]))],_).

% conj_to_list(Conj,List) provides in List the elements of
% the conjunction Conj
conj_to_list((A,B),[A|ListB]):-
	!,
	conj_to_list(B,ListB).
conj_to_list(A,[A]).	










