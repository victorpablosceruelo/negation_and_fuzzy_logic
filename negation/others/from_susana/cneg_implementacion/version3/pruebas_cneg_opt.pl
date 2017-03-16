:- module(_pruebas_cneg,[impar/1,no_es_impar/1],ciaopp).

:- meta_predicate cneg(goal).

:- data stored_clause/2.

cneg(Goal) :-
        stored_clause(Goal,_Body).

start_of_file('/home/susana/tesis/misarticulos/cneg/pruebas_cneg').

impar(s(0)).

impar(s(s(_1))) :-
        impar(_1).

no_es_impar(_1) :-
        cneg(impar(_1)).

stored_clause(impar(s(s(_1))),[impar(_1)]).

stored_clause(no_es_impar(_1),[cneg(impar(_1))]).

