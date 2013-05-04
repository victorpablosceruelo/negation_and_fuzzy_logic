% Word indexer using tries
% Author: Manuel Carro
% (Modified by Jose Morales)

:- module(trie, [], []).

:- use_module(library(write)).
:- use_module(library(streams)).
:- use_module(library(iso_byte_char)).

:- set_prolog_flag(multi_arity_warnings, off).

:- include('../common').
benchmark_data(trie, 1, trie) :-
	true.
benchmark(trie, Out) :-
	trie(Out).

trie(Where) :- 
        %write('Construct a trie and search some words in it'), nl,
	Files = ['art_of_war_III.txt',
	         'art_of_war_II.txt',
		 'art_of_war_I.txt',
		 'art_of_war_IV.txt',
		 'art_of_war_IX.txt',
		 'art_of_war_VIII.txt',
		 'art_of_war_VII.txt',
		 'art_of_war_VI.txt',
		 'art_of_war_V.txt',
		 'art_of_war_XIII.txt',
		 'art_of_war_XII.txt',
		 'art_of_war_XI.txt',
		 'art_of_war_X.txt'],
	consulta('words.txt',Files,Where).
	%write(where = Where), nl.

 %% Trie:
 %% [final(const, [fichero], [trie])]
 %% [trie(const, [trie])]
 %% []
 %%
 %% Usar nodos diferentes para se~nalar el final de palabra hace las
 %% estructuras de datos algo mas peque~nas, pero hay que considerar el
 %% caso de cambiar un nodo de ser no final a ser final.


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% trie_vacio(T): T es un trie vacio. 
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trie_vacio([]).

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% trie_inserta(Trie, Palabra, Fichero, NuevoTrie): NuevoTrie
 %% contiene la informacion que habia en Trie, mas la asociacion de
 %% Palabra a Fichero.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 %% Insercion en Trie vacio
trie_inserta([], [C1,C2|List], Fich, [trie(C1, Trie)]):- !,  %% Verde
        trie_inserta([], [C2|List], Fich, Trie).
trie_inserta([], [C], Fich, [final(C, [Fich], [])]).

 %% Insercion en Trie no vacio
trie_inserta([Trie|Rest], [C|Cs], F, [NTrie|Rest]):-
        first_arg(Trie, C), !,                               %% Verde
        trie_inserta_rama([C|Cs], F, Trie, NTrie).
trie_inserta([Trie|Rest], [C1|Cs], F, [Trie|NRest]):-
        first_arg(Trie, C),
        C \== C1,
        trie_inserta(Rest, [C1|Cs], F, NRest).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% trie_inserta_rama(Palabra, Fichero, RamaTrie, NuevaRamaTrie):
 %% Palabra empieza en RamaTrie, y NuevaRamaTrie asocia esa palabra a
 %% Fichero. Hay que cambiar el nodo correspondiente de  trie/2 a
 %% final/3, si la palabra termina ahi.  Todos los cortes son verdes.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trie_inserta_rama([C, C1|CL], Fich, trie(C, TrList), trie(C, NTrList)):-!,
        trie_inserta(TrList, [C1|CL], Fich, NTrList).
trie_inserta_rama([C,C1|CL], Fich, final(C,Fs,TrList), final(C,Fs,NTrList)):-!,
        trie_inserta(TrList, [C1|CL], Fich, NTrList).
trie_inserta_rama([C], Fich, trie(C, TrList), final(C, [Fich], TrList)):-!.
trie_inserta_rama([C], Fich, final(C, Fs, TrList), final(C, NFs, TrList)):-
        inserta_ordenados(Fs, Fich, NFs).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% inserta_ordenados(ListaOrd, Item, ListaOrd2): ListaOrd2 es una
 %% lista ordenada sin repeticion que contiene los elementos en la lista
 %% ordenada sin repeticion ListaOrd m'as Item.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


inserta_ordenados([], Fich, [Fich]).
inserta_ordenados([F1|Fs], F, [F1|F1s]):- 
        F1 @< F, !,                                   %% Verde
        inserta_ordenados(Fs, F, F1s).
inserta_ordenados([F|Fs], F, [F|Fs]):- !.             %% Verde
inserta_ordenados([F1|Fs], F, [F,F1|Fs]):- F1 @> F, !.%% Verde


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Nota: puede usarse
 %%
 %%   sort([Fich|Fs], NFs)
 %%
 %% que es un predicado predefinido, en lugar de
 %%
 %%   inserta_ordenados(Fs, Fich, NFs)
 %%
 %% pero el primero es orden O(n*log n), y el segundo es O(n), con
 %% tiempo medio T(n/2)
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% trie_multi_insert(ListaPalabras, Fichero, Trie, NuevoTrie): asocia
 %% cada palabra de ListaPalabras a Fichero en Trie para dar NuevoTrie
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trie_multi_insert([], _F, T, T).
trie_multi_insert([Word|WFs], File, T1, T3):-
        trie_inserta(T1, Word, File, T2),
        trie_multi_insert(WFs, File, T2, T3).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% trie_busca(Palabra, Trie, Ficheros): Palabra esta asociada en Trie
 %% a la lista de Ficheros
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trie_busca([C], Tries, Fichs):-          %% Necesitamos un nodo final
        get_trie(C, Tries, final(C, Fichs, _OtherTries)).
trie_busca([C,C1|Cs], Tries, Fichs):-    %% Necesitamos un nodo no final
        get_trie(C, Tries, TrieAndSons),
        (TrieAndSons = trie(_, Trie); TrieAndSons = final(_, _, Trie)),
        trie_busca([C1|Cs], Trie, Fichs).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% get_trie(Car, Trie, RamaTrie): la RamaTrie de Trie es la que
 %% comienza por Car.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_trie(C, [Trie|_Tries], Trie):-
        first_arg(Trie, C).
get_trie(C1, [NoTrie|Tries], Trie):-
        first_arg(NoTrie, C),
         C1 \== C,
         get_trie(C1, Tries, Trie).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% first_arg(NodoTrie, Car): NodoTrie encabeza una palabra que
 %% empieza por Car.
 %% Nota: se podria usar el predicado predefinido arg/3, poniendo 
 %% arg(1, T, C) donde aparece first_arg(T, C).
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

first_arg(final(C, _, _), C).
first_arg(trie(C, _), C).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% indexa(Ficheros, Trie): Trie indexa las palabras en la lista de Ficheros
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

indexa(Fichs, T):-
        trie_vacio(T0),
        indexa(Fichs, T0, T).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% indexa(Ficheros, Trie, NuevoTrie): las palabras en la lista de
 %% Ficheros se a~naden a Trie para dar NuevoTrie
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

indexa([], T, T).
indexa([F|Fs], T0, T):-
        lee_fichero(F, Palabras),
        trie_multi_insert(Palabras, F, T0, T1),
        indexa(Fs, T1, T).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% lee_fichero(Fichero, Palabras): Palabras estan contenidas en Fichero
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lee_fichero(F, Palabras):-
        open(F, read, Stream),
        set_input(Stream),
        lista_palabras(Palabras),
        close(Stream).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% lista_palabras(Palabras): Palabras es la lista de palabras leidas
 %% del "stream" de entrada actual
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lista_palabras(Palabras):-
        get_alpha_or_eof_char(Char),
        lista_palabras(Char, Palabras).

lista_palabras(-1, []):- !.              %% Verde
lista_palabras(Char, [P|Ps]):-
        Char > -1,
        lee_palabra(Char, NewChar, P),
        lista_palabras(NewChar, Ps).
        

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% lee_palabra(InicioCar, FinalCar, Palabra): Palabra se lee del
 %% "stream" de entrada actual, empieza por InicioCar, y esta
 %% delimitada por FinalCar, que ya no forma parte de la palabra.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lee_palabra(Char, Char, []):- Char = -1, !.  %% Verde
lee_palabra(Char, NewChar, [AtomChar|Chars]):-
        is_alpha(Char), !,                   %% Verde
        char_code(AtomChar, Char),
        get_code(NextChar),
        lee_palabra(NextChar, NewChar, Chars).
lee_palabra(Char, NewChar, []):- 
        \+ is_alpha(Char),
        get_alpha_or_eof_char(NewChar).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% get_alpha_or_eof_char(Char): Char es el primer caracter alfabetico
 %% o de fin de fichero que aparece en el "stream"  de entrada actual
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_alpha_or_eof_char(Char):-
        repeat,
          get_code(Char),
          (is_alpha(Char); Char = -1), 
        !.


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% is_alpha(X): X es un caracter alfabetico.
 %% Nota: he incluido tambien `_' entre los alfabeticos para poder
 %% leer identificadores de programas
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_alpha(X):- X >= 0'a, X =< 0'z, !.     %% Verde
is_alpha(X):- X >= 0'A, X =< 0'Z, !.     %% Verde
is_alpha(0'_).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% consulta(Palabras, Ficheros, Donde): Donde es una lista de pares
 %% Palabra-ListaFicheros que refleja en que fichero de la lista
 %% Ficheros aparece cada palabra de la lista Palabras. Si no aparece, se
 %% asocia al atomo no_encontrada.
 %%
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


consulta(FicheroPalabras, Ficheros, Donde):-
        lee_fichero(FicheroPalabras, Palabras),
        indexa(Ficheros, Trie),
        consulta_(Palabras, Trie, Donde).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% consulta_(Palabras, Trie, Donde): en Donde aparecen cada una de
 %% las Palabras indexadas por Trie
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

consulta_([], _T, []).
consulta_([P|Ps], Trie, [P-Fichs|Ficheros]):-
        (
            trie_busca(P, Trie, Fichs) ->
            true
        ;
            Fichs = no_encontrada
        ),
        consulta_(Ps, Trie, Ficheros).

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% 
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% imprime_indice(Fs):-
%         indexa(Fs, Trie),
%         trie_busca(AtomsList, Trie, Fichs),
%         atom_codes(Atom, AtomsList),
%         write(Atom : Fichs), nl,
%         fail.
% imprime_indice(_Fs).
