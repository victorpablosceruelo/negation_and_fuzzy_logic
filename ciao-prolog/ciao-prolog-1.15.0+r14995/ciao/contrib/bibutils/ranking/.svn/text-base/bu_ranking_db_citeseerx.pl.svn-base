:- module(bu_ranking_db_citeseerx, [init/0, 
	
	                         pub_rank/3,
  	                         rank_threshold/2,
  			         oneentry/6,

				 known_acronyms/1],
			        [assertions, fsyntax]).


:- use_module(library(bibutils(bu_databases)), [
	reset/1,
	
	load_citeseerx_entry/1, 
	load_alias/2,
	
	citeseerx_entry/6, 
	citeseerx_total_entries/2,
	citeseerx_alias/5
        ]).

:- use_module(library(bibutils(bu_options)), [get/2]).
:- use_module(library(bibutils(bu_messages))).

:- use_module(library(aggregates), [findall/3, setof/3, (^)/2]).

init:-
	bu_databases:reset(citeseerx_db),
	list(~(bu_options:get(citeseerx_db_files)),
	     bu_databases:load_citeseerx_entry), 
	bu_databases:load_alias(citeseerx, ~(bu_options:get(citeseerx_aliases_file))).


rank_threshold(1,"33"). 
rank_threshold(2,"50"). 
%% threshold(3,"100"). % Not really useful

oneentry(Acronym, Type, Impact, Rank, Name, na) :-
	(alias(Acronym, Type, Acronym_, Type_, Name) -> true),
	(
	    Acronym_ = na ->
	    Rank = na, Name = "" 
	;
	    findall(Position/Total, (
	        bu_databases:citeseerx_entry(Acronym_, Type_, Year, _, Position, Name), 
	        bu_databases:citeseerx_total_entries(Year, Total) ),
                Ps),
	    findall(I,  citeseerx_entry(Acronym_, Type_, Year, I, _, Name), Is),
	    bu_databases:citeseerx_entry(Acronym_, Type_, _, _, _, Name),
	    ave(Ps, Rank), 
	    ave(Is, Impact)
	), !.
oneentry(Acronym, Type, na, na, na, na) :-
	bu_messages:warning("Venue ~w (~w) unknown in internal DB (for ~w).", 
	                    [Acronym, Type, 'CiteSeerX']).

pub_rank(Acronym, Type, Rank) :-
	oneentry(Acronym, Type, _Impact, Rank, _Name, _Info), 
	Rank \== na,
	!.
	
known_acronyms(L) :-
	setof(Acronym-Type-Name, 
	        Impact ^ Acronym_ ^ Position^ Year ^(
		    bu_databases:citeseerx_entry(Acronym_, Type_, Year, Impact, Position , Name),
		    (bu_databases:citeseerx_alias(Acronym, Type, Acronym_, Type_, Name) -> true) 
                ;
		    bu_databases:citeseerx_alias(Acronym, Type, na, _, _), Name =""
		),
                L).

conversion([], []).
conversion([H|T], [H2|T2]):-
	H2 is exp(H) - 1, 
	conversion(T, T2).

ave_i(L1, R):-
	conversion(L1, L2), 
	ave(L2, R1), 
	R is log(R1 +1 ).


ave([], na) :- !.
ave(Ps, R) :-
	ave_1(Ps, 0, 0, Total, N),
	R is Total/N.

ave_1([], Total, N, Total, N).
ave_1([A|Ps], TotalAccm, NAccm, Total, N) :-
    TotalAccm_New is TotalAccm+A,
    NAccm_New is NAccm+1,
    ave_1(Ps, TotalAccm_New, NAccm_New, Total, N).


alias(venue_not_in_db, _, na, _, _).
alias(Acronym1, Type1, Acronym2, Type2, Name):-
	bu_databases:citeseerx_alias(Acronym1, Type1, Acronym2, Type2, Name).
alias(Acronym, Type, Acronym, Type, _):-
	Acronym \= na,
	bu_messages:warning("Use defaut alias for venue ~w (~w) (for ~w).",
	       [Acronym, Type, 'CiteSeerX']).
