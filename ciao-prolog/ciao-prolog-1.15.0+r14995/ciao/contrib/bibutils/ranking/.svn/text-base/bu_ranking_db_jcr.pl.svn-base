:- module(bu_ranking_db_jcr, [init/0,

                          pub_rank/3,
 	                  rank_threshold/2,
 			  oneentry/6,

 			  known_acronyms/1,
 			  known_events/1
                          ], [assertions, fsyntax]).

:- use_module(library(bibutils(bu_databases)), [
	 reset/1,
         load_jcr_entry/1, 
         load_alias/2,

	 jcr_entry/5, 
	 jcr_total_entries/3,
	 jcr_alias/2,
	 jcr_alias/5
        ]).

:- use_module(library(bibutils(bu_options)), [get/2]).
:- use_module(library(bibutils(bu_messages))).
:- use_module(library(aggregates), [findall/3, setof/3, (^)/2]).

init:-
	bu_databases:reset(jcr_db),
	list(~(bu_options:get(jcr_db_files)),
	     bu_databases:load_jcr_entry), 
	bu_databases:load_alias(jcr, ~(bu_options:get(jcr_aliases_file))).

rank_threshold(1,"2/3"). 
rank_threshold(2,"2/3 -- 3/3"). 
%% rank_threshold(3,""). % Not really useful


pub_rank(Acronym, Type, R) :-
	oneentry(Acronym, Type, _Impact, R, _Name, _Subjects),
	R \== na,
	!.

ave_position_by_year([], _Name, []).
ave_position_by_year([Year|YT], Name, [P|PT]):-
	findall(Position/Total, (
	             bu_databases:jcr_entry(Name, Year, _Impact, Position, Subject),
	             bu_databases:jcr_total_entries(Subject, Year, Total)
	       )
	       , Ps), 
	ave(Ps, P),
	ave_position_by_year(YT, Name, PT).

oneentry(Acronym, Type, Impact, R, Name, Subjects) :- 
	(alias(Acronym, Type, Name, _, _) -> true), 
	(
	    Name = na ->
	    R = na, Impact = "", Subjects = na
	;
	        setof(Year, _Impact ^ _Position ^ _Subject ^(
		             bu_databases:jcr_entry(Name, Year, _Impact, _Position, _Subject)), Years), 
			    
	        ave_position_by_year(Years, Name, Ps),

		 findall(Impact, (member(_Year, Years), ( bu_databases:jcr_entry(Name, _Year, Impact, _Position, _Subject) -> true)),Is),
		 setof(Subject,_Year ^ _Impact^ _Position^ Subject^(
		         bu_databases:jcr_entry(Name, _Year, _Impact, _Position, Subject)
	         ),Subjects),
		 ave(Ps, R),
		 ave(Is, Impact)
	),!.
oneentry(Acronym, Type, na, na, na, []) :-
	bu_messages:warning("Venue ~w (~w) unknown in internal DB (for ~w).", 
	                    [Acronym, Type, 'JSR']).

ave([], na) :- !.
ave(Ps, R) :-
	ave_1(Ps, 0, 0, Total, N),
	R is Total/N.

ave_1([], Total, N, Total, N).
ave_1([A|Ps], TotalAccm, NAccm, Total, N) :-
    TotalAccm_New is TotalAccm+A,
    NAccm_New is NAccm+1,
    ave_1(Ps, TotalAccm_New, NAccm_New, Total, N).


known_acronyms(L) :-
	setof(Acronym-Type-Name, 
	        Impact ^ N_ ^ Name_ ^ Position^ Year ^ Subject ^(
		     bu_databases:jcr_entry(Name_, Year, Impact, Position, Subject),
		    (alias(Acronym, Type, Name_, _, N_) -> true), 
		    atom_codes(Name_, Name)
		),
                L).

known_events(L) :-
	setof(Name-jour-Name_, 
	        Impact ^ Name_ ^ Position^ Year ^ Subject ^(
		    bu_databases:jcr_entry(Name_, Year, Impact, Position, Subject),
		    atom_codes(Name_, Name)
		),
                L).

alias(venue_not_in_db, _, na, _, _).
alias(_, conf, na, _, _).

alias(Acronym1, Type1, Acronym2, Type2, _):-
	bu_databases:jcr_alias(Acronym1, Type1, Acronym2, Type2, _).
alias(Acronym, jour, Name, jour, _):-
	bu_databases:jcr_alias(Acronym, Name),
	Name \= na,
		bu_messages:warning("Use defaut alias for venue ~w (~w) (for ~w).",
	       [Acronym, jour, 'JCR']).
alias(_, _, na, _, _). 