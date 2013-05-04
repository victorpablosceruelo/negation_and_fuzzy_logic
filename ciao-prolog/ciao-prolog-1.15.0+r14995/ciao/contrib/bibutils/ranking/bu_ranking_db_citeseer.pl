:- module(bu_ranking_db_citeseer, [init/0, 
	
	                        pub_rank/3,
  	                        rank_threshold/2,
  			        oneentry/6,
				
  			        known_acronyms/1], 
				[assertions, fsyntax]).

:- use_module(library(bibutils(bu_databases)), [
	reset/1,
	
	load_citeseer_entry/1, 
	load_alias/2,
	 
	citeseer_entry/5, 
	citeseer_total_entries/1,
	citeseer_alias/5
        ]).

:- use_module(library(bibutils(bu_options)), [get/2]).
:- use_module(library(bibutils(bu_messages))).
:- use_module(library(aggregates), [setof/3, (^)/2]).

init:-
	bu_databases:reset(citeseer_db),
	list(~(bu_options:get(citeseer_db_files)),
	     bu_databases:load_citeseer_entry), 
	bu_databases:load_alias(citeseer, ~(bu_options:get(citeseer_aliases_file))).

rank_threshold(1,"33"). 
rank_threshold(2,"50"). 
% citeseer_rank_threshold(3,"100"). % Not really useful

oneentry(venu_not_in_db, _, na, na, na, na):-!.
oneentry(Acronym, Type, Impact, Rank, Name, Info) :-
	(alias(Acronym, Type, Acronym_, Type_, Name) -> true),
	(
	    Acronym_ = na ->
	    Rank = na, Name = "", Info = na
	;    
	    bu_databases:citeseer_entry(Acronym_, Type_, Impact, Position, Name), !,
	    bu_databases:citeseer_total_entries(Total),
	    Rank is Position/Total, 
	    Info = Position/Total
	), !.
oneentry(Acronym, Type, na, na, na, na):-
	bu_messages:warning("Venue ~w (~w) unknown in internal DB (for ~w).", 
	                [Acronym, Type, 'CiteSeer']).

pub_rank(Acronym, Type, Rank) :-
	oneentry(Acronym, Type, _Impact, Rank, _Name, _Total),
	Rank \= na.
    

known_acronyms(L):-
	setof(Acronym-Type-Name, 
	        Impact ^ Acronym_ ^ Position^ (
		    bu_databases:citeseer_entry(Acronym_, Type_, Impact, Position , Name),
		    (alias(Acronym, Type, Acronym_, Type_, Name) -> true)
                ;
		    alias(Acronym, Type, na, _, _), Name =""
		),
              L).

% DEFAULT ALIAS

alias(venue_not_in_db, _, na, _, _).
alias(Acronym1, Type1, Acronym2, Type2, Name):-
	bu_databases:citeseer_alias(Acronym1, Type1, Acronym2, Type2, Name).
alias(Acronym, Type, Acronym, Type, _):-
	Acronym \= na,
	bu_messages:warning("Use defaut alias for venue ~w (~w) (for ~w).",
	       [Acronym, Type, 'CiteSeer']).


