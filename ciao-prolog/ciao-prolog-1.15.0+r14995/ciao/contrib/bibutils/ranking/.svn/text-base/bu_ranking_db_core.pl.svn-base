:- module(bu_ranking_db_core,[init/0,

	                   pub_rank/3,
	                   rank_threshold/2,
			   
                           convert_core_conf_rank/2,
 
			   oneentry/6,

			   known_events/1,
			   known_acronyms/1

			   ], [assertions, fsyntax]).

:- use_module(library(bibutils(bu_databases)), [
	reset/1,

         load_core_entry/1, 
         load_alias/2,

	 core_conf_entry/3, 
	 core_jour_entry/2,
	 core_alias/2,
	 core_alias/5
        ]).

:- use_module(library(bibutils(bu_options)), [get/2]).
:- use_module(library(bibutils(bu_messages))).
:- use_module(library(aggregates), [setof/3, (^)/2]).


init:-
	bu_databases:reset(core_db),
	list(~(bu_options:get(core_db_files)),
	     bu_databases:load_core_entry), 
	bu_databases:load_alias(core, ~(bu_options:get(core_aliases_file))).



rank_threshold(1,"A / A+"). 
rank_threshold(2,"B"). 
rank_threshold(3,"C"). 

convert_core_conf_rank('A+', 0.10).
convert_core_conf_rank('A', 0.33).
convert_core_conf_rank('B', 0.64).
convert_core_conf_rank(_, 1).

% convert_core_conf_rank('A+', 0.2126).
% convert_core_conf_rank('A', 0.2126).
% convert_core_conf_rank('B', 0.4833).
% convert_core_conf_rank(_, 1).

pub_rank(Acronym, Type, R) :-
    oneentry(Acronym, Type, _Impact, Core_Rank, _Name, _),
    Core_Rank \== na,
    (
	Type = jour ->  
	( 
	    Core_Rank = 'A*' -> R=0.05
	;
	    Core_Rank = 'A'  -> R=0.20
	;
	    Core_Rank = 'B'  -> R=0.64
	;
	    R=1
	)
   ;
	Type = conf ->
        convert_core_conf_rank(Core_Rank, R)
    ),
    !.


oneentry(Acronym, Type, na, Rank, Name, na) :-
	(alias(Acronym, Type, Acronym_, Type_, Name) -> true),
	(
	    Acronym_ = na ->
	    Rank = na, Name = "" 
	;  
	     ( Type_ = jour ->  Name = Acronym_  ; Name = N), 
	    core_entry(Acronym_, Type_, Rank, N)
	), !.   
oneentry(Acronym, Type, na, na, na, na):-
	bu_messages:warning("Venue ~w (~w) unknown in internal DB (for ~w).", 
	                [Acronym, Type, 'CORE']).


known_acronyms(L):-
	setof(Acronym-Type-Name, 
	        Acronym_ ^ Rank^ (
		    core_entry(Acronym_, Type_, Rank, N),
		    ( Type_ = jour ->  atom_codes(Acronym_, Name) ; Name = N), 
		    (alias(Acronym, Type, Acronym_, Type_, Name) -> true) 
		),
                L).

known_events(L):-
	setof(Name-Type-Acronym, 
	        Rank ^ (
		    core_entry(Acronym, Type, Rank, N),
		    ( 
			Type = jour ->  
			atom_codes(Acronym, Name)
		    ;
			Name = N
		    ) 
		),
                L).



alias(venue_not_in_db, _, na, _, _).
alias(Acronym1, Type1, Acronym2, Type2, Name):-
	bu_databases:core_alias(Acronym1, Type1, Acronym2, Type2, Name).
alias(Acronym, Type, Acronym_, Type, _):-
	Acronym \= na,
	(
	    Type = conf -> 
	    Acronym = Acronym_
	;
	    Type = jour, 
	    bu_databases:core_alias(Acronym, Acronym_)
	),
	bu_messages:warning("Use defaut alias for venue ~w (~w) (for ~w).",
	       [Acronym, Type, 'CORE']).

core_entry(Acronym_, conf, Rank, Name):-
	bu_databases:core_conf_entry(Acronym_, Rank, Name).
core_entry(Acronym, jour, Rank, ""):-
	bu_databases:core_jour_entry(Acronym, Rank).



