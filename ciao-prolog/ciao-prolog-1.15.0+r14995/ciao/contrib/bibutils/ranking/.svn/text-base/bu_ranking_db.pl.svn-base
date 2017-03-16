:- module(bu_ranking_db,[init/0,

	                 pub_all_ranks/3,
		         show_ranks/1,rank_our_venues/0,
			 ranks_doctree/4,

		         current_ranking_method/1,
		         current_levels_mapping/1
%		         ranking_explanation/3 
		       ], 
		       [dcg]).

:- use_module(library(bibutils(ranking(bu_ranking_db_citeseer))),
	[init/0, pub_rank/3,oneentry/6]).
:- use_module(library(bibutils(ranking(bu_ranking_db_citeseerx))),
	[init/0, pub_rank/3,oneentry/6]).
:- use_module(library(bibutils(ranking(bu_ranking_db_core))),
	[init/0, pub_rank/3,oneentry/6, convert_core_conf_rank/2]).
:- use_module(library(bibutils(ranking(bu_ranking_db_jcr))),
	[init/0, pub_rank/3,oneentry/6]).
:- use_module(library(bibutils(ranking(bu_ranking_acronym))),
	[available_ranking_acronym/1]).

%:- use_module(library(bibutils(bu_options)), [get/2]).
:- use_module(library(bibutils(bu_messages))).
:- use_module(library(aggregates), [findall/3]).

:- use_module(library(format), [format/2]).

init:-
	bu_ranking_db_citeseer:init, 
	bu_ranking_db_citeseerx:init, 
	bu_ranking_db_core:init, 
	bu_ranking_db_jcr:init.	

max_rank_value(3).

pub_rank(Db, Acronym, Type, Rank):-
	intercept(pub_rank_(Db, Acronym, Type, Rank), 
	          ranking(Signal),
		  handle_signal(Signal)).

pub_rank_(Db, Acronym, Type, Rank) :- 
 	(var(Db); member(Db, [citeseer, citeseerx])) ->
 	(bu_ranking_db_citeseer:pub_rank(Acronym, Type, Rank1) -> true; Rank1 = 4),
 	(bu_ranking_db_citeseerx:pub_rank(Acronym, Type, Rank2) -> true; Rank2 = 4),
 	(
 	    Rank1 < Rank2 -> 
 	    Db = citeseer, Rank = Rank1
 	;
 	    Db = citeseerx, Rank = Rank2
 	), 
	Rank < 4.

pub_rank_(core, Acronym, Type, Rank) :-
    bu_ranking_db_core:pub_rank(Acronym, Type, Rank).

pub_rank_(jcr, Acronym, Type, Rank) :- 
   bu_ranking_db_jcr:pub_rank(Acronym, Type, Rank).

handle_signal(unkown_venue(Db, Acronym, Type)) :- !, 
	bu_messages:error("Venue ~w (~w) unknown in internal DB (for ~w).",
	       [Acronym, Type, Db]).
handle_signal(default_alias(Db, Acronym, Type)) :- !, 
	bu_messages:warning("Use defaut alias for venue ~w (~w) (for ~w).",
	       [Acronym, Type, Db]).

%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% ranking methods
%%%%%%%%%%%%%%%%%%

%% ave and 3 seem the most reasonable so far...
current_ranking_method(ave).
current_levels_mapping(3).


levels_mapping(1,[0.20,0.66],["20","66"]).
levels_mapping(2,[0.25,0.66],["25","66"]).
levels_mapping(3,[0.33,0.66],["33","66"]).

method_desc(max,eng," the \\textbf{maximum} ").
method_desc(max,esp," el \\textbf{m\\'{a}ximo} ").
method_desc(ave,eng," the \textbf{average} ").
method_desc(ave,esp," la \\textbf{media aritm\\'{e}tica} ").


rank_to_level(I, R,Level) :-
    levels_mapping(I,[L1,L2],_),
    ( 
      R =< L1 -> Level = 1
    ;
      R =< L2 -> Level = 2
    ;
      Level = 3
    ).


ranking_core_ranks([RA, RB, RC]):-
     convert_core_conf_rank('A', _RA),
     RA is round(_RA * 1000) /10, 
     convert_core_conf_rank('B', _RB), 
     RB is round(_RB * 1000) /10, 
     convert_core_conf_rank('C', _RC), 
     RC is round(_RC * 1000) /10.


pub_all_ranks(Acronym, Type, Rank_Cat) :-
    current_levels_mapping(I),
    current_ranking_method(M),
    pub_all_ranks_(Acronym, M, I, Type, Rank_Cat).


%%% this takes the max of all available rankings
%%%
pub_all_ranks_(Acronym, M, I, Type, Rank_Cat) :-
    M==max,
    !,
    findall(rank(RANKDB, R),
            pub_rank(RANKDB,Acronym, Type, R), 
            Ranks),
    Ranks = [_|_],
    max_rank(Ranks,MaxR),
    rank_to_level(I, MaxR,Rank_Cat).


%%% this takes the average of all available rankings
%%%
pub_all_ranks_(Acronym, M, I, Type, Rank_Cat) :-
    M==ave,
    !,
    findall(rank(RANKDB,R),
            pub_rank(RANKDB,Acronym, Type, R), 
            Ranks),
    Ranks = [_|_],
    ave_rank(Ranks,AveR),
    rank_to_level(I, AveR,Rank_Cat).


max_rank(Ranks,MaxR) :-
	member(rank(jcr,_,R),Ranks),
	!,
	MaxR = R.
max_rank(Ranks,MaxR) :-
        max_rank_value(Max_RValue),
	max_rank_1(Ranks,Max_RValue,MaxR).
max_rank_1([],MaxR,MaxR).
max_rank_1([rank(_,R)|Rs],Max,MaxR) :-
	min(R,Max,Max_1),                     % the smaller the better
 	max_rank_1(Rs,Max_1,MaxR).
min(A,B,A) :- A=<B, !.
min(_,B,B).


ave_rank(Ranks,AveR) :-
	ave_rank_1(Ranks, 0, Sum, 0, N),
	AveR is Sum/N.

ave_rank_1([], Sum, Sum, N, N).
ave_rank_1([rank(_,R)|Rs], Sum_Accm, Sum, N_Accm, N) :-
	Sum_Accm_1 is Sum_Accm + R,
	N_Accm_1 is N_Accm + 1,
	ave_rank_1(Rs, Sum_Accm_1, Sum, N_Accm_1, N).



show_ranks([]).
show_ranks([rank(DB,R)|Ranks]) :-
    format("~n~w: ~w",[DB,R]),
    show_ranks(Ranks).

    


% Useful for seeing where our conferences and journals stand
rank_our_venues :-
	format("~n Acronym ",[]),
	method_desc(M,eng,_), 
	format(" ~w ",[M]),
	levels_mapping(I,_,_), 
	format(" ~w ",[I]),
	fail.
rank_our_venues :-
	available_ranking_acronym(Acronyms),
	member(Acronym,Acronyms),
	format("~n ~w ",[Acronym]),
	method_desc(M,eng,_), 
	levels_mapping(I,_,_), 
	pub_all_ranks_(Acronym, M, I, _, Rank_Cat),
	format(" ~w ",[Rank_Cat]),
	fail.
rank_our_venues.

ranks_doctree(Acronym, Type) -->
	ranks_doctree_([jcr, core, citeseer], Acronym, Type),
	(
	    {
		findall(rank(Db, R),
		        (
			    pub_rank_(Db,Acronym, Type, R), 
			    (member(Db, [jcr, core, citeseer, citeseerx]) -> true)
			), 
			Ranks),
	    Ranks = [_|_] ,
	    ave_rank(Ranks,AveR),
	    Perc is round(AveR * 100)
	    } 
	->
	    [misc(ave_pos_rank, [[number(Perc), string_esc("%")]]), nl]
	;
	    []
	).

ranks_doctree_([jcr|T], Acronym, Type) -->
	(
	    {
	    bu_ranking_db_jcr:oneentry(Acronym, Type, Impact, R, _Name, Subjects), 
	    R \== na
	    }
	->
	    { Pos is round(100*R) },
	    [misc(jcr_rank, [number(Pos), number(Impact, 2), list(Subjects, atom)]), space]
	;	    
	    []     
	), 
	ranks_doctree_(T, Acronym, Type). 
ranks_doctree_([core|T], Acronym, Type) -->
	(
	    {
	    bu_ranking_db_core:oneentry(Acronym, Type, _Impact, Rating, _Name, _),
	    Rating \== na
	    }
	->
	    [misc(core_rank, [atom(Rating)]), space]
	;
	    []
	),
	ranks_doctree_(T, Acronym, Type). 
ranks_doctree_([citeseer|T], Acronym, Type) -->  
	(
	    {pub_rank(Db, Acronym, Type, _), Db = citeseer } 
	->
	    {
		bu_ranking_db_citeseer:oneentry(Acronym, Type, Impact, Rating, _Name, Position/Total),
		Percent is round(Rating*100) 
	    }, 
	    [misc(citeseer_rank, [number(Position), number(Total), number(Percent), number(Impact,2)]), space]
	;
	    {pub_rank(Db, Acronym, Type, _), Db = citeseerx } 
	->
	    {
		bu_ranking_db_citeseerx:oneentry(Acronym, Type, Impact, Rating, _Name, _),
		Percent is round(Rating*100)
	    },
	    [misc(citeseerx_rank, [number(Percent), number(Impact,3)]), space]
	;
	    []
	), 
	ranks_doctree_(T, Acronym, Type).
ranks_doctree_([], _Acronym, _Type) --> [].





