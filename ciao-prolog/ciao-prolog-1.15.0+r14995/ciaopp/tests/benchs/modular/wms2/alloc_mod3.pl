
:- module(alloc_mod3,[allocate/0],[]).
%% :- entry(allocate,[]).
%% '$start' :- allocate.
:- use_module(library(dynamic), [assertz/1]).
:- use_module(library(write)).

:- use_module(coord).
:- use_module(job).
:- use_module(job_skills).
:- use_module(tech).
:- use_module(tech_base).


length(L,N) :- length(L,0,N).

length([], N, N).
length([_|L], N0, N) :-
	N1 is N0 + 1, length(L, N1, N).

/*  alloc_mod3.pl  
 *  allocates jobs to techs, by recurring through the list of techs, rather than the list of jobs
 *  parallel execution
 *  tours asserted into DB
 */

%max_tech(1).
%max_job(1).

max_tech(118).
max_job(250).

max_length(3).					% max of 3 jobs per phase
max_tour_length(6).				% just for rocky


% /*  go/1:
%  *  Argument: "abstime" or "runtime", depending on whether Andorra or Quintus/SICStus Prolog is run
%  *  To run in Andorra, the top-level goal is "go.", to run in prolog, top-level goal is  "go(runtime)".
%  *  top-level predicate to start the program
%  */

% go :- go(abstime).



/*  allocate/0:
 *  initialises the sets of techs with linked variables, and splits the jobs into six subsets according 
 *  to their type. With a view to parallelisation, allocation of early/am and pm/last jobs can proceed 
 *  independently, with implicit linking of the tours through variables. After these tours have been linked,
 *  allocation of long jobs and allday jobs can proceed sequentially. 
 */

allocate :-
	initialise(T1,T2,Long,Early,Last,AM,PM,AllDay),
	allocate_jobs(T1,'E',Early,ToursE,  UnallocE),  % in parallel   
	allocate_jobs(ToursE, '1', AM,   ToursAM, UnallocAM),
	allocate_jobs(T2,'L',Last, ToursL,  UnallocL),  %
	allocate_jobs(ToursL, '2', PM,   ToursPM, UnallocPM),
	unify(ToursAM,ToursPM,Tours),
	allocate_jobs(Tours,long,Long,ToursLong,UnallocLong),
	allocate_jobs(ToursLong,'5',AllDay,All,UnallocAD),
	print_tours(All,Result),		% also puts tours in DB
	print_unalloc(UnallocE,UnallocL,UnallocAM,UnallocPM,UnallocLong,UnallocAD).


/*  initialise/8:
 *  all arguments are output variables
 *  constructs the initial sets of jobs and technicians, splits up the job set according to the jobs' types
 *  and also constructs two identical lists of techs but with different end variables
 */

% :- mode initialise(-,-,-,-,-,-,-,-).

initialise(T1,T2,Long,Early,Last,AM,PM,AllDay):-
	get_started(JobSet,Techs),
	build(Techs,T1,T2),
	divide_jobs(JobSet,Long,Early,Last,AM,PM,AllDay).

/*  build/3:
 *  Arguments: an initial set of techs and two variables
 *  constructs the two lists of techs
 */

% :- mode build(+,-,-).

build([],[],[]).
build([tour(Tix,[],AM,PM)|T],[tour(Tix,End1,End1,AM,PM)| T1],[tour(Tix,End2,End2,AM,PM) | T2]) :- 
	build(T,T1,T2).


/*  unify/3:
 *  Arguments: two lists of tours ending in variables, and an output variable for the merged tours
 *  by unifying the variables, the two tours are merged without the need for copying
 */

unify([],[],[]).
unify([tour(Tix,TourE,EndE,AM,_) | R1], [tour(Tix,TourL,EndL,_,PM) | R2], [tour(Tix,TourE,_,AM,PM) | Tours]):-
	EndL = [],
	EndE = TourL,
	unify(R1,R2,Tours).

break_in(11,30) :- !, break_marker.
break_in(_,_).

break_marker.

/*  allocate_jobs/5:
 *  Arguments: a list of (partial) tours, the type of job being dealt with, the relevant subset, two variables
 *  adds up to max_length jobs to each tech's tour, returning the expanded tours and those jobs found to have been
 *  unallocatable
 */ 

% :- mode allocate_jobs(+,+,+,-,-).

allocate_jobs([],_,Unalloc,[],Unalloc) :- !.	% no more techs => remaining jobs can't be allocated
allocate_jobs(Tours,_,[],Tours,[]) :- !.	% no more jobs => done

allocate_jobs([tour(Tix,TourSoFar,End,AMTime,PMTime) | Rest], Type, [J|Js], [tour(Tix,NewTour,End,NAM,NPM) | NTours], Unalloc):-
	((Type == '5' ; Type == 'long') -> Flag = full
       ;
	  Flag = half
        ),
	space_left(Flag,TourSoFar), !, 
break_in(Tix,J),
	get_suitable_jobs([J|Js],Tix,1000-0,[]-0,Selected,RestJobs),
	add_jobs(Type,Selected,tour(Tix,TourSoFar,End,AMTime,PMTime),tour(Tix,NewTour,End,NAM,NPM),NotAlloc),
	append(NotAlloc,RestJobs,NRestJobs),
	allocate_jobs(Rest,Type,NRestJobs,NTours,Unalloc).

allocate_jobs([tour(Tix,Tour,End,AMTime,PMTime) | Rest], Type, [J|Js], [tour(Tix,Tour,End,AMTime,PMTime) | NTours], Unalloc):-
	allocate_jobs(Rest, Type,[J|Js], NTours, Unalloc).

/*  get_suitable_jobs/6:
 *  Arguments: a list of potential jobs, the tech's index, a 'threshold' Distance-job pair holding the 
 *             'worst' job still ok, an auxiliary list and the number of elements in it, two variables
 *  selects the max_length closest jobs from the job set for which the tech has the necessary skills.
 *  returns the chosen jobs and the remaining jobs
 */

get_suitable_jobs([],_,_,Jobs1-_,Jobs,[]):- reverse(Jobs1,[],Jobs). % get best job at front
	
get_suitable_jobs([J|Js],Tix, DJ, AuxL, Jobs, RestJobs):-
	check_skills(Tix,J),!,			% tech has got skills to do job J
	tech_travel(Tix,J,TTime),
	check_ttime(TTime-J,DJ,AuxL,NDJ,NewAuxL,RestJobs,Others),
	get_suitable_jobs(Js,Tix,NDJ, NewAuxL, Jobs, Others).

get_suitable_jobs([J|Js],Tix,DJ,AuxL,Jobs,[J|RestJobs]):- % tech hasn't got right skills
	get_suitable_jobs(Js,Tix,DJ,AuxL,Jobs,RestJobs).


/*  check_ttime/7:
 *  Arguments: a traveltime-job pair, the threshold distance-job pair, an auxiliary list with selected jobs, four variables
 *  If new job is nearer than currently worst one, adds it to the list of selected jobs, incorporating any now
 *  deselected job in the list of RestJobs, otherwise adds J to list of RestJobs
 */

check_ttime(TTime-J,Distance-Jix,AuxL,NDJ,NewAuxL,RestJobs,Others):-
	TTime < Distance,!,			% as distance holds worst job, no need to push it out
	max_length(Len),
	check_len(TTime-J,Distance-Jix,AuxL,Len,NDJ,NewAuxL,Worst),
	check_var(Worst,RestJobs,Others).

check_ttime(_-J,DJ,AuxL,DJ,AuxL,[J|Others],Others).


/*  check_len/7:
 *  Arguments: a traveltime-job pair, the threshold distance-job pair, a list of selected jobs, the maximum length
 *             of that list, 3 variables
 *  If there is still room in the list of selected jobs, the current one is added to it and the counter increased;
 *  the last argument remains a variable. Otherwise, the currently worst job is thrown out and returned in the
 *  last argument, and the new list and new worst job also get returned.
 */

check_len(TTJ,DJ,Aux-N,Len,DJ,NewAux-NewN,_):-
	N < Len,!,
	insert_back(Aux,TTJ,NewAux),
	NewN is N + 1.

check_len(TTJ,_,[_-Worst | T]-N,_,NewD-NewJ,NewAux-N,Worst):-
	insert_back(T,TTJ,NewAux),
	NewAux = [NewD-NewJ|_].

/*  check_var/3:
 *  Arguments: 1st arg may or may not be instantiated, 2 variables
 *  If 1st arg is uninstantiated, the list of RestJobs remains the same. Otherwise the job is added to RestJobs,
 *  and its new variable end is returned in the third argument.
 */

check_var(Worst,RestJobs,RestJobs):- var(Worst),!.
check_var(J,[J|RestJobs],RestJobs).

/*  add_jobs/5:
 *  Arguments: the job type, a list of suitable Distance-Jobs pairs, the tour-so-far, two variables
 *  in turn tries to add as many of the suggested jobs as possible to the tech's tour, returning the new
 *  tour and those suggested jobs that couldn't be allocated
 */

% :- mode add_jobs(+,+,+,-,-).

    % a) Termination: no more suggested jobs

add_jobs(_,[],Tour,Tour,[]) :- !.	
 
    % b) early jobs: only one early job per tour, and since no am jobs have been added yet, choose the first one
    %                job can't overrun into lunch time, as it takes less than 240 minutes

add_jobs('E',[D-J|Remainder], tour(Tix,TourSoFar,End,AMTime,PMTime), tour(Tix,[J|TourSoFar],End,NAM,PMTime), NotAlloc):- !,
	job(J,'E',Duration,_),
	NAM is AMTime - (D + Duration),
	remove_distance(Remainder,NotAlloc).

    % c) last jobs: only one last job per tour, the others are returned to the job set to be suggested to
    %               other techs. Again, all afternoon time is still available, so no or little overtime (travel)
    %               will be incurred.

add_jobs('L',[D-J|Remainder], tour(Tix,TourSoFar,End,AMTime,PMTime), tour(Tix,[J|TourSoFar],End,AMTime,NPM), NotAlloc):- !,
	job(J,'L',Duration,_),
	NPM is PMTime - (D + Duration),
	remove_distance(Remainder,NotAlloc).

    % d) am jobs: add as many of the jobs as will fit into available time. If one job doesn't fit, try the others,
    %             their duration might be shorter

add_jobs('1',[D-J|Rest], tour(Tix,TourSoFar,End,AMTime,PMTime), tour(Tix,NewTour,End,NAM,PMTime), NotAlloc):- !,
	job(J,'1',Duration,_),
	NextAM is AMTime - (D + Duration),
	( (NextAM > 0, space_left(half,TourSoFar))  -> 
	    add_jobs('1',Rest,tour(Tix,[J|TourSoFar],End,NextAM,PMTime), tour(Tix, NewTour, End, NAM,PMTime), NotAlloc)
	;
	 add_jobs('1',Rest,tour(Tix,TourSoFar,End,AMTime,PMTime), tour(Tix,NewTour,End,NAM,PMTime), NotA),
	 NotAlloc = [J|NotA]
        ).

    % e) pm jobs: as for am jobs

add_jobs('2',[D-J|Rest], tour(Tix,TourSoFar,End,AMTime,PMTime), tour(Tix,NewTour,End,AMTime,NPM), NotAlloc):- !,
	job(J,'2',Duration,_),
	NextPM is PMTime - (D + Duration),
	( (NextPM > 0, space_left(half,TourSoFar)) ->
	    add_jobs('2',Rest,tour(Tix,[J|TourSoFar],End,AMTime,NextPM), tour(Tix, NewTour, End, AMTime,NPM), NotAlloc)
	;
	 add_jobs('2',Rest,tour(Tix,TourSoFar,End,AMTime,PMTime), tour(Tix,NewTour,End,AMTime,NPM), NotA),
	 NotAlloc = [J|NotA]
        ).

    % f) allday jobs: AMTime can now be set to 0 as all the remaining time can be considered for allday jobs

add_jobs('5',[D-J | Rest], tour(Tix,TourSoFar,_,AM,PM), tour(Tix, NewTour, _, 0, PMNew), NotAlloc) :- !,
	job(J,'5',Duration,_),
	TimeLeft is AM + PM,
	PMNext is TimeLeft - (D + Duration),
	( (PMNext > 0 , space_left(full,TourSoFar)) ->		% rocky
	    add_jobs('5',Rest, tour(Tix,[J|TourSoFar],_,0,PMNext), tour(Tix,NewTour, _,0,PMNew), NotAlloc)
	;
	 add_jobs('5',Rest,tour(Tix,TourSoFar,_,0,TimeLeft), tour(Tix,NewTour,_,0,PMNew), NotA),
	 NotAlloc = [J|NotA]
        ).

    % g) long jobs (all remaining clauses); all jobs taking longer than 240 minutes; 
    %    only one long am or pm job can be allocated by definition. Long allday jobs are treated as normal 
    %    allday jobs, as for them it is possible to add AM and PM time, increasing their chance of getting allocated

    % g1) a long job of type early or am: takes up all morning time, so at least 240 minutes morning time 
    %     must be left, and there must be enough time left for any pm/last jobs allocated previously

add_jobs(long,[D-J | Rest], tour(Tix,TourSoFar,_,AM,PM), tour(Tix,[J|TourSoFar],_,0,PMNew), NotAlloc):-
	job(J,T,Duration,_),
	(T == '1' ; T == 'E'),
	space_left(full,TourSoFar),		% rocky
	AM >= 240,				% otherwise no point in starting a long job before lunch
	PMNew is (AM + PM) - (D + Duration),	% any remaining time is PM time per definition
	PMNew >= 0,
	(T == 'E' -> no_LE_jobs(TourSoFar,'E')	% only one job of type 'E'
        ;
	  true),!,
	remove_distance(Rest,NotAlloc).		

    % g2) a long job of type pm or last: any am jobs mustn't encroach upon pm time

add_jobs(long,[D-J | Rest], tour(Tix,TourSoFar,_,AM,PM), tour(Tix,[J|TourSoFar],_,AM,PMNew), NotAlloc):-
	job(J,T,Duration,_),
	(T == '2' ; T == 'L'),
	space_left(full,TourSoFar),		% rocky
	AM > 0,					% any morning jobs mustn't already cause overtime; if AM = 0 => negative => long 'E' job
	PM >= 240,				% otherwise no point in starting a long job before EndTime
	PMNew is PM - (D + Duration),		% can't start early even if amtime left; if this overruns, so be it...
	(T == 'L' -> no_LE_jobs(TourSoFar,'L')	% only one job of type 'L'
        ;
	  true),!,
	remove_distance(Rest,NotAlloc).

    % g3) can't allocate first job in list => try others

add_jobs(long,[_-J | Rest], Tour, TourNew, NotAlloc):- % try some other long job
	add_jobs(long, Rest, Tour, TourNew, NotA),
	NotAlloc = [J|NotA].


/*  insert_back/3:
 *  Arguments: a list of Distance-Job pairs, the pair to be added, a variable
 *  adds the new pair in its proper place, list sorted with worst job at front
 */

insert_back([],D-J,[D-J]).
insert_back([D1-J1 | Rest], D-J, New) :-
	D >= D1,!,
	New = [D-J, D1-J1 | Rest].
insert_back([D1-J1 | Rest], D-J, New):-
	D < D1, 
	New = [D1-J1 | Rest2],
	insert_back(Rest,D-J,Rest2).


/*  remove_distance/2:
 *  Arguments: a list of Distance-Jobs pairs and a variable
 *  returns just the list of jobs
 */

remove_distance([],[]).
remove_distance([_-J | Rest],[J|Rest2]):-
	remove_distance(Rest,Rest2).

/*  space_left/2: succeeds if there is room for more jobs in tour  */

space_left(Flag,Tour):-
	my_length(Tour,0,TL),
	max_tour_length(XL),
	(Flag == half -> L is XL // 2
       ;
	  L = XL
        ),
	TL < L.

/*  reverse/3:
 *  reverses a list
 */

reverse([],L,L).
reverse([H|T],L2,L3) :-
	reverse(T,[H|L2],L3).


/*  divide_jobs/7:
 *  Arguments: the initial job set and 6 variables
 *  divides the job set into the six subsets
 *  long allday jobs are treated as normal allday jobs
 *  calls find_bucket/9.
 */

% :- mode divide_jobs(+,-,-,-,-,-,-).

divide_jobs([],[],[],[],[],[],[]).
divide_jobs([Jix|Jobs],Long,Early,Last,AM,PM,AllDay):-
	job(Jix,Type,Duration,_),
	find_bucket(Type,[Jix|Jobs],Duration,Long,Early,Last,AM,PM,AllDay).

% :- mode find_bucket(+,+,+,-,-,-,-,-,-).

find_bucket(Type,[Jix|Jobs],Duration,Long,Early,Last,AM,PM,AllDay):-
	Duration > 240, 
	Type \== '5',!,
	divide_jobs(Jobs,Long1,Early,Last,AM,PM,AllDay),
	Long = [Jix | Long1].

find_bucket('E',[Jix|Jobs],Duration,Long,Early,Last,AM,PM,AllDay):-
	Duration =< 240,!,
	divide_jobs(Jobs,Long,Early1,Last,AM,PM,AllDay),
	Early = [Jix|Early1].

find_bucket('L',[Jix|Jobs],Duration,Long,Early,Last,AM,PM,AllDay):-
	Duration =< 240,!,
	divide_jobs(Jobs,Long,Early,Last1,AM,PM,AllDay),
	Last = [Jix | Last1].

find_bucket('1',[Jix|Jobs],Duration,Long,Early,Last,AM,PM,AllDay):-
	Duration =< 240,!,
	divide_jobs(Jobs,Long,Early,Last,AM1,PM,AllDay),
	AM = [Jix | AM1].

find_bucket('2',[Jix|Jobs],Duration,Long,Early,Last,AM,PM,AllDay):-
	Duration =< 240,!,
	divide_jobs(Jobs,Long,Early,Last,AM,PM1,AllDay),
	PM = [Jix | PM1].

find_bucket('5',[Jix|Jobs],_,Long,Early,Last,AM,PM,AllDay):- !,
	divide_jobs(Jobs,Long,Early,Last,AM,PM,AllDay1),
	AllDay = [Jix | AllDay1].


/*  get_started/2:
 *  Arguments: two variables
 *  Instantiates its arguments to the initial job list and the initial (empty) tour set
 */

% :- mode get_started(-,-).

get_started(Jobs,Tours) :-
	max_job(MaxJob),
	get_jobs(1,MaxJob,Jobs),
	max_tech(MaxTech),
	get_tours(1,MaxTech,Tours).


/*  get_jobs/3:
 *  Arguments: a counter, an auxiliary list and a variable
 *  Returns the initial job set
 */

get_jobs(X,X,[X]):-!.
get_jobs(Count,Max,[Count|Jobs]) :-
	NewCount is Count + 1,
	get_jobs(NewCount,Max,Jobs).


/*  get_tours/3:
 *  Arguments: a counter, an auxiliary list and a variable
 *  Returns the initial tour set
 */
	
get_tours(Count,Count,[tour(Count,[],AMTime,PMTime)]):- !,
	tech(Count,StartTime,EndTime,_,_),
	AMTime is 720 - StartTime,		% how much time left in morning
	PMTime is EndTime - 720.		% how much time left in afternoon

get_tours(Count,Max,[tour(Count,[],AMTime,PMTime) | Tours]) :-
	Count =\= Max,
	NewCount is Count + 1,
	tech(Count,StartTime,EndTime,_,_),
	AMTime is 720 - StartTime,		% how much time left in morning
	PMTime is EndTime - 720,		% how much time left in afternoon
	get_tours(NewCount,Max,Tours).


/*  tech_travel/3:
 *  Arguments: a tech, a job, a variable
 *  Returns the travel time from tech's base to job
 */

% :- mode tech_travel(+,+,-).
	
tech_travel(Tix,Jix,Time) :-
	base(Tix,X1,Y1),
	coord(Jix, X2, Y2),
	XDelta0 is X1 - X2,
	YDelta0 is Y1 - Y2,
	abs(XDelta0, XDelta),
	abs(YDelta0, YDelta),
	(XDelta > YDelta ->
	    Time is ((XDelta + (YDelta // 2)) // 8)
	;
	    Time is ((YDelta + (XDelta // 2)) // 8)
	).


abs(Number,Absolute):-
	( Number >= 0 -> Absolute = Number
    ;
	 Absolute is -Number
        ).


/*  print_tours/2:
 *  Arguments: a list of tours including timing info etc, and a variable
 *  removes the timing info from the tours, adds base and stores points, returning the result
 *  and also pretty-printing it
 */

print_tours(Tours,Result):-
	remove_times(Tours,Res1),
	extend_all(Res1,Result),
	write(Result), nl.


/*  pp/1:
 *  Argument: a list of tours
 *  pretty-prints all non-empty tours one per line
 */

pp([]) :- nl,nl.
pp([H|T]) :- 
	( H = tour(_,[]) ->
	    assertz(H),
	    pp(T)
	;
	  assertz(H),
	  format('~w~n',[H]),
	  pp(T)
        ).


/*  print_unalloc/6:
 *  Arguments: the six sets of unallocatable jobs
 *  determines how many jobs there are left altogether, then prints the six lists
 */

print_unalloc(UnallocE,UnallocL,UnallocAM,UnallocPM,UnallocLong,UnallocAD):-
	length(UnallocE,L1),
	length(UnallocL,L2),
	length(UnallocAM,L3),
	length(UnallocPM,L4),
	length(UnallocLong,L5),
	length(UnallocAD,L6),
	L is L1 + L2 + L3 + L4 + L5 + L6,
	format('~d jobs could not be allocated: ~n~w~n~w~n~w~n~w~n~w~n~w~n',
	        [L,UnallocE,UnallocL,UnallocAM,UnallocPM,UnallocLong,UnallocAD]).


/*  remove_times/2:
 *  removes the last three arguments from the tours, namely the times and the variable end
 */

remove_times([],[]).
remove_times([tour(Tix,Tour,_,_,_)|Rest],[tour(Tix,Tour)|NewRest]):-
	remove_times(Rest,NewRest).


/*  extend_all/2:
 *  Arguments: a list of tours and a variable
 *  returns the tours extended by base and stores points
 */

extend_all([],[]).
extend_all([tour(Tix,Tour) | Rest],[NewTour | Tours]):-
	( Tour == [] -> 
	    NewTour = tour(Tix,Tour)
	;
	 stores_and_start(tour(Tix,Tour),NewTour)
        ),
	 extend_all(Rest,Tours).


/*  stores_and_start/2:
 *  Arguments: a complete tour and a variable
 *  Returns the tour extended by stores and start points
 */

stores_and_start(tour(Tix,Tour),tour(Tix,NewTour)) :-
	stores_point(Tix,Stores),
	start_point(Tix,Start),
	append(Tour,[Stores,Start],NewTour).


/*  stores_point/2:
 *  Arguments: a tech and a variable
 *  Returns the index of the stores point for tech 
 */

stores_point(Tix,Stores) :-
	Stores is 250 + Tix.


/*  start_point/2:
 *  Arguments: a tech and a variable
 *  Returns the index of the start point for tech 
 */

start_point(Tix,Start) :-
	Start is 368 + Tix.


/*  no_LE_jobs/1: succeeds if there are no early or last jobs in its argument list  */

no_LE_jobs([],_).
no_LE_jobs([J|Js],Type):-
	job(J,Type,_,_),
	!,fail.
no_LE_jobs([_|Js],Type):-
	no_LE_jobs(Js,Type).

/*  my_length: as length/[2,3], but works for lists ending in a variable   */

my_length(X,L,L):- var(X),!.
my_length(X,L,L):- X == [],!.
my_length([H|T],Count,L):-
	NC is Count + 1,
	my_length(T,NC,L).


/*  check_skills/2: succeeds if technician Tix can do job J  */

check_skills(Tix,J):-
	job_skills(J,L),
	memberchk(Tix,L).			% tech has necessary skills

/*********** some stuff for Andorra ***************/

memberchk(X,[X|_]):- !.
memberchk(X,[_|L]):-
	memberchk(X,L).

append([],L,L).
append([H|T],L2,[H|L3]):-
	append(T,L2,L3).

format(String,Variables):-
	name(String,ASCII),
	look_at_string(ASCII,Variables,[]).

look_at_string([],[],Sofar) :- !,
	write_all(Sofar).

look_at_string([126, 110 | Rest], Vars, Sofar):- !,
	write_all(Sofar),nl,
	look_at_string(Rest,Vars,[]).

look_at_string([126,X|Rest],[V1|Vs],Sofar) :-
	X \== 110,
	!,
	write_all(Sofar),write(' '),
	write(V1),write(' '),
	look_at_string(Rest,Vs,[]).

look_at_string([C|Rest],Vars,Sofar):-
	C \== 126, 
	!,
	look_at_string(Rest,Vars,[C|Sofar]).

write_all([]).
write_all(List):-
	List \== [],
	reverse(List,[],String),
	name(Atom,String),
	write(Atom).



