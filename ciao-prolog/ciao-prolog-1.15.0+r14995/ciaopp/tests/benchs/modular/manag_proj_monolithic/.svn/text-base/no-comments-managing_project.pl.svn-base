:- module(managing_project,
	[available_hours/6,
	 effort/6,
	 personal_effort/7,
	 project_effort/7,
	 personal_project_effort/8,
	 monthly_plan/4,
	 cost_statement/6,
	 cost_statement_personnel/6,
	 global_cost_statement/5,
	 check_plan/0,
	 check_all_availabilities/4,
	 person_load_overview/4,
	 time_sheet/4,
	 hours_to_be_justified_person_project/7,
	 hours_to_be_justified_person/6,
	 hours_to_be_justified_project/6
	],
	 [assertions]).
:- use_module(library(format)).
:- use_module(library(lists), [append/3]).
:- use_module(library(aggregates), [findall/3]).
effort(SM,SY,EM,EY,Unit,Effort):-
	get_list_projects(L),
	all_project_efforts(L,SM,SY,EM,EY,Unit,Effort).
all_project_efforts([],_SM,_SY,_EM,_EY,_Unit,0).
all_project_efforts([Project|Projects],SM,SY,EM,EY,Unit,MMs):-
	project_effort(Project,SM,SY,EM,EY,Unit,Effort),
	all_project_efforts(Projects,SM,SY,EM,EY,Unit,MMs0),
	MMs is MMs0 + Effort.
personal_effort(Person,SM,SY,EM,EY,Unit,Effort):-
	get_list_projects(L),
	personal_all_project_effort(L,Person,SM,SY,EM,EY,Unit,Effort).
personal_all_project_effort([],_Person,_SM,_SY,_SM1,_SY1,_Unit,0).
personal_all_project_effort([P|Projects],Person,SM,SY,SM1,SY1,Unit,TP):-
	personal_project_effort(Person,P,SM,SY,SM1,SY1,Unit,Effort),
	personal_all_project_effort(Projects,Person,SM,SY,SM1,SY1,Unit,TP0),
	TP is TP0 + Effort.
project_effort(Project,SM,SY,EM,EY,Unit,Effort):-
	convert_date(Project,SM,SY,Month),
	convert_date(Project,EM,EY,Month1),
	planned(Project,Month,Month1,1,Unit,_Tasks,_Efforts,Effort).
personal_project_effort(Person,Project,SM,SY,EM,EY,Unit,Effort):-
	convert_date(Project,SM,SY,Month),
	convert_date(Project,EM,EY,Month1),
	planned(Project,Month,Month1,1,Unit,Tasks,Efforts,_),
	add_up_hours(Tasks,Efforts,Project,1,[],Assignments),
	member(person(Person,Effort,_,_),Assignments),!.
personal_project_effort(_Person,_P,_SM,_SY,_SM1,_SY1,_Unit,0).
monthly_plan(SM,SY,EM,EY):-	
	get_list_projects(Projects),
	show_project_monthly_plan(SM,SY,EM,EY,Projects).
show_project_monthly_plan(M,Y,M,Y,_Projects):-!.
show_project_monthly_plan(SM,SY,EM,EY,Projects):-	
	next_month(SM,SY,SM1,SY1),
	all_project_efforts(Projects,SM,SY,SM1,SY1,mm,MMs),
	format("~a ~d Total ~2f: ",[SM,SY,MMs]),
	show_project_effort(Projects,SM,SY,SM1,SY1),
	show_project_monthly_plan(SM1,SY1,EM,EY,Projects).
show_project_effort([],_SM,_SY,_EM,_EY):-
	nl.
show_project_effort([P|Projects],SM,SY,EM,EY):-
	project_effort(P,SM,SY,EM,EY,mm,Effort),
	format("(~a, ~2f)",[P,Effort]),
	show_project_effort(Projects,SM,SY,EM,EY).
person_load_overview(SM,SY,EM,EY):-
	get_list_people(People),
	show_person_load(People,SM,SY,EM,EY).
show_person_load([],_SM,_SY,_EM,_EY):-
	nl.
show_person_load([P|People],SM,SY,EM,EY):-
	personal_effort(P,SM,SY,EM,EY,mm,Effort),
	format("(~a, ~2f)~n",[P,Effort]),
	show_person_load(People,SM,SY,EM,EY).
available_hours(_Person,M,Y,M,Y,0):-!.
available_hours(Person,SM,SY,EM,EY,Hours):-
	committed_hours(SM,SY,Person,Teaching,Other,Holiday),!,
	max_hours_per_month(Max),
	Available0 is Max - (Teaching +Other +Holiday),
	(Available0 < 0 -> Available = 0
	;
	    Available = Available0),
	next_month(SM,SY,SM1,SY1),
	personal_effort(Person,SM,SY,SM1,SY1,hour,TP),
	Still_Available is Available -TP,
	available_hours(Person,SM1,SY1,EM,EY,Tmp_Hours),
	Hours is Tmp_Hours + Still_Available.
available_hours(Person,SM,SY,EM,EY,Hours):-
	next_month(SM,SY,SM1,SY1),
	available_hours(Person,SM1,SY1,EM,EY,Hours).
add_up_hours([],[],_,_,Assignments,Assignments).
add_up_hours([Task|Tasks],[E|Es],Project,Partner,Tmp,Assignments):-
	Task = t(WP,T,_S,_E,_N),
	responsible(Project,WP,T,Partner,Workers,Ratios),
	insert(Workers,Ratios,E,Task,Tmp,N_Tmp), 
	add_up_hours(Tasks,Es,Project,Partner,N_Tmp,Assignments).
insert([],[],_E,_T,Tmp,Tmp).
insert([W|Workers],[R|Ratios],E,T,Tmp,Final):-
	Num_Hours  is E * R,
	add_worker(Tmp,W,Num_Hours,T,N_Tmp),
	insert(Workers,Ratios,E,T,N_Tmp,Final).
add_worker([],W,Num_Hours,T,N_Tmp):-
	N_Tmp = [person(W,Num_Hours,[T],[Num_Hours])].
add_worker(Tmp,W,Num_Hours,T,N_Tmp):-
	Tmp = [person(W,Hours,Tasks,Efforts)|Persons], !,
	N_Hours is Hours + Num_Hours,
	append(Tasks,[T],N_Tasks),
	append(Efforts,[Num_Hours],N_Efforts),
	N_Tmp = [person(W,N_Hours,N_Tasks,N_Efforts)|Persons].
add_worker([P|Persons],W,Num_Hours,T,[P|N_Tmp]):-
	add_worker(Persons,W,Num_Hours,T,N_Tmp).
cost_statement(Project,SM,SY,EM,EY,Efforts):-
	findall(P,person(P),Persons),
	get_all_personal_project_efforts(Persons,Project,SM,SY,EM,EY,Efforts).
cost_statement_personnel(Project,SM,SY,EM,EY,Euros):-
	cost_statement(Project,SM,SY,EM,EY,Efforts),
	add_all_costs(Efforts,Euros).
add_all_costs([],0).
add_all_costs([(Person,Hours)|Efforts],Euros):-
	add_all_costs(Efforts,TmpEuros),
	category(Person,Cat),
	cost_per_hour(Cat,Cost_Hour),
	Euros is TmpEuros + Hours * Cost_Hour.
global_cost_statement(SM,SY,EM,EY,Efforts):-
	findall(P,person(P),Persons),
	get_all_efforts(Persons,SM,SY,EM,EY,Efforts).
get_all_efforts([],_SM,_SY,_EM,_EY,[]).
get_all_efforts([Person|Persons],SM,SY,EM,EY,Efforts):-
	personal_effort(Person,SM,SY,EM,EY,hours,Eff),
	(Eff = 0 ->
	    Efforts = More_Efforts
	;
	    Efforts = [(Person,Eff)|More_Efforts]),
	get_all_efforts(Persons,SM,SY,EM,EY,More_Efforts).
time_sheet(Person,SM,SY,Efforts):-
	committed_hours(SM,SY,Person,Teaching,Other,Holiday),!,
	Efforts = [(avail,Still_Available),(teach,Teaching),(other,Other),(vacat,Holiday)|MoreEfforts],
	max_hours_per_month(Max),
	Available0 is Max - (Teaching +Other +Holiday),
	(Available0 < 0 -> Available = 0
	;
	    Available = Available0),
	next_month(SM,SY,SM1,SY1),
	get_list_projects(Projects),
	get_all_personal_efforts(Projects,Person,SM,SY,SM1,SY1,MoreEfforts),
	personal_effort(Person,SM,SY,SM1,SY1,hour,TP),
	Still_Available is Available -TP.
get_all_personal_efforts([],_Person,_SM,_SY,_EM,_EY,[]).
get_all_personal_efforts([Project|Projects],Person,SM,SY,EM,EY,Efforts):-
	personal_project_effort(Person,Project,SM,SY,EM,EY,hours,Eff),
	(Eff = 0 ->
	    Efforts = More_Efforts
	;
	    Efforts = [(Project,Eff)|More_Efforts]),
	get_all_personal_efforts(Projects,Person,SM,SY,EM,EY,More_Efforts).
check_plan:-
	start_period(SM,SY),
	end_period(EM,EY),
	check_all_availabilities(SM,SY,EM,EY).
check_all_availabilities(SM,SY,EM,EY):-
	findall(P,person(P),Persons),
	check_each_availability(Persons,SM,SY,EM,EY).
check_each_availability([],_SM,_SY,_EM,_EY).
check_each_availability([Person|Persons],SM,SY,EM,EY):-
	check_availability(Person,SM,SY,EM,EY),
	check_each_availability(Persons,SM,SY,EM,EY).
check_availability(_Person,M,Y,M,Y):-!.
check_availability(Person,SM,SY,EM,EY):-
	next_month(SM,SY,SM1,SY1),
	available_hours(Person,SM,SY,SM1,SY1,Hours),
	(Hours >= 0 ->
	  true
	;
	    format("In ~a ~d ~a has negative availability: ~2f~n", [SM,SY,Person,Hours])),
	check_availability(Person,SM1,SY1,EM,EY).
get_all_personal_project_efforts([],_Project,_SM,_SY,_EM,_EY,[]).
get_all_personal_project_efforts([Person|Persons],Project,SM,SY,EM,EY,Efforts):-
	personal_project_effort(Person,Project,SM,SY,EM,EY,hours,Eff),
	(Eff = 0 ->
	    Efforts = More_Efforts
	;
	    Efforts = [(Person,Eff)|More_Efforts]),
	get_all_personal_project_efforts(Persons,Project,SM,SY,EM,EY,More_Efforts).
hours_to_be_justified_person_project(Person,Project,SM,SY,EM,EY,Hours):-
	received(Person,Project,SM,SY,EM,EY,Amount),
	category(Person,Cat),
	cost_per_hour(Cat,Cost_Hour),
	Hours is Amount/Cost_Hour.
hours_to_be_justified_person(Person,SM,SY,EM,EY,Hours):-
	get_list_projects(Projects),
	all_hours_to_justify(Projects,Person,SM,SY,EM,EY,Hours).
all_hours_to_justify([],_Person,_SM,_SY,_EM,_EY,[]).
all_hours_to_justify([Project|Projects],Person,SM,SY,EM,EY,[(Project,Hours)|More]):-
	hours_to_be_justified_person_project(Person,Project,SM,SY,EM,EY,Hours),
	all_hours_to_justify(Projects,Person,SM,SY,EM,EY,More).
hours_to_be_justified_project(Project,SM,SY,EM,EY,Hours):-
	get_list_people(People),
	all_hours_to_justify_project(People,Project,SM,SY,EM,EY,Hours).
all_hours_to_justify_project([],_Project,_SM,_SY,_EM,_EY,[]).
all_hours_to_justify_project([noone|People],Project,SM,SY,EM,EY,More):-
	!,
	all_hours_to_justify_project(People,Project,SM,SY,EM,EY,More).
all_hours_to_justify_project([Person|People],Project,SM,SY,EM,EY,Result):-
	hours_to_be_justified_person_project(Person,Project,SM,SY,EM,EY,Hours),
	(Hours =:= 0 ->
	    Result = More
	;
	    Result = [(Person,Hours)|More]),
	all_hours_to_justify_project(People,Project,SM,SY,EM,EY,More).
committed_hours(jan,02,german, 0,2,0).
committed_hours(feb,02,german, 0,2,0).
committed_hours(mar,02,german, 9,2,0).
committed_hours(apr,02,german,12,2,0).
committed_hours(may,02,german, 6,1,0).
committed_hours(jun,02,german, 0,1,0).
committed_hours(jul,02,german, 0,1,0).
committed_hours(aug,02,german, 0,1,80). %100
committed_hours(sep,02,german, 3,1,20). %0
committed_hours(oct,02,german,12,1,0).
committed_hours(nov,02,german,12,1,0).
committed_hours(dec,02,german, 9,1,41).
committed_hours(jan,03,german, 9,2,0).
committed_hours(feb,03,german,12,2,0).
committed_hours(mar,03,german, 8,2,0).
committed_hours(apr,03,german, 8,2,0).
committed_hours(may,03,german, 2,1,0).
committed_hours(jun,03,german, 0,1,0).
committed_hours(jul,03,german, 0,1,50).
committed_hours(aug,03,german, 0,1,50).
committed_hours(sep,03,german, 0,1,0). 
committed_hours(oct,03,german, 0,1,0).
committed_hours(nov,03,german, 0,1,0).
committed_hours(dec,03,german, 0,1,41).
committed_hours(jan,04,german, 0,0,0).
committed_hours(feb,04,german, 0,0,0).
committed_hours(mar,04,german,12,0,0).
committed_hours(apr,04,german,12,0,0).
committed_hours(may,04,german,12,0,0).
committed_hours(jun,04,german, 0,0,0).
committed_hours(jul,04,german, 0,0,0).
committed_hours(aug,04,german, 0,0,100).
committed_hours(sep,04,german, 3,0,0).
committed_hours(oct,04,german, 0,0,0).
committed_hours(nov,04,german, 0,0,0).
committed_hours(dec,04,german, 0,0,41).
committed_hours(jan,05,german, 0,0,0).
committed_hours(feb,05,german, 0,0,0).
committed_hours(mar,05,german,12,0,0).
committed_hours(apr,05,german,12,0,0).
committed_hours(may,05,german,12,0,0).
committed_hours(jun,05,german, 0,0,0).
committed_hours(jul,05,german, 0,0,0).
committed_hours(aug,05,german, 0,0,100).
committed_hours(sep,05,german, 3,0,0).
committed_hours(oct,05,german,12,0,0).
committed_hours(jan,02,herme, 0,143,0).
committed_hours(feb,02,herme, 0,143,0).
committed_hours(mar,02,herme, 0,143,0).
committed_hours(apr,02,herme, 0,143,0).
committed_hours(may,02,herme, 0,143,0).
committed_hours(jun,02,herme, 0,143,0).
committed_hours(jul,02,herme, 0,143,0).
committed_hours(aug,02,herme, 0,143,0).
committed_hours(sep,02,herme, 0,0,0).
committed_hours(oct,02,herme, 0,0,0).
committed_hours(nov,02,herme, 0,0,0).
committed_hours(dec,02,herme, 0,0,41).
committed_hours(jan,03,herme, 0,0,0).
committed_hours(feb,03,herme, 8,0,0).
committed_hours(mar,03,herme, 8,0,0).
committed_hours(apr,03,herme, 8,0,0).
committed_hours(may,03,herme, 0,0,0).
committed_hours(jun,03,herme, 0,0,0).
committed_hours(jul,03,herme, 0,0,0).
committed_hours(aug,03,herme, 0,0,100).
committed_hours(sep,03,herme, 8,20,0).  %UNM
committed_hours(oct,03,herme, 8,20,0).  %UNM
committed_hours(nov,03,herme, 8,20,0).  %UNM
committed_hours(dec,03,herme, 8,20,41). %UNM
committed_hours(jan,04,herme, 0,0,0).
committed_hours(feb,04,herme, 8,0,0).
committed_hours(mar,04,herme, 8,0,0).
committed_hours(apr,04,herme, 8,0,0).
committed_hours(may,04,herme, 0,0,0).
committed_hours(jun,04,herme, 0,0,0).
committed_hours(jul,04,herme, 0,0,0).
committed_hours(aug,04,herme, 0,0,100).
committed_hours(sep,04,herme, 8,0,0).
committed_hours(oct,04,herme, 8,0,0).
committed_hours(nov,04,herme, 8,0,0).
committed_hours(dec,04,herme, 8,0,41).
committed_hours(jan,05,herme, 0,0,0).
committed_hours(feb,05,herme, 8,0,0).
committed_hours(mar,05,herme, 8,0,0).
committed_hours(apr,05,herme, 8,0,0).
committed_hours(may,05,herme, 0,0,0).
committed_hours(jun,05,herme, 0,0,0).
committed_hours(jul,05,herme, 0,0,0).
committed_hours(aug,05,herme, 0,0,100).
committed_hours(sep,05,herme, 8,0,0).
committed_hours(oct,05,herme, 8,0,0).
committed_hours(nov,05,herme, 8,0,0).
committed_hours(dec,05,herme, 8,0,41).
committed_hours(jan,02,pedro, 0,0,0).
committed_hours(feb,02,pedro,18,0,0).
committed_hours(mar,02,pedro,24,0,0).
committed_hours(apr,02,pedro,24,0,0).
committed_hours(may,02,pedro,24,0,0).
committed_hours(jun,02,pedro, 0,0,0).
committed_hours(jul,02,pedro, 0,0,0).
committed_hours(aug,02,pedro, 0,0,100).
committed_hours(sep,02,pedro, 0,0,0).
committed_hours(oct,02,pedro, 0,0,0).
committed_hours(nov,02,pedro, 2,0,0).
committed_hours(dec,02,pedro, 0,0,41).
committed_hours(jan,03,pedro, 0,0,0).
committed_hours(feb,03,pedro,18,0,0).
committed_hours(mar,03,pedro,24,0,0).
committed_hours(apr,03,pedro,24,0,0).
committed_hours(may,03,pedro,24,0,0).
committed_hours(jun,03,pedro, 0,0,0).
committed_hours(jul,03,pedro, 0,0,0).
committed_hours(aug,03,pedro, 0,0,100).
committed_hours(sep,03,pedro, 0,0,0).
committed_hours(oct,03,pedro, 0,0,0).
committed_hours(nov,03,pedro, 0,0,0).
committed_hours(dec,03,pedro, 0,0,41).
committed_hours(jan,04,pedro, 0,0,0).
committed_hours(feb,04,pedro,18,0,0).
committed_hours(mar,04,pedro,24,0,0).
committed_hours(apr,04,pedro,24,0,0).
committed_hours(may,04,pedro,24,0,0).
committed_hours(jun,04,pedro, 0,0,0).
committed_hours(jul,04,pedro, 0,0,0).
committed_hours(aug,04,pedro, 0,0,100).
committed_hours(sep,04,pedro, 0,0,0).
committed_hours(oct,04,pedro, 0,0,0).
committed_hours(nov,04,pedro, 0,0,0).
committed_hours(dec,04,pedro, 0,0,41).
committed_hours(jan,05,pedro, 0,0,0).
committed_hours(feb,05,pedro,18,0,0).
committed_hours(mar,05,pedro,24,0,0).
committed_hours(apr,05,pedro,24,0,0).
committed_hours(may,05,pedro,24,0,0).
committed_hours(jun,05,pedro, 0,0,0).
committed_hours(jul,05,pedro, 0,0,0).
committed_hours(aug,05,pedro, 0,0,100).
committed_hours(sep,05,pedro, 0,0,0).
committed_hours(oct,05,pedro, 0,0,0).
committed_hours(nov,05,pedro, 0,0,0).
committed_hours(dec,05,pedro, 0,0,41).
committed_hours(jan,02,bueno, 0,143,0).
committed_hours(feb,02,bueno, 0,143,0).
committed_hours(mar,02,bueno, 0,143,0).
committed_hours(apr,02,bueno, 0,143,0).
committed_hours(may,02,bueno, 0,143,0).
committed_hours(jun,02,bueno, 0,143,0).
committed_hours(jul,02,bueno, 0,143,0).
committed_hours(aug,02,bueno, 0,143,100).
committed_hours(sep,02,bueno, 0,143,0).
committed_hours(oct,02,bueno, 0,113,0).
committed_hours(nov,02,bueno, 0,0,0).
committed_hours(dec,02,bueno, 0,0,41).
committed_hours(jan,03,bueno, 6,62,0).
committed_hours(feb,03,bueno,20,55,0).
committed_hours(mar,03,bueno,20,55,0).
committed_hours(apr,03,bueno,20,55,0).
committed_hours(may,03,bueno,20,55,0).
committed_hours(jun,03,bueno,20,55,0).
committed_hours(jul,03,bueno, 0,65,0).
committed_hours(aug,03,bueno, 0,65,100).
committed_hours(sep,03,bueno, 0,65,0).
committed_hours(oct,03,bueno, 0,65,0).
committed_hours(nov,03,bueno, 0,65,0).
committed_hours(dec,03,bueno, 0,65,41).
committed_hours(jan,04,bueno, 6,62,0).
committed_hours(feb,04,bueno,20,55,0).
committed_hours(mar,04,bueno,20,55,0).
committed_hours(apr,04,bueno,20,55,0).
committed_hours(may,04,bueno,20,55,0).
committed_hours(jun,04,bueno,20,55,0).
committed_hours(jul,04,bueno, 0,65,0).
committed_hours(aug,04,bueno, 0,65,100).
committed_hours(sep,04,bueno, 0,65,0).
committed_hours(oct,04,bueno, 0,65,0).
committed_hours(nov,04,bueno, 0,65,0).
committed_hours(dec,04,bueno, 0,65,41).
committed_hours(jan,05,bueno, 6,62,0).
committed_hours(feb,05,bueno,20,55,0).
committed_hours(mar,05,bueno,20,55,0).
committed_hours(apr,05,bueno,20,55,0).
committed_hours(may,05,bueno,20,55,0).
committed_hours(jun,05,bueno,20,55,0).
committed_hours(jul,05,bueno, 0,65,0).
committed_hours(aug,05,bueno, 0,65,100).
committed_hours(sep,05,bueno, 0,65,0).
committed_hours(oct,05,bueno, 0,65,0).
committed_hours(nov,05,bueno, 0,65,0).
committed_hours(dec,05,bueno, 0,65,41).
committed_hours(jan,02,boris, 11,0,0).
committed_hours(feb,02,boris, 3,0,0).
committed_hours(mar,02,boris, 0,0,0).
committed_hours(apr,02,boris, 0,0,0).
committed_hours(may,02,boris, 0,1,0).
committed_hours(jun,02,boris, 0,1,0).
committed_hours(jul,02,boris, 0,1,0).
committed_hours(aug,02,boris, 0,1,100).
committed_hours(sep,02,boris, 3,1,0).
committed_hours(oct,02,boris,24,1,0).
committed_hours(nov,02,boris,28,1,0).
committed_hours(dec,02,boris, 15,1,41).
committed_hours(jan,03,boris, 6,1,0).
committed_hours(feb,03,boris, 4,1,0).
committed_hours(mar,03,boris, 0,1,0).
committed_hours(apr,03,boris, 0,1,0).
committed_hours(may,03,boris, 0,1,0).
committed_hours(jun,03,boris, 0,1,0).
committed_hours(jul,03,boris, 0,1,0).
committed_hours(aug,03,boris, 0,1,100).
committed_hours(sep,03,boris, 3,1,0).
committed_hours(oct,03,boris, 24,1,0).
committed_hours(nov,03,boris, 28,1,0).
committed_hours(dec,03,boris, 15,1,41).
committed_hours(jan,04,boris, 6,1,0).
committed_hours(feb,04,boris, 4,1,0).
committed_hours(mar,04,boris, 0,1,0).
committed_hours(apr,04,boris, 0,1,0).
committed_hours(may,04,boris, 0,1,0).
committed_hours(jun,04,boris, 0,1,0).
committed_hours(jul,04,boris, 0,1,0).
committed_hours(aug,04,boris, 0,1,100).
committed_hours(sep,04,boris, 3,1,0).
committed_hours(oct,04,boris, 24,1,0).
committed_hours(nov,04,boris, 28,1,0).
committed_hours(dec,04,boris, 15,1,41).
committed_hours(jan,05,boris, 6,1,0).
committed_hours(feb,05,boris, 4,1,0).
committed_hours(mar,05,boris, 0,1,0).
committed_hours(apr,05,boris, 0,1,0).
committed_hours(may,05,boris, 0,1,0).
committed_hours(jun,05,boris, 0,1,0).
committed_hours(jul,05,boris, 0,1,0).
committed_hours(aug,05,boris, 0,1,100).
committed_hours(sep,05,boris, 3,1,0).
committed_hours(oct,05,boris, 24,1,0).
committed_hours(nov,05,boris, 28,1,0).
committed_hours(dec,05,boris, 15,1,41).
committed_hours(jan,02,bardo, 12,96,0).
committed_hours(feb,02,bardo, 8,96,0).
committed_hours(mar,02,bardo, 8,96,0).
committed_hours(apr,02,bardo,12,96,0).
committed_hours(may,02,bardo, 6,96,0).
committed_hours(jun,02,bardo, 0,96,0).
committed_hours(jul,02,bardo, 0,96,0).
committed_hours(aug,02,bardo, 0,96,100).
committed_hours(sep,02,bardo, 3,96,0).
committed_hours(oct,02,bardo,12,96,0).
committed_hours(nov,02,bardo,12,96,0).
committed_hours(dec,02,bardo, 8,96,31). % split Xmas holidays
committed_hours(jan,03,bardo, 9,96,10). % to avoid negative availability
committed_hours(feb,03,bardo,11,96,0).
committed_hours(mar,03,bardo,12,96,0).
committed_hours(apr,03,bardo,13,96,0).
committed_hours(may,03,bardo,12,96,0).
committed_hours(jun,03,bardo, 0,96,0).
committed_hours(jul,03,bardo, 0,96,0).
committed_hours(aug,03,bardo, 0,96,100).
committed_hours(sep,03,bardo, 0,96,0).
committed_hours(oct,03,bardo, 0,96,0).
committed_hours(nov,03,bardo, 0,96,0).
committed_hours(dec,03,bardo, 0,96,41). 
committed_hours(jan,04,bardo, 9,96,0).
committed_hours(feb,04,bardo,11,96,0).
committed_hours(mar,04,bardo,12,0,0).
committed_hours(apr,04,bardo,13,0,0).
committed_hours(may,04,bardo,12,0,0).
committed_hours(jun,04,bardo, 0,0,0).
committed_hours(jul,04,bardo, 0,0,0).
committed_hours(aug,04,bardo, 0,0,100).
committed_hours(sep,04,bardo, 0,0,0).
committed_hours(oct,04,bardo, 0,0,0).
committed_hours(nov,04,bardo, 0,0,0).
committed_hours(dec,04,bardo, 0,0,41).
committed_hours(jan,05,bardo, 9,0,0).
committed_hours(feb,05,bardo,11,0,0).
committed_hours(mar,05,bardo,12,0,0).
committed_hours(apr,05,bardo,13,0,0).
committed_hours(may,05,bardo,12,0,0).
committed_hours(jun,05,bardo, 0,0,0).
committed_hours(jul,05,bardo, 0,0,0).
committed_hours(aug,05,bardo, 0,0,100).
committed_hours(sep,05,bardo, 0,0,0).
committed_hours(oct,05,bardo, 0,0,0).
committed_hours(nov,05,bardo, 0,0,0).
committed_hours(dec,05,bardo, 0,0,41).
committed_hours(jan,02,jesus, 0,57,0).
committed_hours(feb,02,jesus, 15,57,0).
committed_hours(mar,02,jesus, 14,15,0).
committed_hours(apr,02,jesus, 21,15,0).
committed_hours(may,02,jesus, 17,1,0).
committed_hours(jun,02,jesus, 0,57,0).
committed_hours(jul,02,jesus, 12,34,0).
committed_hours(aug,02,jesus, 0,47,100).
committed_hours(sep,02,jesus, 0,1,0).
committed_hours(oct,02,jesus, 0,1,0).
committed_hours(nov,02,jesus, 0,1,0).
committed_hours(dec,02,jesus, 0,47,41).
committed_hours(jan,03,jesus, 0,1,0).
committed_hours(feb,03,jesus, 0,143,0).
committed_hours(mar,03,jesus, 0,143,0).
committed_hours(apr,03,jesus, 0,143,0).
committed_hours(may,03,jesus, 0,143,0).
committed_hours(jun,03,jesus, 0,143,0).
committed_hours(jul,03,jesus, 0,143,0).
committed_hours(aug,03,jesus, 0,143,100).
committed_hours(sep,03,jesus, 0,143,0).
committed_hours(oct,03,jesus, 0,143,0).
committed_hours(nov,03,jesus, 0,143,0).
committed_hours(dec,03,jesus, 0,143,41).
committed_hours(jan,04,jesus, 0,143,0).
committed_hours(feb,04,jesus, 0,143,0).
committed_hours(mar,04,jesus, 0,143,0).
committed_hours(apr,04,jesus, 0,143,0).
committed_hours(may,04,jesus, 0,143,0).
committed_hours(jun,04,jesus, 0,143,0).
committed_hours(jul,04,jesus, 0,143,0).
committed_hours(aug,04,jesus, 0,143,100).
committed_hours(sep,04,jesus, 0,143,0).
committed_hours(oct,04,jesus, 0,143,0).
committed_hours(nov,04,jesus, 0,143,0).
committed_hours(dec,04,jesus, 0,143,41).
committed_hours(jan,05,jesus, 0,143,0).
committed_hours(feb,05,jesus, 0,143,0).
committed_hours(mar,05,jesus, 0,143,0).
committed_hours(apr,05,jesus, 0,143,0).
committed_hours(may,05,jesus, 0,143,0).
committed_hours(jun,05,jesus, 0,143,0).
committed_hours(jul,05,jesus, 0,143,0).
committed_hours(aug,05,jesus, 0,143,100).
committed_hours(sep,05,jesus, 0,143,0).
committed_hours(oct,05,jesus, 0,143,0).
committed_hours(nov,05,jesus, 0,143,0).
committed_hours(dec,05,jesus, 0,143,41).
committed_hours(jan,02,claudio, 0,0,0).
committed_hours(feb,02,claudio, 0,0,0).
committed_hours(mar,02,claudio, 0,0,0).
committed_hours(apr,02,claudio, 0,0,0).
committed_hours(may,02,claudio, 0,0,0).
committed_hours(jun,02,claudio, 0,0,0).
committed_hours(jul,02,claudio, 0,0,0).
committed_hours(aug,02,claudio, 0,0,0).
committed_hours(sep,02,claudio, 0,0,0).
committed_hours(oct,02,claudio, 0,0,0).
committed_hours(nov,02,claudio, 0,0,0).
committed_hours(dec,02,claudio, 0,0,0).
committed_hours(jan,03,claudio, 0,0,0).
committed_hours(feb,03,claudio, 0,143,0).
committed_hours(mar,03,claudio, 0,143,0).
committed_hours(apr,03,claudio, 0,143,0).
committed_hours(may,03,claudio, 0,143,0).
committed_hours(jun,03,claudio, 0,143,0).
committed_hours(jul,03,claudio, 0,143,0).
committed_hours(aug,03,claudio, 0,143,100).
committed_hours(sep,03,claudio, 0,143,0).
committed_hours(oct,03,claudio, 0,143,0).
committed_hours(nov,03,claudio, 0,143,0).
committed_hours(dec,03,claudio, 0,143,41).
committed_hours(jan,04,claudio, 0,143,0).
committed_hours(feb,04,claudio, 0,143,0).
committed_hours(mar,04,claudio, 0,143,0).
committed_hours(apr,04,claudio, 0,143,0).
committed_hours(may,04,claudio, 0,143,0).
committed_hours(jun,04,claudio, 0,143,0).
committed_hours(jul,04,claudio, 0,143,0).
committed_hours(aug,04,claudio, 0,143,100).
committed_hours(sep,04,claudio, 0,143,0).
committed_hours(oct,04,claudio, 0,143,0).
committed_hours(nov,04,claudio, 0,143,0).
committed_hours(dec,04,claudio, 0,143,41).
committed_hours(jan,05,claudio, 0,143,0).
committed_hours(feb,05,claudio, 0,143,0).
committed_hours(mar,05,claudio, 0,143,0).
committed_hours(apr,05,claudio, 0,143,0).
committed_hours(may,05,claudio, 0,143,0).
committed_hours(jun,05,claudio, 0,143,0).
committed_hours(jul,05,claudio, 0,143,0).
committed_hours(aug,05,claudio, 0,143,100).
committed_hours(sep,05,claudio, 0,143,0).
committed_hours(oct,05,claudio, 0,143,0).
committed_hours(nov,05,claudio, 0,143,0).
committed_hours(dec,05,claudio, 0,143,41).
committed_hours(nov,02,jmanuel, 0,0,0).
committed_hours(dec,02,jmanuel, 0,0,41).
committed_hours(jan,03,jmanuel, 0,0,0).
committed_hours(feb,03,jmanuel, 0,0,0).
committed_hours(mar,03,jmanuel, 0,0,0).
committed_hours(apr,03,jmanuel, 0,0,0).
committed_hours(may,03,jmanuel, 0,0,0).
committed_hours(jun,03,jmanuel, 0,0,0).
committed_hours(jul,03,jmanuel, 0,0,50).
committed_hours(aug,03,jmanuel, 0,0,50).
committed_hours(sep,03,jmanuel, 0,0,0).
committed_hours(oct,03,jmanuel, 0,0,0).
committed_hours(nov,03,jmanuel, 0,0,0).
committed_hours(dec,03,jmanuel, 0,0,41).
committed_hours(jan,04,jmanuel, 0,0,0).
committed_hours(feb,04,jmanuel, 0,0,0).
committed_hours(mar,04,jmanuel, 0,0,0).
committed_hours(apr,04,jmanuel, 0,0,0).
committed_hours(may,04,jmanuel, 0,0,0).
committed_hours(jun,04,jmanuel, 0,0,0).
committed_hours(jul,04,jmanuel, 0,0,0).
committed_hours(aug,04,jmanuel, 0,0,100).
committed_hours(sep,04,jmanuel, 0,0,0).
committed_hours(oct,04,jmanuel, 0,0,0).
committed_hours(nov,04,jmanuel, 0,0,0).
committed_hours(dec,04,jmanuel, 0,0,41).
committed_hours(jan,05,jmanuel, 0,0,0).
committed_hours(feb,05,jmanuel, 0,0,0).
committed_hours(mar,05,jmanuel, 0,0,0).
committed_hours(apr,05,jmanuel, 0,0,0).
committed_hours(may,05,jmanuel, 0,0,0).
committed_hours(jun,05,jmanuel, 0,0,0).
committed_hours(jul,05,jmanuel, 0,0,0).
committed_hours(aug,05,jmanuel, 0,0,100).
committed_hours(sep,05,jmanuel, 0,0,0).
committed_hours(oct,05,jmanuel, 0,0,0).
committed_hours(nov,02,jfran, 0,0,0).
committed_hours(dec,02,jfran, 0,0,41).
committed_hours(jan,03,jfran, 0,0,0).
committed_hours(feb,03,jfran, 0,0,0).
committed_hours(mar,03,jfran, 0,0,0).
committed_hours(apr,03,jfran, 0,0,0).
committed_hours(may,03,jfran, 0,143,0).
committed_hours(jun,03,jfran, 0,143,0).
committed_hours(jul,03,jfran, 0,143,50).
committed_hours(aug,03,jfran, 0,143,50).
committed_hours(sep,03,jfran, 0,143,0).
committed_hours(oct,03,jfran, 0,143,0).
committed_hours(nov,03,jfran, 0,143,0).
committed_hours(dec,03,jfran, 0,143,41).
committed_hours(jan,04,jfran, 0,143,0).
committed_hours(feb,04,jfran, 0,143,0).
committed_hours(mar,04,jfran, 0,143,0).
committed_hours(apr,04,jfran, 0,143,0).
committed_hours(may,04,jfran, 0,143,0).
committed_hours(jun,04,jfran, 0,143,0).
committed_hours(jul,04,jfran, 0,143,0).
committed_hours(aug,04,jfran, 0,143,0).
committed_hours(sep,04,jfran, 0,143,0).
committed_hours(oct,04,jfran, 0,143,0).
committed_hours(nov,04,jfran, 0,143,0).
committed_hours(dec,04,jfran, 0,143,0).
committed_hours(jan,05,jfran, 0,143,0).
committed_hours(feb,05,jfran, 0,143,0).
committed_hours(mar,05,jfran, 0,143,0).
committed_hours(apr,05,jfran, 0,143,0).
committed_hours(may,05,jfran, 0,143,0).
committed_hours(jun,05,jfran, 0,143,0).
committed_hours(jul,05,jfran, 0,143,0).
committed_hours(aug,05,jfran, 0,143,0).
committed_hours(sep,05,jfran, 0,143,0).
committed_hours(oct,05,jfran, 0,143,0).
committed_hours(jan,02,noone, 0,0,143).
committed_hours(feb,02,noone, 0,0,143).
committed_hours(mar,02,noone, 0,0,143).
committed_hours(apr,02,noone, 0,0,143).
committed_hours(may,02,noone, 0,0,143).
committed_hours(jun,02,noone, 0,0,143).
committed_hours(jul,02,noone, 0,0,143).
committed_hours(aug,02,noone, 0,0,143).
committed_hours(sep,02,noone, 0,0,143).
committed_hours(oct,02,noone, 0,0,143).
committed_hours(nov,02,noone, 0,0,143).
committed_hours(dec,02,noone, 0,0,143).
committed_hours(jan,03,noone, 0,0,143).
committed_hours(feb,03,noone, 0,0,143).
committed_hours(mar,03,noone, 0,0,143).
committed_hours(apr,03,noone, 0,0,143).
committed_hours(may,03,noone, 0,0,143).
committed_hours(jun,03,noone, 0,0,143).
committed_hours(jul,03,noone, 0,0,143).
committed_hours(aug,03,noone, 0,0,143).
committed_hours(sep,03,noone, 0,0,143).
committed_hours(oct,03,noone, 0,0,143).
committed_hours(nov,03,noone, 0,0,143).
committed_hours(dec,03,noone, 0,0,143).
committed_hours(jan,04,noone, 0,0,143).
committed_hours(feb,04,noone, 0,0,143).
committed_hours(mar,04,noone, 0,0,143).
committed_hours(apr,04,noone, 0,0,143).
committed_hours(may,04,noone, 0,0,143).
committed_hours(jun,04,noone, 0,0,143).
committed_hours(jul,04,noone, 0,0,143).
committed_hours(aug,04,noone, 0,0,143).
committed_hours(sep,04,noone, 0,0,143).
committed_hours(oct,04,noone, 0,0,143).
committed_hours(nov,04,noone, 0,0,143).
committed_hours(dec,04,noone, 0,0,143).
committed_hours(jan,05,noone, 0,0,143).
committed_hours(feb,05,noone, 0,0,143).
committed_hours(mar,05,noone, 0,0,143).
committed_hours(apr,05,noone, 0,0,143).
committed_hours(may,05,noone, 0,0,143).
committed_hours(jun,05,noone, 0,0,143).
committed_hours(jul,05,noone, 0,0,143).
committed_hours(aug,05,noone, 0,0,143).
committed_hours(sep,05,noone, 0,0,143).
committed_hours(oct,05,noone, 0,0,143).
committed_hours(_,_,susana, 9,1,41):-!. %suffices for the time being
committed_hours(_,_,jorge, 9,1,41):-!. %suffices for the time being
person(german).
person(herme).
person(bueno).
person(bardo).
person(boris).
person(jesus).
person(claudio).
person(pedro).
person(jmanuel).
person(jfran).
person(astrid).
person(susana).
person(jorge).
person(noone).
get_list_people(L):-
	findall(P, person(P), L).
max_hours_per_month(143).
hours_per_month(131.25).
max_hours_per_year(1575).
next_month(jan,Y,feb,Y).
next_month(feb,Y,mar,Y).
next_month(mar,Y,apr,Y).
next_month(apr,Y,may,Y).
next_month(may,Y,jun,Y).
next_month(jun,Y,jul,Y).
next_month(jul,Y,aug,Y).
next_month(aug,Y,sep,Y).
next_month(sep,Y,oct,Y).
next_month(oct,Y,nov,Y).
next_month(nov,Y,dec,Y).
next_month(dec,Y,jan,Y1):-
	Y1 is Y + 1.
check_date_and_convert(asap,M,Y,Month):-!,
	convert_date_origin(M,Y,Date),
	Date >= 10,
	Date < 46, 
	Month is Date -10.
check_date_and_convert(colognet,M,Y,Month):-!,
	convert_date_origin(M,Y,Date),
	Date >= 0,
	Date < 36, 
	Month is Date.
check_date_and_convert(amos,M,Y,Month):-
	convert_date_origin(M,Y,Date),
	Date >= 2,
	Date < 26, 
	Month is Date -2.
convert_date(asap,M,Y,Month):-
	convert_date_origin(M,Y,Date),
	Month is Date -10.
convert_date(colognet,M,Y,Month):-
	convert_date_origin(M,Y,Month).
convert_date(amos,M,Y,Month):-
	convert_date_origin(M,Y,Date),
	Month is Date -2.
convert_date_origin(M,Y,Date):-
	month_to_number(M,M_Num),
	Date is M_Num -1 + 12*(Y-2).
month_to_number(jan,1).
month_to_number(feb,2).
month_to_number(mar,3).
month_to_number(apr,4).
month_to_number(may,5).
month_to_number(jun,6).
month_to_number(jul,7).
month_to_number(aug,8).
month_to_number(sep,9).
month_to_number(oct,10).
month_to_number(nov,11).
month_to_number(dec,12).
project(asap).
project(amos).
project(colognet).
get_list_projects(L):-
	findall(P, project(P), L).
start_period(jan,02).
end_period(nov,05).
cost_per_hour(prof,39.49).
cost_per_hour(asste_prof,23.60).
cost_per_hour(assnt_prof,24.02).
cost_per_hour(astrid,12.86).
cost_per_hour(fpi,7.50).
category(german,asste_prof).
category(herme,prof).
category(bueno,asste_prof).
category(bardo,assnt_prof).
category(boris,assnt_prof).
category(jesus,fpi).
category(claudio,fpi).
category(pedro,assnt_prof).
category(jmanuel,fpi).
category(jfran,fpi).
category(astrid,astrid).
category(susana,assnt_prof).
category(jorge,fpi).
category(noone,_):- fail.
date_less_than(_M1,Y1,_M2,Y2):-
	Y1 < Y2,!.
date_less_than(M1,Year,M2,Year):-
	M1 < M2.
date_less_or_equal(M,Y,M,Y):-!.
date_less_or_equal(M1,Y1,M2,Y2):-
	date_less_than(M1,Y1,M2,Y2).
intended_effort(asap,1,1,2,2).
intended_effort(asap,2,1,2,6).
intended_effort(asap,2,2,2,4).
intended_effort(asap,4,1,2,2).
intended_effort(asap,4,3,2,3).
intended_effort(asap,4,5,2,3).
intended_effort(asap,5,3,2,2).
intended_effort(asap,6,3,2,3).
intended_effort(asap,8,1,2,7).
intended_effort(asap,8,2,2,6).
intended_effort(asap,9,2,2,1).
intended_effort(asap,1,1,4,2).
intended_effort(asap,2,1,4,1).
intended_effort(asap,2,2,4,3).
intended_effort(asap,3,1,4,5).
intended_effort(asap,3,2,4,4).
intended_effort(asap,4,4,4,2).
intended_effort(asap,4,5,4,1).
intended_effort(asap,5,1,4,1).
intended_effort(asap,5,2,4,6).
intended_effort(asap,6,1,4,2).
intended_effort(asap,6,2,4,2).
intended_effort(asap,7,1,4,2).
intended_effort(asap,7,2,4,2).
intended_effort(asap,7,3,4,4).
intended_effort(asap,8,3,4,2).
intended_effort(asap,8,4,4,2).
intended_effort(asap,9,2,4,1).
intended_effort(asap,1,1,3,2).
intended_effort(asap,2,1,3,2).
intended_effort(asap,2,2,3,3).
intended_effort(asap,3,1,3,5).
intended_effort(asap,3,2,3,6).
intended_effort(asap,4,1,3,6).
intended_effort(asap,4,2,3,3).
intended_effort(asap,4,3,3,6).
intended_effort(asap,4,4,3,2).
intended_effort(asap,4,5,3,3).
intended_effort(asap,5,1,3,3).
intended_effort(asap,5,2,3,2).
intended_effort(asap,5,3,3,2).
intended_effort(asap,6,1,3,7).
intended_effort(asap,6,2,3,2).
intended_effort(asap,6,3,3,1).
intended_effort(asap,7,2,3,5).
intended_effort(asap,7,3,3,5).
intended_effort(asap,8,1,3,1).
intended_effort(asap,8,2,3,2).
intended_effort(asap,8,3,3,4).
intended_effort(asap,8,4,3,4).
intended_effort(asap,9,1,3,1).
intended_effort(asap,9,2,3,1).
intended_effort(asap,9,3,3,1).
intended_effort(asap,1,1,1,2).
intended_effort(asap,1,2,1,7).
intended_effort(asap,3,1,1,7). % was 4
intended_effort(asap,3,2,1,1). % was 4
intended_effort(asap,4,1,1,4).
intended_effort(asap,4,3,1,1).
intended_effort(asap,4,4,1,6).
intended_effort(asap,4,5,1,5).
intended_effort(asap,6,1,1,3.33). % was 2!
intended_effort(asap,6,2,1,6).
intended_effort(asap,6,3,1,6).
intended_effort(asap,7,1,1,5). % was 4
intended_effort(asap,7,2,1,6). % tool was 5
intended_effort(asap,7,3,1,11).
intended_effort(asap,8,1,1,4).
intended_effort(asap,8,2,1,4).
intended_effort(asap,8,4,1,2).
intended_effort(asap,9,1,1,2).
intended_effort(asap,9,2,1,2).
intended_effort(asap,9,3,1,2).
intended_effort(colognet,1,german,1,1.21904761904761904761). %160 hours german
intended_effort(colognet,1,herme,1,0.76190476190476190476). %100 hours herme
intended_effort(colognet,1,boris,1,0.60952380952380952380). % 80 hours boris
intended_effort(colognet,1,bardo,1,1.52380952380952380952). %200 hours bardo
intended_effort(colognet,1,bueno,1,0.38095238095238095238). % 50 hours bueno
intended_effort(colognet,1,claudio,1,0.26666666666666666666). % 35 hours claudio
intended_effort(colognet,2,german,1,X):- X is 210/131.25. 
intended_effort(colognet,2,herme,1,X):-  X is 215/131.25. 
intended_effort(colognet,2,boris,1,X):-  X is 40/131.25.  
intended_effort(colognet,2,bardo,1,X):- X is  100/131.25. 
intended_effort(colognet,2,bardo2,1,X):-  X is 95/131.25. 
intended_effort(colognet,2,jorge,1,X):- X is  80/131.25. 
intended_effort(colognet,2,susana,1,X):-  X is 16/131.25. 
intended_effort(colognet,3,1,1,4). % 
intended_effort(amos,1,herme,1,X):- X is 150/131.25. %Herme
intended_effort(amos,1,german,1,X):- X is 400/131.25. %German
intended_effort(amos,1,boris,1,X):- X is 990/131.25. %Boris
intended_effort(amos,1,jesus,1,X):- X is 507/131.25. %Jesus
intended_effort(amos,1,german2,1,X):- X is 30/131.25. %German
intended_effort(amos,1,boris2,1,X):- X is 200/131.25. %Boris
intended_effort(amos,1,jesus2,1,X):- X is 125/131.25. %Jesus
intended_effort(amos,2,herme,1,X):-    X is  300/131.25. %Herme
intended_effort(amos,2,german,1,X):-   X is  250/131.25. %German
intended_effort(amos,2,boris,1,X):-    X is 1200/131.25. %Boris
intended_effort(amos,2,bardo,1,X):-    X is  100/131.25. %Bardo
intended_effort(amos,2,jmanuel,1,X):-  X is  300/131.25. %JManuel
intended_effort(amos,2,herme2,1,X):-   X is  200/131.25. %Herme 
intended_effort(amos,2,german2,1,X):-  X is  100/131.25. %German
intended_effort(amos,2,boris2,1,X):-   X is  275/131.25. %Boris
intended_effort(amos,2,bardo2,1,X):-   X is   75/131.25. %Bardo
intended_effort(amos,2,jmanuel2,1,X):- X is  50/131.25. %JManuel
total_amos(X):-
	Prometidos is 21.7,
	findall(X,intended_effort(amos,2,_,_,X),List_Effort),
	subtract_list(List_Effort,Prometidos,X).
subtract_list([],P,P).
subtract_list([X|Xs],P,NP):-
	subtract_list(Xs,P,Tmp),
	NP is Tmp - X.
payment(herme,asap,nov,02,jan,04,1).
payment(herme,amos,mar,02,jan,04,1).
payment(herme,colognet,jan,02,jan,04,1).
payment(german,asap,nov,02,nov,03,1).
payment(german,amos,mar,02,jan,04,1).
payment(german,colognet,jan,02,jan,04,1).
payment(bueno,asap,nov,02,jan,04,1).
payment(bueno,omnipaper,nov,02,jan,04,1).
payment(boris,amos,mar,02,jan,04,1).
payment(pedro,asap,nov,02,feb,04,1).
payment(bardo,asap,nov,02,jan,04,1).
payment(bardo,colognet,nov,02,jan,04,1).
received(Person,Project,SM,SY,EM,EY,Amount):-
	findall(p(SM1,SY1,EM1,EY1,Total),
	    payment(Person,Project,SM1,SY1,EM1,EY1,Total),L),
	add_wages(L,SM,SY,EM,EY,Amount).
add_wages([],_,_,_,_,0).
add_wages([p(M1,Y1,_,_,_)|Ps],SM,SY,EM,EY,Amount):-
	date_less_than(EM,EY,M1,Y1),!,
	add_wages(Ps,SM,SY,EM,EY,Amount).
add_wages([p(_,_,M2,Y2,_)|Ps],SM,SY,EM,EY,Amount):-
	date_less_than(M2,Y2,SM,SY),!,
	add_wages(Ps,SM,SY,EM,EY,Amount).
add_wages([p(M1,Y1,M2,Y2,Total)|Ps],SM,SY,EM,EY,Amount):-
	add_wages(Ps,SM,SY,EM,EY,Tmp),
	convert_date_origin(M1,Y1,S),
	convert_date_origin(M2,Y2,E),
	convert_date_origin(SM,SY,Start),
	convert_date_origin(EM,EY,End),
	intersection(S,E,Start,End,Lower,Upper),
	length(Lower,Upper,Actual_Length),
	length(S,E,Full_Length),
	Amount is Tmp + Total*Actual_Length/Full_Length.
active_tasks(Project,Start,End,Tasks):-
	findall(t(WP,T,S,E,Name),task(Project,WP,T,S,E,Name),All_Tasks),
	select_active(All_Tasks,Start,End,Tasks).
select_active([],_,_,[]).
select_active([t(WP,T,S,E,Name)|Tasks],Start,End,Active_Tasks):-
	(is_active(S,E,Start,End) ->
	    Active_Tasks = [t(WP,T,S,E,Name)|More_Tasks]
	;
	    Active_Tasks = More_Tasks),
	select_active(Tasks,Start,End,More_Tasks).
planned(Project,Start,End,Partner,Unit,Tasks,Efforts,Total):-
	active_tasks(Project,Start,End,A_Tasks),
	get_intended_effort(A_Tasks,Project,Start,End,Partner,Tasks,Efforts_MM),
	sum_list(Efforts_MM,Total_MM),
	(Unit = mm ->
	    Efforts = Efforts_MM,
	    Total = Total_MM
	;
	    hours_in_a_MM(Partner,Num_Hours),
	    map_multiply(Efforts_MM,Num_Hours,Efforts),
	    Total is Total_MM * Num_Hours).
is_active(_S,E,Start,_End):-
	E =< Start, !, fail.
is_active(S,_E,_Start,End):-
	S >= End, !, fail.
is_active(_S,_E,_Start,_End).
get_intended_effort([],_Project,_Start,_End,_Partner,[],[]).
get_intended_effort([Task|Tasks],Project,Start,End,Partner,Active_Tasks,Intended_Effort):-
	Task = t(WP,T,S,E,_Name),
	intended_effort(Project,WP,T,Partner,Effort),
	Effort > 0, !,
	intersection(S,E,Start,End,Lower,Upper),
	length(Lower,Upper,Actual_Length),
	length(S,E,Full_Length),
	Estimated_Effort is Effort*Actual_Length/Full_Length,
	Active_Tasks = [Task|A_Tasks],
	Intended_Effort = [Estimated_Effort|I_Effort],
	get_intended_effort(Tasks,Project,Start,End,Partner,A_Tasks,I_Effort).
get_intended_effort([_Task|Tasks],Project,Start,End,Partner,Active_Tasks,Intended_Effort):-
	get_intended_effort(Tasks,Project,Start,End,Partner,Active_Tasks,Intended_Effort).
intersection(S,E,Start,End,Lower,Upper):-
	max(S,Start,Lower),
	min(E,End,Upper).
max(A,B,C):-
	(A > B -> C = A ; C = B).
min(A,B,C):-
	(A < B -> C = A ; C = B).
length(Start,End,Length):-
	Length is End - Start.
hours_in_a_MM(1,131.25). %1575/12
hours_in_a_MM(2,128.33). %1540/12
hours_in_a_MM(3,135.00). %1620/12
hours_in_a_MM(4,135.67). %1628/12
map_multiply([],_,[]).
map_multiply([X|Xs],Num,[NX|NXs]):-
	NX is X * Num,
	map_multiply(Xs,Num,NXs).
sum_list([],0).
sum_list([X|Xs],Sum):-
	sum_list(Xs,Tmp),
	Sum is Tmp + X.
responsible(asap,1,1,1,[german,herme],[0.8,0.2]).
responsible(asap,1,2,1,[german,herme,astrid],[0.4,0.4,0.2]).
responsible(asap,3,1,1,[german,bueno,herme],[0.2,0.4,0.4]).
responsible(asap,3,2,1,[german,pedro],[0.3,0.7]).
responsible(asap,4,1,1,[jfran,pedro],[0.75,0.25]).
responsible(asap,4,3,1,[german],[1]).
responsible(asap,4,4,1,[noone],[1]).
responsible(asap,4,5,1,[noone],[1]).
responsible(asap,6,1,1,[noone],[1]).
responsible(asap,6,2,1,[noone],[1]).
responsible(asap,6,3,1,[noone],[1]).
responsible(asap,7,1,1,[jesus,german,bueno,pedro,herme],[0.0,0.2,0.1,0.5,0.2]).
responsible(asap,7,2,1,[pedro,german,bueno,herme],[0.25,0.25,0.25,0.25]).
responsible(asap,7,3,1,[noone],[1]).
responsible(asap,8,1,1,[noone],[1]).
responsible(asap,8,2,1,[noone],[1]).
responsible(asap,8,4,1,[noone],[1]).
responsible(asap,9,1,1,[noone],[1]).
responsible(asap,9,2,1,[noone],[1]).
responsible(asap,9,3,1,[noone],[1]).
responsible(colognet,1,german,1,[german],[1]).
responsible(colognet,1,herme,1,[herme],[1]).
responsible(colognet,1,boris,1,[boris],[1]).
responsible(colognet,1,bardo,1,[bardo],[1]).
responsible(colognet,1,bueno,1,[bueno],[1]).
responsible(colognet,1,claudio,1,[claudio],[1]).
responsible(colognet,2,german,1,[german],[1]).
responsible(colognet,2,herme,1,[herme],[1]).
responsible(colognet,2,boris,1,[boris],[1]).
responsible(colognet,2,bueno,1,[bueno],[1]).
responsible(colognet,2,bardo,1,[bardo],[1]).
responsible(colognet,2,bardo2,1,[bardo],[1]).
responsible(colognet,3,1,1,[noone],[1]).
responsible(amos,1,herme,1,[herme],[1]).
responsible(amos,1,german,1,[german],[1]).
responsible(amos,1,boris,1,[boris],[1]).
responsible(amos,1,jesus,1,[jesus],[1]).
responsible(amos,1,german2,1,[german],[1]).
responsible(amos,1,boris2,1,[boris],[1]).
responsible(amos,1,jesus2,1,[jesus],[1]).
responsible(amos,2,herme,1,[herme],[1]).
responsible(amos,2,german,1,[german],[1]).
responsible(amos,2,boris,1,[boris],[1]).
responsible(amos,2,bardo,1,[bardo],[1]).
responsible(amos,2,jmanuel,1,[jmanuel],[1]).
responsible(amos,2,herme2,1,[herme ],[1]).
responsible(amos,2,german2,1,[german],[1]).
responsible(amos,2,boris2,1,[boris],[1]).
responsible(amos,2,bardo2,1,[bardo],[1]).
responsible(amos,2,jmanuel2,1,[jmanuel],[1]).
task(asap,1,1,0,36,"Internal Management").
task(asap,1,2,0,36,"Coordination Management").
task(asap,2,1,0, 6,"Requirements").
task(asap,2,2,3,12,"Case Studies Definition").
task(asap,3,1,0,6,"Analysis Domains for Specialization").
task(asap,3,2,0,6,"Off-line Specialization: precision and Efficiency").
task(asap,4,1, 6,15,"Reduzing the Size of Programs"). 
task(asap,4,2, 6,15,"Studying Platform-dependent Issues").
task(asap,4,3, 9,18,"Self-tuning Specialization System").
task(asap,4,4,15,24,"Cost Analysis for CLP").
task(asap,4,5,15,24,"Specialization at Abstract Machine Level").
task(asap,5,1,18,24,"Analysis of Process Languages").
task(asap,5,2,24,30,"Analysis and Specialization of Low Level Abstract Machines").
task(asap,5,3,21,27,"Analysis of High Level Specification Languages").
task(asap,6,1,12,18,"Safety Conditions in Pervasive Computing").
task(asap,6,2,18,24,"Combined Static and Dynamic Checking").
task(asap,6,3,24,30,"Run-time Checking in Pervasive Computing").
task(asap,7,1, 6, 9,"Specialization of Real CLP Languages").
task(asap,7,2, 6,12,"Implementation of First Prototype").
task(asap,7,3,24,32,"Final Integrated Tool").
task(asap,8,1,12,21,"Case Studies: First Cycle").
task(asap,8,2,29,35,"Case Studies: Second Cycle").
task(asap,8,3,32,35,"Evaluation").
task(asap,8,4,34,36,"Proposed Development Method").
task(asap,9,1,12,15,"Initial Web Site").
task(asap,9,2,12,36,"Dissemination of Results").
task(asap,9,3,33,36,"Final Project Web Site").
task(colognet,1,german,0,12,"German").
task(colognet,1,herme,8,12,"Herme").
task(colognet,1,boris,0,12,"Boris").
task(colognet,1,bardo,0,12,"Bardo").
task(colognet,1,bueno,0,12,"Bueno").
task(colognet,1,claudio,0,12,"Claudio").
task(colognet,2,german,12,24,"German").
task(colognet,2,herme,12,24,"Herme").
task(colognet,2,boris,12,24,"Boris").
task(colognet,2,bueno,12,24,"Bueno").
task(colognet,2,bardo,12,18,"Bardo").
task(colognet,2,bardo2,18,24,"Bardo").
task(colognet,3,1,24,36,"noone").
task(amos,1,herme,6,10,"Herme").
task(amos,1,german,0,10,"German").
task(amos,1,boris,0,10,"Boris").
task(amos,1,jesus,0,10,"Jesus").
task(amos,1,german2,10,12,"German").
task(amos,1,boris2,10,12,"Boris").
task(amos,1,jesus2,10,11,"Jesus").  % en febrero no disponible
task(amos,2,herme,12,22,"Herme").
task(amos,2,german,12,22,"German").
task(amos,2,boris,12,22,"Boris").
task(amos,2,bardo,12,22,"Bardo").
task(amos,2,jmanuel,12,22,"JManuel").
task(amos,2,herme2,22,24,"Herme ").
task(amos,2,german2,22,24,"German").
task(amos,2,boris2,22,24,"Boris").
task(amos,2,bardo2,22,24,"Bardo").
task(amos,2,jmanuel2,22,24,"JManuel").
