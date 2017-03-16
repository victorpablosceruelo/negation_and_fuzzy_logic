:- module(_,_,[assertions,nativeprops]).

:- use_module(library(messages)).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(sort)).
:- use_module(ciaopp(ciaopp)).
:- use_module(plai(fixpo_bu)).
%:- use_module(plai(plai_db),[complete/7]).
:- use_module(domain(bshare(bshare)), [bshare_crack/2]).
:- use_module(domain(share_amgu), [share_amgu_call_to_entry/8]).

test(Program,Opt):-
	set_pp_flag(fixpoint,bu),
	set_pp_flag(bshare_option,Opt),
	load_path(Program,Prog),
	module(Prog),
	analyze(bshare),
	analyze(share_amgu),
	current_fact(iterate(bshare,N1)),
	current_fact(iterate(share_amgu,N2)),
	( N1 == N2 ->
	  simple_message("Same number of iterations ~q OK",[N1])
	;
	  simple_message("#iterations bshare ~q \n #iterations share_amgu ~q ",[N1,N2])
        ),  
	findall( (ClKey,Sg,Proj,Prime),
 	         current_fact(debug_complete(SgKey,ClKey,bshare,Sg,Proj,[Prime])),
                 BSH_s_u),
	sort(BSH_s_u,BSH_s),
	findall( (ClKey,Sg,Proj,Prime),
 	         current_fact(debug_complete(SgKey,ClKey,share_amgu,Sg,Proj,[Prime])),
                 SH_s_u),
        sort(SH_s_u,SH_s),
	simple_message(" *********** Comparing results ************ "),
	%% compare results	 
        crack_completes(BSH_s,BSH0_s),
	( compare_results(SH_s,BSH0_s) -> 
  	  simple_message("OK")
        ; 
	   print_results(SH_s,BSH0_s)
        %  print_results(SH_s,BSH_s)
        ).

% compare_results(share_amgu,bshare)
compare_results([],[]).
compare_results([(_,Head,Proj0,Entry0)|As],[(_,Sg,Proj_,Entry_)|Bs]):-
	remove_empty_list(Proj_,Proj),
	remove_empty_list(Entry_,Entry),
	varset(Head,Hv),
	varset(Sg,Sv),
	Fv = [],
	share_amgu_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Proj_renamed,_),
	share_amgu_call_to_entry(Sv,Sg,Hv,Head,Fv,Entry,Entry_renamed,_),
	( Proj0 \== Proj_renamed ->
	  error_message("Projections ~q and ~q are not equivalent",[Proj0,Proj_renamed]),
	  !, fail
        ;
          ( Entry0 \== Entry_renamed -> 
  	    error_message("Entries ~q and ~q are not equivalent",[Entry0,Entry_renamed]),
	    !, fail
	  ;
	    compare_results(As,Bs)  
          )
        ).
	
% print_results(share_amgu,bshare)
print_results([],[]).
print_results([(Key,Head,Proj0,Entry0)|As],[(Key0,Sg,Proj,Entry)|Bs]):-
	Key == Key0,!,
	simple_message("~q:",[Key]),
	simple_message("share - ~q :: ~q => ~q",[Head,Proj0,Entry0]),
	simple_message("bshare- ~q :: ~q => ~q",[Sg,Proj,Entry]),
	print_results(As,Bs).
print_results([(Key,_,_,_)|_],[(Key0,_,_,_)|_]):-
	error_message("Unsorted completes ... ~q vs ~q",[Key,Key0]),
        !, fail.
print_results([],Bs):-
	simple_message("More calling patters for bshare"),
	print_results_(Bs).
print_results(As,[]):-
	simple_message("More calling patters for share_amgu"),
	print_results_(As).

print_results_([]).
print_results_([(Key,Head,Proj0,Entry0)|As]):-
	simple_message("~q:",[Key]),
	simple_message("~q :: ~q => ~q",[Head,Proj0,Entry0]),
	print_results_(As).



crack_completes([],[]).
crack_completes([(Key,Sg,Proj,Prime)|Cs],[(Key,Sg,Proj0,Prime0)|Rs]):-
	bshare_crack(Proj,Proj0),
	bshare_crack(Prime,Prime0),
	crack_completes(Cs,Rs).
	

remove_empty_list([],[]).
remove_empty_list([X|Xs],Zs):-
	X == [],!,
	remove_empty_list(Xs,Zs).
remove_empty_list([X|Xs],[X|Zs]):-!,
	remove_empty_list(Xs,Zs).

load_path(append,
          '/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tests/examples/append.pl').
load_path(qsort,
          '/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tests/examples/qsortapp.pl').
load_path(deriv,
          '/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tests/examples/deriv.pl').
load_path(fib,
          '/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tests/examples/fib.pl').
load_path(hanoi,
          '/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tests/examples/hanoiapp.pl').
load_path(grammar,
          '/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tests/examples/grammar.pl').
load_path(query,
          '/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tests/examples/query.pl').
load_path(serialize,
          '/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tests/examples/serialize.pl').
