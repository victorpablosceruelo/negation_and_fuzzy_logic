:- module(itf_db,
	[ assert_itf/5, assert_itf_chapuza/2,
	  cleanup_itf_db/0,
	  current_itf/3,
	  retract_itf/5,
	  dump_lib_itf/1,
	  load_lib_itf/1,
	  cleanup_lib_itf/0,
	  preloaded_module/2
	],
	[ assertions, isomodes ] ).

:- use_module(library(compiler(c_itf)), [module_expansion/9]).
:- use_module(program(p_abs), [get_module_from_sg/2]).
:- use_module(program(unexpand), [unexpand_meta_calls/2]).
:- use_module(api(api_base), [get_module/2, get_abs_filename/2]).

:- reexport(program(itf_base_db)).

cleanup_itf_db:-
	retractall_fact(defines(_,_,_)),
	retractall_fact(imports(_,_)),
	retractall_fact(reexported(_,_,_)),
	retractall_fact(exports(_,_)),
	retractall_fact(multifile(_,_)),
	retractall_fact(meta(_,_)),
	retractall_fact(dynamic(_)),
	retractall_fact(curr_file(_,_)),
	retractall_fact(impl_defines(_,_)).

assert_itf(defined,M,F,A,_Type):- % already expanded
	asserta_fact(defines(F,A,M)).
assert_itf(defines,M,F,A,_Type):-
	functor(Goal0,F,A),
	goal_module_expansion( Goal0 , M , Goal ),
	functor(Goal,FG,A),
	asserta_fact(defines(FG,A,M)).
assert_itf(imports,_M,F,A,ImPathAlias):-
	functor(Goal0,F,A),
	get_abs_filename(ImPathAlias , ImBase ),
	get_module( ImBase , IM ),
	% expansion in the context of IM instead of M,
	% because in M it will wrongly expand as M:F (as a head)
	% if expand as a body, problem is the same when F exists
	% also as a local predicate
	goal_module_expansion( Goal0 , IM , Goal1 ),
	unexpand_meta_calls(Goal1,Goal),
	% if there are 2 use_module, or 1 use_module and 1 reexport
	% this can happens
	( current_fact( imports( Goal , IM ) )
        -> true 
	; asserta_fact(imports(Goal,IM)) ).
assert_itf(exports,M,F,A,_M):-
	functor(Goal0,F,A),
	goal_module_expansion( Goal0 , M , Goal ),
	( current_fact( exports(Goal,M) ) 
	-> true
	;  asserta_fact(exports(Goal,M)) ).
assert_itf(new_exports,M,F,A,_M):-
	functor(Goal,F,A),
	( current_fact(exports(Goal,M))
	-> true
	;  asserta_fact(exports(Goal,M)) ).
% DTM: Special case: M and EM are the modules:
% assert_itf( reexported , module1 , pred , 3 , mod2 )
assert_itf(reexported,IM,F,A,EM):-
	functor(Goal,F,A),
%	goal_module_expansion( Goal0 , IM , Goal ),
	asserta_fact(reexported(Goal,IM,EM)).
assert_itf(multifile,M,F,A,_DynType):-
	functor(Goal0,F,A),
	goal_module_expansion( Goal0 , M , Goal ),
	asserta_fact(multifile(Goal,M)).
assert_itf(meta,M,F,A,Meta0):-
  	functor(Goal0,F,A),
	goal_module_expansion( Goal0 , M , Goal ),
  	functor(Goal,MF,_),
        Meta0 =.. [_|As],
        Meta =.. [MF|As],
  	asserta_fact(meta(Goal,Meta)).
assert_itf(dynamic,M,F,A,_Deftype):-
	functor(Goal0,F,A),
	goal_module_expansion( Goal0 , M , Goal ),
	asserta_fact(dynamic(Goal)).
assert_itf(defines_module,M,_,_,Base):-
	( current_fact(defines_module(Base,M)) ->
	  true
	; asserta_fact(defines_module(Base,M))).
assert_itf(impl_defines,M,F,A,_DynType):-
	functor(Goal0,F,A),
	goal_module_expansion( Goal0 , M , Goal ),
	asserta_fact(impl_defines(Goal,M)).

assert_itf_chapuza(remote,imports(Goal,IM)):-
	( current_fact(imports(Goal,IM)) -> true
	; asserta_fact(imports(Goal,IM)) ).




% {ERROR (p_asr): ERROR PROCESSING FACT exports(basiccontrol,\+,1,static,\+goal)
%   from ast file}


goal_module_expansion( if(A,B,C) , _basiccontrol , if(A,B,C) ) :-
	!.
goal_module_expansion( ','(A,B) , _basiccontrol , ','(A,B) ) :-
	!.
goal_module_expansion( ';'(A,B) , _basiccontrol , ';'(A,B) ) :-
	!.
goal_module_expansion( ^(A,B) , _basiccontrol , ^(A,B) ) :-
	!.
goal_module_expansion( '->'(A,B) , _basiccontrol , '->'(A,B) ) :-
	!.
goal_module_expansion( \+A , _basiccontrol , \+A ) :-
	!.
goal_module_expansion( ! , _basiccontrol , ! ) :-
	!.
goal_module_expansion( Goal , M , GoalExpanded ) :-
	c_itf:module_expansion( Goal , true , M , _Dict ,
	                        asr , _ , _ , GoalExpanded , _Body ).



:- pred retract_itf(+Class,_M0,+F,+A,_M)
	# "This predicate allows removing itf information when it 
           is no longer true. This can happen for example during 
           program transformation.". 

retract_itf(exports,M0,F,A,_M):-
	functor(Goal,F,A),
	retract_fact(exports(Goal,M0)).

current_itf(visible,Goal,X):-
	var(X),
	visible_goal(Goal).
current_itf(visible,F,A):-
	nonvar(A),
	visible_spec(F,A).
current_itf(defines,F,A):-
	current_fact(defines(F,A,_)).
current_itf(defines,F,A):-
	lib_defines(F,A,_).
current_itf(defines_pred,G,M):-
	current_fact(defines(F,A,M)),
	functor(G,F,A).
current_itf(defines_pred,G,M):-
	lib_defines(F,A,M),
	functor(G,F,A).
current_itf(imports,Goal,IM):-
	current_fact(imports(Goal,IM)).
current_itf(imports,Goal,IM):-
	lib_imports(Goal,IM).
current_itf(exports,Goal,M):-
	current_fact(exports(Goal,M)).
current_itf(exports,Goal,M):-
	lib_exports(Goal,M).
current_itf(exports,Goal,user(File)):-
	curr_file(_,user(File)),
	current_fact(defines(F,A,user(File))),
	functor(Goal,F,A).
current_itf(reexported,Goal,r(IM,EM)) :-
	current_fact(reexported(Goal,IM,EM)).
current_itf(reexported,Goal,r(IM,EM)) :-
	lib_reexported(Goal,IM,EM).
current_itf(multifile,Goal,M):-
	current_fact(multifile(Goal,M)).
current_itf(multifile,Goal,M):-
	lib_multifile(Goal,M).
current_itf(meta,Goal,Meta):-
	current_fact(meta(Goal,Meta)).
current_itf(meta,Goal,Meta):-
	lib_meta(Goal,Meta).
current_itf(dynamic,Goal,_Deftype):-
	current_fact(dynamic(Goal)).
current_itf(dynamic,Goal,_Deftype):-
	lib_dynamic(Goal).
current_itf(defines_module,M,Base):-
	current_fact(defines_module(Base,M)).
current_itf(defines_module,M,Base):-
	lib_defines_module(Base,M).
current_itf(impl_defines,Goal,M):-
	current_fact(impl_defines(Goal,M)).
current_itf(impl_defines,Goal,M):-
	lib_impl_defines(Goal,M).

visible_goal(Goal):-
	current_itf(imports,Goal,_).
visible_goal(Goal):-
	current_itf(defines,F,A),
	functor(Goal,F,A).
visible_goal(Goal):-
	current_fact(multifile(Goal,_)).

visible_spec(F,A):-
	current_itf(defines,F,A).
visible_spec(F,A):-
	current_itf(imports,Goal,_),
	functor(Goal,F,A).
visible_spec(F,A):-
	current_fact(multifile(Goal,_)),
	functor(Goal,F,A).

:- use_module(library(write), [writeq/2]).
:- data lib_defines/3, lib_imports/2, lib_exports/2, lib_multifile/2, lib_meta/2.
:- data lib_dynamic/1, lib_impl_defines/2, lib_defines_module/2, lib_reexported/3.

fake_module_name(lib_fake).

dump_lib_itf(Stream):-
	defines(A,B,C),
	\+ fake_module_name(C),
	writeq(Stream,lib_defines(A,B,C)),
	display(Stream,'.'),nl(Stream),
	fail.
dump_lib_itf(Stream):-
	imports(A,B),
	writeq(Stream,lib_imports(A,B)),
	display(Stream,'.'),nl(Stream),
	fail.
dump_lib_itf(Stream):-
	exports(A,B),
	\+ fake_module_name(B),
	writeq(Stream,lib_exports(A,B)),
	display(Stream,'.'),nl(Stream),
	fail.
dump_lib_itf(Stream):-
	reexported(A,B,C),
	writeq(Stream,lib_reexported(A,B,C)),
	display(Stream,'.'),nl(Stream),
	fail.
dump_lib_itf(Stream):-
	multifile(A,B),
	writeq(Stream,lib_multifile(A,B)),
	display(Stream,'.'),nl(Stream),
	fail.
dump_lib_itf(Stream):-
	meta(A,B),
	get_module_from_sg(A,M),
	\+ fake_module_name(M),
	writeq(Stream,lib_meta(A,B)),
	display(Stream,'.'),nl(Stream),
	fail.
dump_lib_itf(Stream):-
	dynamic(A),
	get_module_from_sg(A,M),
	\+ fake_module_name(M),
	writeq(Stream,lib_dynamic(A)),
	display(Stream,'.'),nl(Stream),
	fail.
dump_lib_itf(Stream):-
	defines_module(A,B),
	\+ fake_module_name(B),
	writeq(Stream,lib_defines_module(A,B)),
	display(Stream,'.'),nl(Stream),
	fail.
dump_lib_itf(Stream):-
	impl_defines(A,B),
	writeq(Stream,lib_impl_defines(A,B)),
	display(Stream,'.'),nl(Stream),
	fail.
dump_lib_itf(_).

%--------------------------------------------------------------------------

:- pred cleanup_lib_itf

# "Cleans up all facts of lib_* predicates.".

cleanup_lib_itf:-
	retractall_fact(lib_defines(_,_,_)),
	retractall_fact(lib_imports(_,_)),
	retractall_fact(lib_exports(_,_)),
	retractall_fact(lib_reexported(_,_,_)),
	retractall_fact(lib_multifile(_,_)),
	retractall_fact(lib_meta(_,_)),
	retractall_fact(lib_dynamic(_)),
	retractall_fact(lib_defines_module(_,_)),
	retractall_fact(lib_impl_defines(_,_)).


%--------------------------------------------------------------------------

:- pred load_lib_itf(Stream)

# "Loads the facts for lib_*/* from the stream @var{Stream}.".

:- use_module(library(read), [read/2]).

load_lib_itf(Stream):-
	repeat,
	read(Stream,Fact),
	(
	    Fact = end_of_file ->
	    true
	;
	    assertz_fact(Fact),
	    fail
	).

%--------------------------------------------------------------------------

:- pred preloaded_module(M,Base) # "Module @var{M} with basename
@var{Base} is a module already preloaded into CiaoPP.".

preloaded_module(M,Base):-
	lib_defines_module(Base,M).

