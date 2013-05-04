:- module(gen_expre,
	[
	    gen_expresion/6, 
	    actual_time_function_parameters/7, 
	    substitute_sizevars_using_argposdict/3,  
	    decide_type_gen_eval_expresion/4, 
	    gen_eval_expresion_using_argposdict/4, 
	    insert_dyn_size_info/3 
	],
	[assertions]). 

:- doc(author,"Pedro L@'{o}pez").  

:- use_module(infercost(algebraic(normal_form_basic)), [variable/1]).

:- use_module(infercost(top(utility)), [compound/1]). 

:- use_module(need_size_table, [find_size/3]). 

:- use_module(name_basic, [create_func_name/5]). 

:- use_module(gen_c_expre, [trans_c/7]). 
:- use_module(write_c_function, [write_c_function/6]). 

:- use_module(dynamic_size, [generate_size_key/4]). 

:- use_module(g_utility, 
	[
	    number_of_items/2, 
	    insert_in_open_list/2
	]). 


gen_expresion(_,bot,FNum,FNum,_,bot). 
gen_expresion(_,inf,FNum,FNum,_,inf).
gen_expresion(_,Size,FNum,FNum,_,num(Size)):- number(Size).
gen_expresion(_,Size,FNum,FNum,_,vari(Size)):- variable(Size).
gen_expresion(ST,Size,FNum,NFNum,FN,NSize):-
        (evaluable(Size)-> 
	 (NSize= is_exp(Size),NFNum=FNum)
	;
	    (gen_c_expresion(ST,Size,FNum,NFNum,FN,CSize), 
             NSize= CSize)
	).


gen_c_expresion(ST,Size,FNum,NFNum,FN,CSize) :-
        trans_c(Size,AV,IT,0,IL,TV,ESize),
        number_of_items(AV,Arity),
        NArity is Arity + 1,
	create_func_name(ST,FNum,NArity,NFNum,FunName),
        insert_in_open_list(FN,FunName/NArity), 
        CSize = c_func(FunName,AV),
        write_c_function(FunName,AV,IT,TV,IL,ESize).

/*
gen_eval_expresion(bot,_,_,bot).
gen_eval_expresion(inf,_,_,inf).
gen_eval_expresion(num(Size),_,_,Size).
gen_eval_expresion(vari(Size),HeadVars,_,SizeArg):- 
        substitute_var(Size,HeadVars,SizeArg).
gen_eval_expresion(c_func(FuncName,Vars),HeadVars,Exp,SizeArg):-
	create_exp_size(FuncName,HeadVars,Vars,Exp,SizeArg).
gen_eval_expresion(is_exp(Size),HeadVars,Exp,SizeArg):-
	gen_prolog_expre(Size,HeadVars,Exp,SizeArg).
*/

 %% decide_type_gen_eval_expresion(+Size,+HeadVars,-Exp,-SizeArg,-ArgPosDict)
 %% Size: a size expression.
 %% Exp: expression that compute a particular size. It has logic variables
 %%      instead of argument positions.
 %% SizeArg: logic variable that will be bound to the computed size once
 %%          Exp is evaluated. 
 %% ArgPosDict: argument position dictionary.

decide_type_gen_eval_expresion(bot, _, bot, _ArgPosDict):-
	!.
decide_type_gen_eval_expresion(inf, _, inf, _ArgPosDict):-
	!.
decide_type_gen_eval_expresion(num(Size), _, Size, _ArgPosDict):-
	!.
decide_type_gen_eval_expresion(vari(Size), _, SizeArg, ArgPosDict):-
	!, 
	decide_type_substitute_vars(Size, SizeArg, ArgPosDict).
decide_type_gen_eval_expresion(c_func(FuncName,Vars),Exp,SizeArg,_ArgPosDict):-
        !,
	create_exp_size(FuncName, _HeadVars, Vars, Exp, SizeArg).
decide_type_gen_eval_expresion(is_exp(Size), Exp, SizeArg, ArgPosDict):-
	!,
	decide_type_gen_prolog_expre(Size, Exp, SizeArg, ArgPosDict).
decide_type_gen_eval_expresion(Size, Exp, SizeArg, ArgPosDict):-
	decide_type_substitute_vars(Size, NSize, ArgPosDict),
	(var(NSize) -> SizeArg = NSize
	;  Exp = (SizeArg is NSize)
	).


%
%  creates a literal "Exp" that computes the size of an input argument.
%  "SizeArg" is the variable bound to the size calculated once the predicate 
%  successes.
%
create_exp_size(Name,HeadVars,Vars,Exp,SizeArg):-
        number_of_items(Vars,VarNum),
        Arity is VarNum + 1, 
	functor(Exp,Name,Arity),
        arg(Arity,Exp,SizeArg), % The last argument is the result
        put_args(Exp,1,HeadVars,Vars).

 
%
% Put actual argument sizes ,e.d. actual clause's size variables that
% correspond to the formal sizes in AV.
%
put_args(_,_,_,AV):-
	var(AV).
put_args(Exp,ArNum,HeadVars,AV):-
	nonvar(AV),
	AV = [A|L],
	find_var(HeadVars,A,Var),
	arg(ArNum,Exp,Var),
	NArNum is ArNum + 1,
	put_args(Exp,NArNum,HeadVars,L).


find_var([],_,_):-fail.
find_var([(Var ,Value)|List],VS, OVal):-
	(VS == Var -> 
	 OVal = Value
	; find_var(List,VS,OVal)
	).

%% 
%% find_var([],_,_).
%% 
%% find_var([(V ,Var)|_],VS,Var):-
%% 	VS == V,!. 
%% 
%% find_var([(V ,Var)|List],VS,Var):- 
%% 	VS \== V,
%% 	find_var(List,VS,Var).
%% 

/*
substitute_var(Size,HeadVars,SizeArg):-
	head_var_number(Size,HeadNum),
        find_head_var(HeadVars,HeadNum,SizeArg).

head_var_number($(N),N) :- number(N).
head_var_number($(M,N),N) :-
	number(M),
	number(N),
	M=:=0 . 


find_head_var([],_,_).
find_head_var([(ArgNum,HeadVar)|_],SArgNum,HeadVar):-
	SArgNum == ArgNum,
	!. 
find_head_var([(ArgNum,_)|VarList],SArgNum,HeadVar):-
	SArgNum \== ArgNum, 
	find_head_var(VarList,SArgNum,HeadVar).
*/

% Dynamic size transformation.
decide_type_gen_prolog_expre(Size, Exp, SizeArg, ArgPosDict):-
	decide_type_substitute_vars(Size, NSize, ArgPosDict),
	Exp = ( SizeArg is NSize ).

/*
% Simple transformation.
gen_prolog_expre(Size, HeadVars, Exp, SizeArg):-
	substitute_vars(Size, HeadVars, NSize),
	Exp = ( SizeArg is NSize ).
*/

decide_type_substitute_vars(X, X, _ArgPosDict) :-
	number(X),
        !.
decide_type_substitute_vars($(N), Y, ArgPosDict):-
	integer(N),
        !,
        insert_dyn_size_info($(0, N), ArgPosDict, Y).
decide_type_substitute_vars($(0, N), Y, ArgPosDict):-
        number(N),
        !,
        insert_dyn_size_info($(0, N), ArgPosDict, Y).
decide_type_substitute_vars($(N, M), Y, ArgPosDict):- 
        number(N),
        N>0,
        number(M),
        !,
        insert_dyn_size_info($(N,M), ArgPosDict, Y).
decide_type_substitute_vars(X, Y, ArgPosDict):-
	functor(X,F,N),
        F \== $,
        N>0,
        !,
	functor(Y,F,N),
	decide_type_function_substitute_vars(N, X, Y, ArgPosDict).
decide_type_substitute_vars(X, X, _ArgPosDict).


decide_type_function_substitute_vars(0, _, _, _ArgPosDict).
decide_type_function_substitute_vars(N, X, Y, ArgPosDict) :-
	N > 0,
	arg(N, X, Arg),
	decide_type_substitute_vars(Arg, NArg, ArgPosDict),
	arg(N, Y, NArg),
	N1 is N-1,
	decide_type_function_substitute_vars(N1, X, Y, ArgPosDict).


% Insert an item in the argument position dictionary.
% the argument position dictionary is an open list with the format:
% [($(LitNum1, ArgNum1), Var1), ..., ($(LitNumN, ArgNumN), VarN)|_] 

insert_dyn_size_info(SizeVar, ArgPosDict, Var):-
	var(ArgPosDict),
	!,
	ArgPosDict = [(SizeVar, Var)|_].
insert_dyn_size_info(SizeVar, ArgPosDict, Var):-
	nonvar(ArgPosDict),
	ArgPosDict = [(SizeVar1, Var1)|_],
	SizeVar == SizeVar1,
	!,
	Var = Var1.
insert_dyn_size_info(SizeVar, ArgPosDict, Var):-
	nonvar(ArgPosDict),
	ArgPosDict = [(SizeVar1, _Var1)|Rest],
	SizeVar \== SizeVar1,
	insert_dyn_size_info(SizeVar, Rest, Var).

/*
substitute_vars(X,_,X) :-
	number(X),!.
substitute_vars($(N),HV,Y):-
	number(N),!,find_head_var(HV,N,Y).
substitute_vars($(0,M),HV,Y):- number(M),!,find_head_var(HV,M,Y).
substitute_vars(X,HV,Y):-
	functor(X,F,N),
        F \== $,N>0,!,
	functor(Y,F,N),
	function_substitute_vars(N,X,HV,Y).
substitute_vars(X,_,X).

function_substitute_vars(0,_,_,_).
function_substitute_vars(N,X,HV,Y) :-
	N > 0,
	arg(N,X,Arg),
	substitute_vars(Arg,HV,NArg),
	arg(N,Y,NArg),
	N1 is N-1,
	function_substitute_vars(N1,X,HV,Y).
*/
%
% Test if a expresion contains only operators ,e.d. it can be 
% evaluated without using intermediate variables,so it can be
% put in the right hand of the "is" operator. 
%

evaluable(X) :-
	number(X),!.
evaluable(X) :-
	compound(X),!,
	comp_evaluable(X).

comp_evaluable(X) :-
	variable(X),
        !.
comp_evaluable(X+Y) :-
        !,
	evaluable(X),
	evaluable(Y).
comp_evaluable(X-Y) :-
        !,
	evaluable(X),
	evaluable(Y).
comp_evaluable(X*Y) :-
        !,
	evaluable(X),
	evaluable(Y).
comp_evaluable(X/Y) :-
        !,
	evaluable(X),
	evaluable(Y).
comp_evaluable(-X) :-
        !,
	evaluable(X).
comp_evaluable(exp(X,Y)) :-
        !,
	evaluable(X),
	evaluable(Y).
comp_evaluable(log(X,Y)) :-
        !,
	evaluable(X),
	evaluable(Y).
comp_evaluable(fact(X)) :-
        !,
	evaluable(X).
comp_evaluable(max(X,Y)) :-
        !,
	evaluable(X),
	evaluable(Y).
comp_evaluable(min(X,Y)) :-
        !,
	evaluable(X),
	evaluable(Y).
% Review this!--------------------------------
/* comp_evaluable(X,Y) :-
	functor(X,arg,2),
	function_expr(X,Y).
comp_evaluable(X,Y) :-
	functor(X,arity,1),
	function_expr(X,Y).
comp_evaluable(X,Y) :-
	functor(X,head,1),
	function_expr(X,Y).
comp_evaluable(X,Y) :-
	functor(X,tail,1),
	function_expr(X,Y).
comp_evaluable(X,Y) :-
	userfunc(X),
	userfunc_evaluable(X,Y). */
% ---------------------------------------------


%% Added 20 Oct 97

 %% gen_eval_expresion_using_argposdict(+Size, -Exp, -SizeArg, -ArgPosDict)
 %% Size: a size expression.
 %% Exp: expression that compute a particular size. It has logic variables
 %%      instead of argument positions.
 %% SizeArg: logic variable that will be bound to the computed size once
 %%          Exp is evaluated. 
 %% ArgPosDict: argument position dictionary.

gen_eval_expresion_using_argposdict(bot, _, bot, _ArgPosDict).
gen_eval_expresion_using_argposdict(inf, _, inf, _ArgPosDict).
gen_eval_expresion_using_argposdict(num(Size), _, Size, _ArgPosDict).
gen_eval_expresion_using_argposdict(vari(Size), _, SizeArg, ArgPosDict):- 
	substitute_sizevars_using_argposdict(Size, SizeArg, ArgPosDict).
gen_eval_expresion_using_argposdict(c_func(FuncName,Vars), Exp,
                                    SizeArg, _ArgPosDict):-
%% Warning!! actualize this!.
        create_exp_size(FuncName, _HeadVars, Vars, Exp, SizeArg).
gen_eval_expresion_using_argposdict(is_exp(Size),Exp,SizeArg,ArgPosDict):-
	gen_prolog_expre_using_argposdict(Size,Exp,SizeArg,ArgPosDict).


gen_prolog_expre_using_argposdict(Size, Exp, SizeArg, ArgPosDict):-
        substitute_sizevars_using_argposdict(Size, NSize, ArgPosDict),
	Exp = (SizeArg is NSize).


substitute_sizevars_using_argposdict(X, X, _ArgPosDict):-
	number(X),
        !.
substitute_sizevars_using_argposdict($(N), Var, ArgPosDict):-
	integer(N),
        !,
        insert_dyn_size_info($(0, N), ArgPosDict, Var).
substitute_sizevars_using_argposdict($(N, M), Var, ArgPosDict):- 
        integer(N),
        integer(M),
        !,
        insert_dyn_size_info($(N, M), ArgPosDict, Var).
substitute_sizevars_using_argposdict(X, Y, ArgPosDict):-
	functor(X, F, N),
        F \== $,
        N > 0,
        !,
	functor(Y, F, N),
	functor_substitute_sizevars_using_argposdict(N, X, Y, ArgPosDict).
substitute_sizevars_using_argposdict(X, X, _ArgPosDict).


functor_substitute_sizevars_using_argposdict(0, _, _, _ArgPosDict).
functor_substitute_sizevars_using_argposdict(N, X, Y, ArgPosDict) :-
	N > 0,
	arg(N, X, Arg),
	substitute_sizevars_using_argposdict(Arg, NArg, ArgPosDict),
	arg(N, Y, NArg),
	N1 is N - 1,
	functor_substitute_sizevars_using_argposdict(N1, X, Y, ArgPosDict).


% Commented by PLG on 28-Jul-99 (currently is dead code)
 %% 
 %% % 27 Oct 97
 %% 
 %% actual_param_eval_expresion(bot, _LitNum, _, bot, _ArgPosDict).
 %% actual_param_eval_expresion(inf, _LitNum, _, inf, _ArgPosDict).
 %% actual_param_eval_expresion(num(Size), _LitNum, _, Size, _ArgPosDict).
 %% actual_param_eval_expresion(vari(Size), LitNum, _, SizeArg, ArgPosDict):- 
 %%      actual_time_function_parameters(Size, LitNum, SizeArg, ArgPosDict).
 %% actual_param_eval_expresion(c_func(FuncName, Vars), _LitNum, Exp,
 %%                                     SizeArg, _ArgPosDict):-
 %% %% Warning!! actualize this!.
 %%      create_exp_size(FuncName, _HeadVars, Vars, Exp, SizeArg).
 %% actual_param_eval_expresion(is_exp(Size), LitNum, Exp, SizeArg,
 %%                             ArgPosDict):-
 %%      actual_time_function_parameters(Size, LitNum, NSize, ArgPosDict),
 %%      Exp = (SizeArg is NSize).
 %% 
 %% %
% End Commented by PLG on 28-Jul-99


actual_time_function_parameters(X, _SizeRel, _Literal, _LitNum, X,
                                _ArgPosDict, _NeededVars):-
        number(X),
        !.
actual_time_function_parameters($(0, ArgNum), SizeRel, Literal,
                                LitNum, Exp, ArgPosDict, NeededVars):- 
        integer(ArgNum),
        !,
        generate_size_key(Literal, LitNum, ArgNum, Key),
        find_size(SizeRel, Key, Exp),
        substitute_sizevars_using_argposdict(Exp, _Exp1, ArgPosDict),
        substitute_sizevars_using_argposdict(Exp, _Exp2, NeededVars).
actual_time_function_parameters(X, SizeRel, Literal, LitNum, Y,
                                ArgPosDict, NeededVars):-
	functor(X, F, N),
        F \== $,
        N > 0,
        !,
	functor(Y, F, N),
	functor_actual_time_function_parameters(N, SizeRel, Literal,
                                       LitNum, X, Y, ArgPosDict, NeededVars).
actual_time_function_parameters(X, _SizeRel, _Literal, _LitNum, X,
                                _ArgPosDict, _NeededVars).


functor_actual_time_function_parameters(0, _SizeRel, _Literal,
                                 _LitNum, _, _, _ArgPosDict, _NeededVars).
functor_actual_time_function_parameters(N, SizeRel, Literal, LitNum,
                                        X, Y, ArgPosDict, NeededVars) :-
	N > 0,
	arg(N, X, Arg),
	actual_time_function_parameters(Arg, SizeRel, Literal, LitNum,
                                        NArg, ArgPosDict, NeededVars),
	arg(N, Y, NArg),
	N1 is N - 1,
	functor_actual_time_function_parameters(N1, SizeRel, Literal,
                                         LitNum, X, Y, ArgPosDict, NeededVars).


