:- module(native,
	[ builtin/2, builtin_package/1, 
	  native_prop_map/3, native_prop_term/1,
	  native_builtin/2, native_property/2 ],
	[assertions, regtypes, nortchecks]).

:- use_module(library(lists), [append/3]).
%:- use_module(engine(internals),[term_to_meta/2]).
term_to_meta(Term,Term).

%------------------------------------------------------------------------%

:- doc(module,"This module has the basic procedures for understanding
	predicates as builtins and properties as native properties.").

%------------------------------------------------------------------------%

:- doc(builtin/2,
   "checks that a user level (property) predicate is declared native and
    translates it into the internal representation for that native
    (builtin or property) predicate.").

builtin(regtype(Term),'basic_props:regtype'(Meta)):-
	term_to_meta(Term,Meta).
builtin(native(Term,Y),'basic_props:native'(Meta,Y)):-
	term_to_meta(Term,Meta).
%% builtin(native(Term,Term),native(Meta)):-
%% 	term_to_meta(Term,Meta).
builtin(native(Term,Y),'basic_props:native'(Meta)):-
	term_to_meta(Term,Meta),
	Meta=..[F|Args],
	unexpand(F,FY),
	Y=..[FY|Args].
builtin(entry_point_name(Term,Y),'native_props:entry_point_name'(Meta,Y)):-
	term_to_meta(Term,Meta).

unexpand(F,FT):-
	atom_codes(F,Codes),
	append(_MCodes,":"||FTCodes,Codes), !,
	atom_codes(FT,FTCodes).
unexpand(F,F).

:- doc(hide,builtin_package/1).

builtin_package(nativeprops).

:- doc(bug, "Verify why this module is not compatible with
	rtchecks. -- EMM").

%------------------------------------------------------------------------%
:- doc(native_prop_term/1,"Defined as @includedef{native_prop_term/1}").
:- true prop native_prop_term(Prop) + regtype 
   # "@var{Prop} identifies a native property.".

native_prop_term(Prop):- native:native(Prop).
native_prop_term(regtype(Prop)):- callable(Prop).

:- redefining(native/1).
:- doc(doinclude,native/1).
:- doc(native(Prop),
   "@var{Prop} is (the internal representation of) a native property:
    one that CiaoPP knows natively how to manipulate.
    @includedef{native/1}").
:- regtype native/1.

% state
native(covered(V,L)):- var(V), list(L,var).
native(free(L)):- vlist(L).
native(ground(L)):- vlist(L).
native(indep(L)):- list(L,list(var)).
native(instance(V,T)):- var(V), nonvar(T).
native(linear(L)):- vlist(L).
native(not_free(L)):- vlist(L).
native(not_ground(L)):- vlist(L).
native(sharing(L)):- list(L,list(var)).
% computation
native(covered).
native(fails).
native(is_det).
native(mut_exclusive).
native(non_det).
native(not_covered).
native(not_fails).
native(not_mut_exclusive).
native(sideff(X)):- member(X,[pure,soft,hard]).
%native(possibly_fails).
%native(possibly_nondet).
native(steps(C)):- cost_exp(C).
native(steps_lb(C)):- cost_exp(C).
native(steps_ub(C)):- cost_exp(C).
native(steps_o(C)):- cost_exp(C).
native(cost(C)):- resource_exp(C).    % JNL
native(terminates).
native(size(C)):- cost_exp(C).
native(size(A,B)) :- var(A), cost_exp(B).
native(size_lb(C)):- cost_exp(C).
native(size_ub(C)):- cost_exp(C).
native(size_o(C)):- cost_exp(C).

cost_exp(_). % Not implemented yet --EMM

resource_exp(_). % Not implemented yet --EMM

:- doc(doinclude,vlist/1).
:- doc(vlist/1,"@includedef{vlist/1}").
:- regtype vlist/1.

vlist(L):- var(L).
vlist(L):- list(L,var).

:- doc(native_prop_map(Prop,P,Vars),
   "@var{Prop} is a property @var{P}(@var{Vars}) representing a mapping
    @tt{map}(@var{Vars},@var{P})").

native_prop_map(free(Vars),free,Vars):-
	nonvar(Vars).
native_prop_map(not_free(Vars),not_free,Vars):-
	nonvar(Vars).
native_prop_map(not_ground(Vars),not_ground,Vars):-
	nonvar(Vars).

%------------------------------------------------------------------------%
% Should correspond with native/1 above:
:- doc(hide,native_property/2).

:- use_module(library(resdefs(rescostfunc)), [compact_cf/3]).

native_property('native_props:covered'(V,L),covered(V,L)).
native_property('term_typing:var'(L),free(L)).
native_property('term_typing:ground'(L),ground(L)).
native_property('andprolog_rt:indep'(L),indep(L)).
native_property('native_props:instance'(V,T),instance(V,T)).
native_property('native_props:linear'(L),linear(L)).
native_property('term_typing:nonvar'(L),not_free(L)).
native_property('native_props:nonground'(L),not_ground(L)).
native_property('native_props:mshare'(L),sharing(L)).
% computation
native_property('native_props:covered'(G),covered(G)).
native_property('native_props:fails'(G),fails(G)).
native_property('native_props:possibly_fails'(G),possibly_fails(G)).
native_property('native_props:is_det'(G),is_det(G)).
native_property('native_props:mut_exclusive'(G),mut_exclusive(G)).
native_property('native_props:non_det'(G),non_det(G)).
native_property('native_props:not_covered'(G),not_covered(G)).
native_property('native_props:not_fails'(G),not_fails(G)).
native_property('native_props:not_mut_exclusive'(G),not_mut_exclusive(G)).
%native_property('basic_props:sideff'(G,X),sideff(G,X)).  %ACC
native_property('basic_props:native'(G),sideff(G,_)).     %ACC
native_property('native_props:steps'(G,C),steps(G,C)).
native_property('native_props:steps_lb'(G,C),steps_lb(G,C)).
native_property('native_props:steps_ub'(G,C),steps_ub(G,C)).
native_property('native_props:steps_o'(G,C),steps_o(G,C)).
native_property('resources_props:cost'(G,Rel,Ap,Type,R,_,IF,CFN),
	cost(G,Rel,Ap,Type,R,CF)) :- compact_cf(CFN,IF,CF).
native_property('native_props:terminates'(G),terminates(G)).
native_property('native_props:size'(G,C),size(G,C)).
native_property('native_props:size_lb'(G,C),size_lb(G,C)).
native_property('native_props:size_ub'(G,C),size_ub(G,C)).
native_property('native_props:size_o'(G,C),size_o(G,C)).

%------------------------------------------------------------------------%

:- doc(hide,native_builtin/2).

native_builtin('basiccontrol:fail',fail). % used in spec.pl
native_builtin('basiccontrol:$metachoice'(V),metachoice(V)). % in remotecut.pl
native_builtin('basiccontrol:$metacut'(V),metacut(V)). % in remotecut.pl

% ACC
native_builtin('andprolog_rt:&'(A,B),
               ampersand(A,B)).              % in tr_parallel.pl
native_builtin('andprolog_rt:&!'(A,B),
               ampersand_det(A,B)).          % in tr_parallel.pl
native_builtin('andprolog_rt:&>'(A,B),
               amp_publish_goal(A,B)).       % in tr_parallel.pl
native_builtin('andprolog_rt:&!>'(A,B),
               amp_publish_goal_det(A,B)).   % in tr_parallel.pl
native_builtin('andprolog_rt:<&'(A),
               amp_get_result(A)).           % in tr_parallel.pl
native_builtin('andprolog_rt:<&!'(A),
               amp_get_result_det(A)).       % in tr_parallel.pl

native_builtin('arithmetic:is'(X,Y),is(X,Y)).
native_builtin('term_basic:='(X,Y),=(X,Y)).
native_builtin('io_basic:display'(X),display(X)).
native_builtin('write:write'(X),write(X)).
native_builtin('io_basic:nl',nl).
native_builtin(_,_):- fail.
/* have to add module qualifications:
native_builtin(<(X,Y),<(X,Y)).
native_builtin(=(X,Y),=(X,Y)).
native_builtin(=..(X,Y),=..(X,Y)).
native_builtin(=:=(X,Y),=:=(X,Y)).
native_builtin(=<(X,Y),=<(X,Y)).
native_builtin(==(X,Y),==(X,Y)).
native_builtin(=\\=(X,Y),=\\=(X,Y)).
native_builtin(>(X,Y),>(X,Y)).
native_builtin(>=(X,Y),>=(X,Y)).
native_builtin(@<(X,Y),@<(X,Y)).
native_builtin(@=<(X,Y),@=<(X,Y)).
native_builtin(@>(X,Y),@>(X,Y)).
native_builtin(@>=(X,Y),@>=(X,Y)).
native_builtin(\\==(X,Y),\\==(X,Y)).
native_builtin(arg(X,Y,Z),arg(X,Y,Z)).
native_builtin(atom(X),atom(X)).
native_builtin(atomic(X),atomic(X)).
native_builtin('operators:current_op'(X,Y,Z),current_op(X,Y,Z)).
%native_builtin(display(X),display(X)).
native_builtin(erase(X),erase(X)).
native_builtin(fail,fail).
native_builtin(float(X),float(X)).
native_builtin(functor(X,Y,Z),functor(X,Y,Z)).
native_builtin(get_code(X),get_code(X)).
native_builtin(ground(X),ground(X)).
native_builtin(integer(X),integer(X)).
native_builtin(is(X,Y),is(X,Y)).
native_builtin(member(X,Y),member(X,Y)).
native_builtin(name(X,Y),name(X,Y)).
%native_builtin(nl,nl).
native_builtin(nonvar(X),nonvar(X)).
native_builtin(number(X),number(X)).
native_builtin('write:numbervars'(X,Y,Z),numbervars(X,Y,Z)).
native_builtin(repeat,repeat).
native_builtin(true,true).
native_builtin(var(X),var(X)).
*/
