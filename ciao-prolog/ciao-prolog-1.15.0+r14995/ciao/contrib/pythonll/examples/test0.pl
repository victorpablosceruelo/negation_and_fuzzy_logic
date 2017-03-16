% This example shows
% - python object creation
% - python method invocation
% Q : Questions
:- module(test0, [test00/0,test01/0, test02/0, main/0], []).

:- use_module(library(pythonll(pythonsock))).
:- use_module(library(pythonll(pythonrt))).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(terms)).
operator("+").

main :-
    python_start,
    run_tests([(test00,'test00'),
              (test01,'test01'),
              (test02,'test02')]),
    pause(3600).

run_tests([]).
run_tests([(T,N)|R]) :- run_test(T, N), run_tests(R).
run_test(Test, Name) :-
    atom_concat(['run ', Name], Start), atom_concat([Name, ' finished'], End),
    display(Start), Test, display(End), nl.

%% ----------------------------------------------------
%  Custom class instanciation
%% ----------------------------------------------------
test00:-
%% My dummy module (should not be done here?)
%%    python_load_module('',Mymodule)
    python_create_object('pl2pylib.Sum'(5), MySum),
    python_invoke_method(MySum,asList(_)),
    python_invoke_method(MySum,compute(_)),
    python_invoke_method(MySum,printme(_)).

test01 :-
    %% FIXME: raise an error if type is not correct (pb. with number of args.?)
    python_create_object('pl2pylib.Sum'(8, "1+2+3+4+5+6+7"), MySum),
    python_invoke_method(MySum,asList(_)), %% to be able to add return result in args
    python_invoke_method(MySum,compute(_)),
    python_invoke_method(MySum,printme(_)).
%% todo: one arg, one string

test02 :-
    %% FIXME: raise an error if type is not correct (pb. with number of args.?)
    python_create_object('pl2pylib.Sum', MySum),
    python_invoke_method(MySum,asList(_)),
    python_invoke_method(MySum,compute(_)),
    python_invoke_method(MySum,printme(_)).

%% ----------------------------------------------------
%  Module import
%% ----------------------------------------------------
/*test_import01 :- % import os, print os.path?
    python_import_module(),*/

%% ----------------------------------------------------
%  Existing class instanciation
%% ----------------------------------------------------

