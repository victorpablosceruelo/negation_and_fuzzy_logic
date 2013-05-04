%% -------------------------------------------------------------------------
:- module(_,_,[make,fsyntax]).
:- use_module(library(system_extra)).
:- use_module(library(lists), [append/3]).
:- use_module(library(terms), [atom_concat/2]).

:- discontiguous(comment/2).

%% -------------------------------------------------------------------------
%% A simple target. Defines how to produce file 'hw'.

hw <-  []    :-
 	writef("Hello world", hw).

%% A comment describing this target (see below):
comment(hw,['Generation of file hw']).

%% -------------------------------------------------------------------------
%% A target with a dependency. 'hwhw' requires 'hw'.

hwhw <- [hw] :-
	readf(hw,Content),
	append(Content,[0'\n|Content],DoubleContent),
	writef(DoubleContent,hwhw).

comment(hwhw,['Generation of file hwhw']).

%% -------------------------------------------------------------------------
%% A simple target. Defines how to produce file 'datafile.simple'.

'datafile.simple' <-  :-
	writef("Hello world", 'datafile.simple').

comment('datafile.simple',['Generation of file datafile.simple']).

%% -------------------------------------------------------------------------
%% A dependency based on suffixes: 
%% <file>.double is generated always from <file>.simple

double <= simple :: Name :-
	readf(~atom_concat([Name,'.simple']),Content),
	append(Content,[0'\n|Content],DoubleContent),
	writef(DoubleContent,~atom_concat([Name,'.double'])).

%% -------------------------------------------------------------------------
%% A dependency based on suffixes with a precondition.
%% <file>.double is generated always from <file>.simple, once 
%% precond is done

boo <- :-
	display((double <= simple :: name <- precond :- body1, body2)).

%% -------------------------------------------------------------------------
%% Example using library predicates

clean <- [] # "Cleanup of temporary files " :-
	delete_files(~ls('*~|*.asr|*.itf|*.po')).

realclean <- clean :-
	delete_files(~ls('hw|hwhw|*simple|*double')).

comment(realclean,['Cleanup of all generated files']).

%% -------------------------------------------------------------------------
%% Reporting progress and documenting commands: 
%% If target_comment/1 is defined it can be used to produce user-defined 
%% output when targets are processed and/or documentation on what each 
%% target does (used for example when lpmake is called with -h). Using 
%% 'generation of foo' instead of 'generating foo' in comments helps in this 
%% dual purpose.
%% -------------------------------------------------------------------------

:- push_prolog_flag(multi_arity_warnings,off).
%% Make calls target_comment/1 for simple targets:
target_comment(Target) :- 
	comment(Target,Comment),
	display(~atom_concat([~atom_concat(Comment), '\n'])).
:- pop_prolog_flag(multi_arity_warnings).

%% Similarly, make calls dependency_comment/3 for dependencies (only 
%% during execution, not when documenting -h).
dependency_comment(SSuffix,TSuffix,FileBase) :- 
	display(~atom_concat(['Generation of ',FileBase,TSuffix,
                              ' from ',FileBase,SSuffix,'\nl'])).
