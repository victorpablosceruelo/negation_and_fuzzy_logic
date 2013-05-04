:- package(mutables).
% TODO: Not sure if this should be a package. At least, this is a
% change in the semantics.

:- op(50, fx, [(@)]). % TODO: conflict with basicmodes.pl, isomodes.pl
:- op(980, xfx, [(<-)]). % priority between (::) and (,); conflict with package bf
:- op(700, xfx, [(+=)]). % TODO: missing many others

:- use_module(library(mutables(mutables_rt))).
