
:- module(det,[det/2,nondet/2],[assertions]).

% typesfd, shfr, det, ctchecks (do NOT take the entry out!)

:- use_module(library(assertions(native_props))).

% is_det is checked
:- pred det(A,B) + is_det 
# "The basic relation between each two nodes @var{A}-@var{B}.".

det(a,b).
det(b,c).
det(c,a).

% non_det is refuted
:- entry nondet(A,B) : ( ground(A), var(B) ).
:- pred nondet(A,B) + non_det 
# "The closure of the previous relation det(@var{A}-@var{B}).".

nondet(a,b).
%forgot: nondet(a,c).
%forgot: nondet(b,a).
nondet(b,c).
nondet(c,a).
%forgot: nondet(c,b).
