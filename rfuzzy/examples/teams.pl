:- module(teams,_,[rfuzzy, clpr]).

:- prop type_Candidate/1.
type_Candidate(john).
type_Candidate(ken).
type_Candidate(bob).
type_Candidate(ann).

:- set_prop successful_team/2 => type_Candidate, type_Candidate.
:- default(successful_team/2, 0.1).
(successful_team(W1,W2) cred(max,0.5)) :~ dluka get_along(W1,W2),prepared_team(W1,W2).

:- set_prop prepared_team/2 => type_Candidate, type_Candidate.
:- default(prepared_team/2, 0.1).
prepared_team(W1,W2) :~ prod efficency(W1),efficency(W2),knowledge(W1),knowledge(W2).

:- set_prop get_along/2 => type_Candidate, type_Candidate.
:- default(get_along/2, 0.1).
get_along(W1,W2) :~ min likes_partner(W1,W2),likes_partner(W2,W1).


:- set_prop efficency/1 => type_Candidate.
:- set_prop likes_partner/2 => type_Candidate, type_Candidate.
:- set_prop knowledge/1 => type_Candidate.

:- default(efficency/1, 0.65).
:- default(likes_partner/2, 0.65).
:- default(knowledge/1, 0.65).

efficency(john) value 0.5.
efficency(ann) value 0.85.

likes_partner(john,ken) value 0.3.
likes_partner(john,bob) value 0.9.
likes_partner(john,ann) value 0.7.
likes_partner(ken,ann) value 0.2.
likes_partner(bob,john) value 0.5.
likes_partner(bob,ann) value 0.8.
likes_partner(ann,ken) value 1.

knowledge(ann) value 0.8.
knowledge(bob) value 0.4.
