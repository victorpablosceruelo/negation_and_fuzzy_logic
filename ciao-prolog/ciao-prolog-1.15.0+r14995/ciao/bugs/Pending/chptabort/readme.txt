Are spurious choicepoints being left after doing C-c and abort or is
it just a problem of the toplevel?

Here is the trace that reproduces the error.

===========================================================================
Ciao 1.15.0-14314:14333: Tue Nov 29 10:02:30 CET 2011
?- X = 1; X = 2.

X = 1 ? n

X = 2 ? n

no
?-   C-c C-c
Ciao interruption (h for help)? a
{ Execution aborted }
?- X = 3; X = 4.

X = 3 ? n

X = 4 ? n

no
?-   C-c C-c
Ciao interruption (h for help)? a
{ Execution aborted }
?- X = 4; X = 5.

X = 4 ? n

X = 4 ? n

X = 5 ? n

no
?- 
