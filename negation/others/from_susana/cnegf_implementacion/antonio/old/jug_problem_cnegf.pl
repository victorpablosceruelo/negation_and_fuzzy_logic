:- module(jug_problem_cnegf, _,[.(cnegf)]).

:- use_module(library(dynamic),[asserta/1,asserta/2,retractall/1]). 

/* SYS$USER2:[PROLOG.PRO]JUG.DAT;2 */

/* This is a simple problem to show the use of Prolog for
   doing simple depth-first search of a graph.

   The water-jug problem is as follows:
      Jug A holds 5 liters, and jug B holds 2 liters.
      Starting with the 5-liter jug full, pour water
      from one jug to another or down the drain until
      you are sure that jug B contains exactly 1 liter.
      Each state is represented by a pair  Aamout:Bamount
      telling how much water is contained in each jug.    */

?-op(100,yfx,':').  /* define : as a left-associative operator */

/* The major predicate is
  solve(Current_state, Goal_state, Traversed_path, Solution_path).
   where Traversed_path is a list of pourings made so far.     */

solve(Goal, Goal, Path, Path).  /* If the current state is the
          goal state, then output the path to the 4th argument */

solve(Current, Goal, Path, Solution) :-
      edge(Step, Current, New),  /* Find 4 ways of pouring -- 'Step' */
      \+ marked(solve(New, Goal, _, _)),  /* Graph search requires
                    checking whether we've searched this node before */
      solve(New, Goal, Path:Step, Solution). /* Use recursion to do
                  depth-first search.  On failure, backup to 'pour'. */

edge(pour_a_down_drain, A:B , 0:B).
edge(pour_b_down_drain, A:B , A:0).
edge(pour_a_into_b, A:B, C:D) :-
       A>0, B<2, T is A+B, (T>=2, C is T-2, D=2 ; T<2, C=0, D=T) .
edge(pour_b_into_a, A:B, C:D) :-
       B>0, A<5, T is A+B, (T>=5, D is T-5, C=5 ; T<5, C=T, D=0) .

/*Check whether a node was already searched.  The rule
    marked(X) :- asserta((marked(X):- !)), fail.
causes a node to be marked if it was not marked before, and fails.
The next time it checks this node, it will succeed since that node
had been asserted to be marked.  Reset_marked permits the
depth-first search to be done several times.   */

reset_marked :- retractall(marked(_)),
      asserta( (marked(X) :- asserta((marked(X):- !)), fail) ).

jug_problem(X,Y,Z) :- reset_marked, solve(X,Y,start,Z).

no_jug_problem(X,Y,Z) :- cnegf(jug_problem(X,Y,Z)).
/* ?-jug_problem(5:0, A:1, Answer).*/







