/* ?- module(neg_disj).
{Loading current module from /home/pawel/Systems/CiaoDE/ciaopp/bugs/neg_disj.pl
{loaded in 2303.648 msec.}
}

yes
?- analyze(eterms).
{Analyzing /home/pawel/Systems/CiaoDE/ciaopp/bugs/neg_disj.pl
{preprocessed for plai in 2.999 msec.}
{In /home/pawel/Systems/CiaoDE/ciaopp/bugs/neg_disj.pl
WARNING (fixpo_ops): (lns 4-5) Unknown predicate ;/2
}
{analyzed by plai using eterms in 5.0 msec.}
}
*/

:- module(_,[p/1],[assertions]).

p(A) :- 
	\+ (q(A); r(A)).
 
q(a).
r(b).

