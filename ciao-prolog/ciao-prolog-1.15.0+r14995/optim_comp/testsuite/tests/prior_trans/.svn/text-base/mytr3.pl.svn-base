:- module(mytr3, _, _).

t_goal(g(1, _), g(4, wrong)). % only if translations applied in wrong order
t_goal(g(2, _), g(4, wrong)). % only if translations applied in wrong order
t_goal(g(3, A), g(4, (3,A))).
t_term(t(A), t((3,A))).
t_clause(clause(c(H),B), clause(c((3,H)),B)).
t_sentence(s(A), s((3,A))).
