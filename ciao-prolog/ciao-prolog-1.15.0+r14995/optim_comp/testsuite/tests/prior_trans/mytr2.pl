:- module(mytr2, _, _).

t_goal(g(1, _), g(3, wrong)). % only if translations applied in wrong order
t_goal(g(2, A), g(3, (2,A))).
t_term(t(A), t((2,A))).
t_clause(clause(c(H),B), clause(c((2,H)),B)).
t_sentence(s(A), s((2,A))).
