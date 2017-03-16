:- module(mytr1, _, _).

t_goal(g(1, A), g(2, (1,A))).
t_term(t(A), t((1,A))).
t_clause(clause(c(H),B), clause(c((1,H)),B)).
t_sentence(s(A), s((1,A))).
