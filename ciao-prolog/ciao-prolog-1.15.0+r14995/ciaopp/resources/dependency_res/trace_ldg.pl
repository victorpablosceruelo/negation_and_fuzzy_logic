:- module(trace_ldg, [trace_ldg/1], []).

:- use_module(library(graphs(ugraphs)), [vertices_edges_to_ugraph/3]).
:- use_module(library(davinci)).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(resources(dependency_res(trace_adg)), [node2atom/2]).

:- data node/1.
:- data edge/2.

trace_ldg([[]]) :- !.
trace_ldg(Ldg) :- !,
	end_view,
	clean_ldg,
	start_view,
	create_graph(Ldg, 1),
	draw_ldg.


clean_ldg :-
	retractall_fact(node(_)),
	retractall_fact(edge(_, _)).

start_view :-
	davinci.

draw_ldg :-
	findall(lit(N1) - lit(N2),
	    edge(N1, N2),
	    Edges),
	findall(lit(N),
	    node(N),
	    Nodes),
	vertices_edges_to_ugraph(Nodes, Edges, Graph),
	davinci_ugraph(Graph).

end_view :- davinci_quit, !.
end_view.


create_graph([],           _).
create_graph([Pred|Preds], P_Id) :-
	create_graph_pred(Pred, P_Id, 1),
	NP_Id is P_Id + 1,
	create_graph(Preds, NP_Id).

create_graph_pred([],       _,    _).
create_graph_pred([Cl|Cls], P_Id, C_Id) :-
	atom_number(P, P_Id),
	atom_number(C, C_Id),
	atom_concat([P, '#', C, '#'], Prefix),
	create_graph_clause(Cl, Prefix),
	NC_Id is C_Id + 1,
	create_graph_pred(Cls, P_Id, NC_Id).

create_graph_clause(LDGs, _) :-
	\+ \+ var(LDGs), !.
create_graph_clause([ldg(Lit, Pred, Succ, _, _, _, _)|Lits], Prefix) :-
	concat_node(Lit, Prefix, Node),
	asserta_node(Node),
	asserta_edges(Node, Pred, Succ, Prefix),
	create_graph_clause(Lits, Prefix).

asserta_node(Node) :-
	current_fact(node(Node)), !.
asserta_node(Node) :-
	asserta_fact(node(Node)), !.

concat_node($(+), Prefix, Node) :-
	atom_concat([Prefix, 'HEAD'], Node).
concat_node($(-), Prefix, Node) :-
	atom_concat([Prefix, 'HEAD'], Node).
concat_node(Lit, Prefix, Node) :-
	node2atom(Lit, Lit0),
	atom_concat([Prefix, Lit0], Node).

asserta_edges(Node, Pred, Succ, Prefix) :-
	asserta_pred(Pred, Node, Prefix),
	asserta_succ(Succ, Node, Prefix).

asserta_pred(Ns, _, _) :-
	\+ \+ var(Ns), !.
asserta_pred([N|Ns], Node, Prefix) :-
	concat_node(N, Prefix, N0),
	asserta_edge(N0, Node),
	asserta_pred(Ns, Node, Prefix).

asserta_succ(Ns, _, _) :-
	\+ \+ var(Ns), !.
asserta_succ([N|Ns], Node, Prefix) :-
	concat_node(N, Prefix, N0),
	asserta_edge(Node, N0),
	asserta_succ(Ns, Node, Prefix).

asserta_edge(N1, N2) :-
	current_fact(edge(N1, N2)), !.
asserta_edge(N1, N2) :-
	asserta_fact(edge(N1, N2)), !.
