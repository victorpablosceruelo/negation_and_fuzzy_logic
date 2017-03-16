:- module(trace_adg, [trace_adg/1, node2atom/2], []).

:- use_module(library(graphs(ugraphs)), [vertices_edges_to_ugraph/3]).
:- use_module(library(davinci)).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms), [atom_concat/2]).

:- data node/1.
:- data edge/2.

trace_adg([[]]) :- !.
trace_adg(Adg) :- !,
	end_view,
	clean_adg,
	start_view,
	create_graph(Adg, 1),
	draw_adg.

clean_adg :-
	retractall_fact(node(_)),
	retractall_fact(edge(_, _)).

start_view :-
	davinci.

draw_adg :-
	findall(arg(N1) - arg(N2),
	    edge(N1, N2),
	    Edges),
	findall(arg(N),
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

create_graph_clause(ADGs, _) :-
	\+ \+ var(ADGs), !.
create_graph_clause([adg(Node, Pred, Succ, _Mode)|ADGs], Prefix) :-
	node2atom(Node, Node1),
	atom_concat([Prefix, Node1], Node0),
	asserta_node(Node0, _M),
	asserta_edges(Node0, Pred, Succ, Prefix),
	create_graph_clause(ADGs, Prefix).

asserta_node(Node, _Mode) :-
	current_fact(node(Node)), !.
asserta_node(Node, _Mode) :-
	asserta_fact(node(Node)), !.

asserta_edge(N1, N2) :-
	current_fact(edge(N1, N2)), !.
asserta_edge(N1, N2) :-
	asserta_fact(edge(N1, N2)), !.

asserta_edges(Node, Pred, Succ, Prefix) :-
	asserta_pred(Pred, Node, Prefix),
	asserta_succ(Succ, Node, Prefix).

asserta_pred(Ns, _, _) :-
	\+ \+ var(Ns), !.
asserta_pred([N|Ns], Node, Prefix) :-
	node2atom(N, N1),
	atom_concat([Prefix, N1], N0),
	asserta_edge(N0, Node),
	asserta_pred(Ns, Node, Prefix).

asserta_succ(Ns, _, _) :-
	\+ \+ var(Ns), !.
asserta_succ([N|Ns], Node, Prefix) :-
	node2atom(N, N1),
	atom_concat([Prefix, N1], N0),
	asserta_edge(Node, N0),
	asserta_succ(Ns, Node, Prefix).

node2atom($(N1), Atom) :-
	atom_number(X1, N1),
	atom_concat(['$(', X1, ')'], Atom).
node2atom($(N1, N2), Atom) :-
	atom_number(X1, N1),
	atom_number(X2, N2),
	atom_concat([X1, '$', X2], Atom).
