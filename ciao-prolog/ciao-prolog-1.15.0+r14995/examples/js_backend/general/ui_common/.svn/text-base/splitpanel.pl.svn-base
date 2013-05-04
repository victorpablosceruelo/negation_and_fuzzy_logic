:- class(splitpanel, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- export(left/1).
:- attr left.

:- export(right/1).
:- attr right.

:- export(attach/1).
attach(Container) :-
	Table = ~js_dom.createElement("table"),
	Tr = ~js_dom.createElement("tr"),
	~left = ~js_dom.createElement("td"),
	left.style.verticalAlign <- "top",
	~right = ~js_dom.createElement("td"),
	right.style.verticalAlign <- "top",
	Tr.append_child(~left),
	Tr.append_child(~right),
	Table.append_child(Tr),
	Container.append_child(Table).

