:- module(queens_ui, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

% Graphical interface for queens benchmark 

:- use_module(library(js_dom)).
:- use_class(.(queens_ui(queens_solution_widget))).

:- use_module(.(queens_ui(queens8))).
% TODO: CLP(FD) version is indeed slower for small chess board
%:- use_module(.(queens_ui(queens8_fd))).

% Files that will be required at execution time
:- resource(.(queens_ui('queens_ui.css'))). % TODO: get hooks as a class or predicate?

:- inline_resource('queens_ui.html', "
<html>
<head>
<title></title>
<script type=\"application/javascript\" src=\"queens_ui.out.js\"></script>
</head>
<body onload=\"__ciao_start__()\"></body>
</html> 
").

:- export(main/0).
main :-
	Title = "8-Queens Demo",
        js_dom.set_title(Title),
	js_dom.load_css("queens_ui.css"),
	Body = @(~js_dom.document.body), % TODO: mut deref should not be necessary
	add_div_with_class(Body, "page", "page"),
	Page = ~js_dom.elem("page"),
	add_h1(Page, Title),
	add_p(Page, "Graphical visualization of solutions for the Queens puzzle."),
	% TODO: improve message
	add_p(Page, "Note for Chrome users: when accessing this page locally (not through HTTP), you may be required to disable Same-Origin Policy in chrome (--allow-file-access-from-files option)."),
	add_br(Page),
	add_div(Page, "basediv"),
	Base = ~js_dom.elem("basediv"),
	%
        Puzzle = ~queens8,
%        Puzzle = ~queens8_fd,
        queens_solution_widget(~Puzzle.'$name',
                               ~Puzzle.name,
	                       ~Puzzle.repeat_count,
			       Data,
			       Puzzle.solve(Data)).attach(Base),
	%
	add_br(Page),
	add_br(Page),
	add_qr_code(Page).

:- resource(.(queens_ui('queens_ui_qrcode.png'))).
add_qr_code(Page) :-	%
	I = ~js_dom.createElement("img"),
%	I.className <- "qrcode",
	I.src <- "queens_ui_qrcode.png",
	Page.append_child(I).
