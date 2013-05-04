:- module(demo, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

% TODO: port CLP(FD) and pick examples from Eclipse
% TODO: put more examples (e.g. the ones in the Prolog programming contest)
%       http://www.cs.kuleuven.be/~dtai/ppcbook/

:- use_module(library(js_dom)).
:- use_class(engine(streams_div)).
:- use_class(.(ui_common(splitpanel))).
:- use_class(library(stopwatch)).
:- use_module(.(ui_common(console))).
%:- use_module(.(quick_test)).
%:- include(.(hiord_test)).
%:- include(.(figures)).
%:- include(.(measures)).
:- use_module(library(id_factory)).
:- use_class(.(ui_common(simple_run_widget))).
:- include(.(ui_test)).

% Files that will be required at execution time
:- resource(.('demo.css')).

:- inline_resource('demo.html', "
<html>
<head>
<title></title>
<script type=\"application/javascript\" src=\"demo.out.js\"></script>
</head>
<body onload=\"__ciao_start__()\"></body>
</html> 
").

% Here starts the program
:- export(main/0).
main :-
	Title = "User Interface Demo",
	Subtitle = "Ciao JavaScript back-end",
        js_dom.set_title(Title),
	js_dom.load_css("demo.css"),
	Body = @(~js_dom.document.body), % TODO: mut deref should not be necessary
	add_h1(Body, Title),
	%
	add_div_with_class(Body, "subtitle", "subtitle"),
	Sub = ~js_dom.elem("subtitle"),
	SubtitleNode = ~js_dom.createTextNode(Subtitle),
	Sub.append_child(SubtitleNode),
	%
	add_br(Body),
	add_div(Body, "basediv"),
	Base = ~js_dom.elem("basediv"),
	Panel = ~splitpanel,
	Panel.attach(Base),
	Left = ~Panel.left,
	Right = ~Panel.right,
	%
        ui_test.attach(Left),
%        quick_test.attach(Left),
%        hiord_test.attach(Left),
%        figures.attach(Left),
%        measures.attach(Left),
        console.attach(Right).
