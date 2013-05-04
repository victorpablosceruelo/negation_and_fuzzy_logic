:- use_package([oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

% TODO: port CLP(FD) and pick examples from Eclipse
% TODO: put more examples (e.g. the ones in the Prolog programming contest)
%       http://www.cs.kuleuven.be/~dtai/ppcbook/

:- use_module(library(js_dom)).
:- use_class(engine(streams_div)).
:- use_class(.(ui_common(splitpanel))).
:- use_class(library(stopwatch)).
% :- use_module(.(ui_common(console))).
:- use_module(library(id_factory)).
:- use_class(.(ui_common(solution_widget))).

:- include(.(testsuite_base)).

% Files that will be required at execution time
:- resource(.('demo.css')). % TODO: get hooks as a class or predicate?

:- export(main/0).
main :-
	Title = "Benchmarking Ciao on JavaScript",
        js_dom.set_title(Title),
	js_dom.load_css("demo.css"),
	Body = @(~js_dom.document.body), % TODO: mut deref should not be necessary
	add_div_with_class(Body, "page", "page"),
	Page = ~js_dom.elem("page"),
	add_h1(Page, Title),
	add_p(Page, "This is a collection of benchmarks and tests for the JavaScript back-end for Ciao. Code is translated to JavaScript and run entirely on your browser without any interaction with a server. Press 'run' to run them as benchmarks or 'query' to show the solutions interactively. Each program will run in an separate worker, so you can explore the solutions of each of them independently."),
	% TODO: improve message
	add_p(Page, "Note for Chrome users: when accessing this page locally (not through HTTP), you may be required to disable Same-Origin Policy in chrome (--allow-file-access-from-files option)."),
	add_br(Page),
	add_div(Page, "basediv"),
	Base = ~js_dom.elem("basediv"),
%	Panel = ~splitpanel,
%	Panel.attach(Base),
%	Left = ~Panel.left,
%	Right = ~Panel.right,
	%
%        console.attach(Right),
	PuzzleBase = Base,
        puzzles.attach(PuzzleBase).

:- module puzzles {
    :- export(container/1).
    :- attr container.

    :- export(attach/1).
    attach(Container) :-
        ~container = Container,
        All = ~all_puzzles,
        do_puzzles(All).

    do_puzzles([]) :- !.
    do_puzzles([X|Xs]) :- do_puzzle(X), do_puzzles(Xs).

    do_puzzle(Puzzle) :-
        solution_widget(~Puzzle.'$name',
                        ~Puzzle.name,
	                ~Puzzle.repeat_count,
			Data,
			Puzzle.solve(Data)).attach(~container).
}.

% TODO: not working yet
% loop_test :-
%         L = [1,2,3],
%         list:for_each(X, L) do display(X), 
%         nl.
