:- class(simple_run_widget, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- use_module(library(arithpreds)).

% A widget with a button to invoke a goal

:- export(title/1).
:- attr title.

:- export(goal/1).
:- attr goal.

:- export(cons__/2).
cons__(Title, Goal) :-
	~title = Title, ~goal = Goal.

:- export(attach/1).
attach(Container) :-
        Id = ~id_factory.new_id,
        Div = ~js_dom.createElement("div"),
        Div.className <- "problembox",
	Div.innerHTML <-
"<form><span>" + ~title + "</span>
  <input type='button' name='absint_button' id='"+ Id +"'/>
</form>",
        Container.append_child(Div),
        Button2 = ~js_dom.elem(Id),
        Button2.value <- "Run",
        Button2.onclick <- ~js_dom.event_handler(~goal).

