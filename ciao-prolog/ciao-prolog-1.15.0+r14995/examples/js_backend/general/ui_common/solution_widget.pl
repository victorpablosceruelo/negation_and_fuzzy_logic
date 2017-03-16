:- class(solution_widget, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "A widget to display solutions of a predicate").

:- doc(bug, "This code is a mess. Separate worker stuff from UI").
:- doc(bug, "Move source viewer to a different class/module").

:- use_module(library(arithpreds)).
:- use_class(library(stopwatch)).

:- export(name/1).
:- attr name.
:- export(repeat_count/1).
:- attr repeat_count.
:- export(counter/1).
:- attr counter.
:- export(soldisp/1).
:- attr soldisp.
:- export(timer/1).
:- attr timer.
:- export(timerdisp/1).
:- attr timerdisp.
:- export(sourceviewdisp/1).
:- attr sourceviewdisp.
:- export(sourcedisp/1).
:- attr sourcedisp.
:- export(wrk/1).
:- attr wrk.
:- export(id/1).
:- attr id.

:- export(title/1).
:- attr title.
:- export(data/1).
:- attr data.
:- export(goal/1).
:- attr goal.

:- export(cons__/5).
cons__(Name, Title, RepeatCount, Data, Goal) :-
	~name = Name,
	~title = Title,
	~repeat_count = RepeatCount,
	~data = Data,
	~goal = Goal,
	~id = ~id_factory.new_id,
	~timer = ~stopwatch.

% TODO: define a trait defining widgets
:- export(attach/1).
attach(Container) :-
        write_html(Container),
        ~soldisp = ~streams_div("sol" + ~id),
        ~timerdisp = ~streams_div("time" + ~id),
	%
	~sourceviewdisp = ~js_dom.elem("sourceview" + ~id),
	~sourcedisp = ~js_dom.elem("source" + ~id),
%	Header = ~js_dom.createElement("a"),
%        Header.href <- "boyer.pl",
%        Header.innerHTML <- "View source",
%	sourcedisp.append_child(Header),
	SourceBox = ~sourcebox,
	SourceName = ~name + ".pl", % TODO: a hack
	SourcePick = ~sourcepick(SourceName, SourceBox),
	%
	SourcePick.attach(~sourceviewdisp),
	SourceBox.attach(~sourcedisp),
        %
        ~wrk = ~basiccontrol.worker,
	ui_state__solve.

ui_state__solve :-
	benchmark_button.innerHTML <- "Run",
	reset_button.innerHTML <- "Clean",
        benchmark_button.onclick <- ~js_dom.event_handler(benchmark),
        reset_button.onclick <- ~js_dom.event_handler(restart),
        more_button.onclick <- ~js_dom.event_handler(more_solutions),
	set_button_status(~reset_button, disabled),
        begin_solve.

% Do benchmark (execute all solutions, repeat several times)
benchmark :-
	Vars = ~data, Goal = ~goal,
%	js_dom.console_log("Pressed bench"),
        timerdisp.clear,
	soldisp.hide,
	soldisp.clear,
        wrk.restart,
        wrk.execute(run_benchmark(Vars, Goal)).
%	js_dom.console_log("Finish bench").

% (executed in 'wrk')
run_benchmark(_X, Goal) :-
        timer.reset, % Reset accumulated time
	Count = ~repeat_count,
	timer.start, soldisp.clear,
	repeat_call(Count, Goal),
	% Stop
	stop_clock(bench),
	% The only valid action now is 'restart'
	set_button_status(~benchmark_button, disabled),
	set_button_status(~more_button, disabled),
	set_button_status(~reset_button, enabled),
	%
        % TODO: this 'suspend' should be in the worker 'solve'
        %       operation (to indicate that the goal has finished
        %       successfully)
	basiccontrol.sys.suspend.

repeat_call(Count, Goal) :-
        ( % (failure-driven loop)
	  repeat(Count),
	    Goal,
	    fail
	; true
	).

repeat(_N).
repeat(N) :- N > 1, repeat(N - 1).

% TODO: I could implement 'decorators' as some kind of initialization code,
%       so that events are connected automatically to methods
% 	  (like in Python?)
restart :-
%	js_dom.console_log("Pressed restart"),
        timerdisp.clear,
	soldisp.hide,
	soldisp.clear,
        wrk.restart,
	% All actions are valid now
	% TODO: concurrency problem:
        %   wrk.execute should be started and suspended before; then
        %   we can enabled the buttons
	set_button_status(~benchmark_button, enabled),
	set_button_status(~more_button, enabled),
	set_button_status(~reset_button, disabled),
	%
        begin_solve.

begin_solve :-
	Vars = ~data, Goal = ~goal,
        wrk.execute(solve(Vars, Goal)).

% ---

pause(Message) :-
        more_button.innerHTML <- Message,
%	js_dom.console_log("Suspending"),
	basiccontrol.sys.suspend.

% Make sure that we have a counter
ensure_counter :-
        ( var(~counter) ->
	    ~counter = ~mutables_rt.nb_mut_num(0)
	; counter <- 0
	).

% (executed in 'wrk')
solve(Vars, Goal) :-
%	js_dom.console_log("Ready to solve"),
	ensure_counter,
        pause("Query"), % TODO: we can interrupt this with "Run" (is it correct?)
	set_button_status(~benchmark_button, disabled),
	set_button_status(~reset_button, enabled),
	soldisp.show,
        find_solutions(Vars, Goal).

find_solutions(X, Goal) :-
        timer.reset, % Reset accumulated time
	counter <- 0, timer.start, soldisp.clear,
        ( Goal, solution(X),
	  ~counter += 1,
          ( @(~counter) >= 15 ->
	      % Pause before going to next solution
	      stop_clock(query),
	      pause("More"),
	      counter <- 0, timer.start, soldisp.clear
	  ; true
	  ),
	  fail % Next solution
	; % No more solutions, repeat again
          stop_clock(query),
          pause("Again"),
	  find_solutions(X, Goal)
	).

solution(X) :- soldisp.display(X), soldisp.nl.

stop_clock(bench) :- !,
        Elapsed = ~timer.end,
	Msg = "Finished in " + ~Elapsed.'$to_str' + " ms",
	Msg2 = Msg + " (repeated x" + ~repeat_count + ")",
	timerdisp.set(Msg2).
stop_clock(query) :- !,
        Elapsed = ~timer.end,
        Accum = ~timer.accum,
	( Elapsed = Accum ->
	    Msg = "Done in " + ~Elapsed.'$to_str' + " ms"
	; Msg = "Done in " + ~Elapsed.'$to_str' + " ms, " +
	        "accumulated " + ~Accum.'$to_str' + " ms"
	),
	timerdisp.set(Msg).

more_solutions :- wrk.resume.

benchmark_button := ~js_dom.elem("id_benchmark_button" + ~id).
reset_button := ~js_dom.elem("id_reset_button" + ~id).
more_button := ~js_dom.elem("id_more_button" + ~id).

write_html(Container) :-
	Div = ~js_dom.createElement("div"),
        Div.className <- "problembox",
        Div.id <- ~id,
        Div.innerHTML <-
  "<table width=\"100%\">\
  <tr>\
    <td>\
      <h2>" + ~title + "</h2>\
    </td>\
    <td align=\"right\" valign=\"top\">\
      <div id=\"sourceview" + ~id + "\"></div>\
    </td>\
  </tr>\
  </table>\
  <div id=\"source" + ~id + "\"></div>\
  <table width=\"100%\">\
  <tr>\
    <td>\
      <span>\
        <button type=\"button\" class=\"buttons\" name=\"benchmark_button" + ~id + "\" id=\"id_benchmark_button" + ~id + "\"></button>\
        <button type=\"button\" class=\"buttons\" name=\"more_button" + ~id + "\" id=\"id_more_button" + ~id + "\"></button>\
        <button type=\"button\" class=\"buttons\" name=\"reset_button" + ~id + "\" id=\"id_reset_button" + ~id + "\"></button>\
      </span>\
    </td>\
    <td align=\"right\" valign=\"top\">\
      <div id=\"time" + ~id + "\" class=\"timerbox\"></div>\
    </td>\
  </tr>\
  </table>\
  <div id=\"sol" + ~id + "\" class=\"solbox\" style=\"display: none\"></div>",
	Container.append_child(Div).

% ---------------------------------------------------------------------------
% A source picker

:- class sourcepick {
    :- attr srcname.
    :- attr sourcebox.
    :- attr button.

    % TODO: this Ajax and event_handler call could be implemented with freeze

    :- export(cons__/2).
    cons__(Name, SourceBox) :- ~srcname = Name, ~sourcebox = SourceBox.

    :- export(attach/1).
    attach(Container) :-
        Button = ~button,
	Button = ~js_dom.createElement("button"),
	Button.className <- "sourceview",
	set_button_action_view,
	Container.append_child(~button).

    button_transition(Name, Action) :-
        Button = ~button,
        Button.innerHTML <- Name,
        Button.onclick <- ~js_dom.event_handler(Action).

    set_button_action_view :-
        button_transition("View source", callback_view).

    callback_view :-
        js_dom.ajax_text(~srcname, Text, callback_got_source(Text)).

    callback_got_source(Text) :-
        set_button_action_hide,
        sourcebox.set_source(Text).

    set_button_action_hide :- !,
        button_transition("Hide source", callback_hide).

    callback_hide :-
        set_button_action_view,
        sourcebox.clean_source.
}.

:- class sourcebox {
    :- attr div.
    :- attr pre.

    :- export(attach/1).
    attach(Container) :-
	~div = ~js_dom.createElement("div"),
        div.className <- "sourcebox",
        div.style.display <- "none",
	~pre = ~js_dom.createElement("pre"),
	pre.className <- "sh_improlog",
        div.append_child(~pre),
	Container.append_child(~div).

    :- export(set_source/1).
    set_source(Text) :-
        Text2 = ~escape_text(Text),
        pre.innerHTML <- Text2,
%        sh_highlightElement(~pre, "improlog"),
        div.style.display <- "block".

    :- export(clean_source/0).
    clean_source :-
        div.style.display <- "none",
        pre.innerHTML <- "".

    :- use_package(js_lang).

    :- pred escape_text/2 :: t_string * t_string + (detfun, argsbox([unbox, unbox])).
    escape_text(Text) := ~js_lang.stats([
      vardecl('t', Text),
      't' <- 't'.replace.['/\\&/g', "&amp"],
      't' <- 't'.replace.['/\\</g', "&lt;"],
      't' <- 't'.replace.['/\\>/g', "&gt;"],
      't' <- 't'.replace.['/\\t/g', "        "],
      return('t')
    ]).

%    :- export(sh_highlightElement/2).
%    :- pred sh_highlightElement/2 :: t_string * t_string + (det, argsbox([unbox, unbox])).
%    sh_highlightElement(Elem, Lang) :- js_lang.stats([
%      sh_highlightElement.[Elem, sh_languages.elem(Lang)]
%    ]).
}.

% ---------------------------------------------------------------------------
% Buttons

set_button_status(B, enabled) :- !,
	B.disabled <- ~js_foreign.jscons.false__.
set_button_status(B, disabled) :- !,
	B.disabled <- ~js_foreign.jscons.true__.

