:- use_module(library(arithpreds)).

:- class sourcepick {
    :- export(srcname/1).
    :- attr srcname.
    :- export(sourcebox/1).
    :- attr sourcebox.

    :- export(cons__/2).
    cons__(Name, SourceBox) :- ~srcname = Name, ~sourcebox = SourceBox.

    :- export(attach/1).
    attach(Container) :-
	Header = ~js_dom.createElement("a"),
        Header.href <- "#",
        Header.innerHTML <- "" + ~srcname + "<br/>",
        Header.onclick <- ~js_dom.event_handler(
          js_dom.ajax_text(~srcname, Text, got_source(Text))
        ),
	Container.append_child(Header).

    got_source(Text) :-
        sourcebox.set_source(Text).
}.

:- class sourcebox {
    :- export(div/1).
    :- attr div.
    :- export(pre/1).
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
        sh_highlightElement(~pre, "improlog"),
        div.style.display <- "block".

    :- pred escape_text/2 :: t_string * t_string + (detfun, argsbox([unbox, unbox])).
    escape_text(Text) := ~js_lang.stats([
      vardecl('t', Text),
      't' <- 't'.replace.['/\\&/g', "&amp"],
      't' <- 't'.replace.['/\\</g', "&lt;"],
      't' <- 't'.replace.['/\\>/g', "&gt;"],
      't' <- 't'.replace.['/\\t/g', "        "],
      return('t')
    ]).

    :- export(sh_highlightElement/2).
    :- pred sh_highlightElement/2 :: t_string * t_string + (det, argsbox([unbox, unbox])).
    sh_highlightElement(Elem, Lang) :- js_lang.stats([
      sh_highlightElement.[Elem, sh_languages.elem(Lang)]
    ]).
}.

