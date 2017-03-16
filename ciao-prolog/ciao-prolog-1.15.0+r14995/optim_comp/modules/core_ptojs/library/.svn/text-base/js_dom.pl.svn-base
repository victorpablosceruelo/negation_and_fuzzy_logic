:- module(js_dom, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "DOM Bindings").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module provides an interface to the Document
   Object Model @href{http://www.w3.org/TR/DOM-Level-2-Core/introduction.html}").

:- use_module(engine(js_foreign)).

:- doc(bug, "Some definitions here are not part of the DOM").
:- doc(bug, "Bindings to the DOM are incomplete").

:- use_package(js_lang).

% Some important notes:
%
%   - According to the standard, if the innerHTML field is directly
%     modified, associated events will be lost. Use append_child
%     instead.

:- export(document/1).
:- pred document/1 :: js_object + (detfun, argsbox([unbox])).
document := ~js_lang.expr(document).

:- export(createElement/2).
:- pred createElement/2 :: t_string * js_object + (detfun, argsbox([unbox, unbox])).
createElement(Kind) := ~js_lang.expr(document.createElement.[Kind]).

:- export(createTextNode/2).
:- pred createTextNode/2 :: t_string * js_object + (detfun, argsbox([unbox, unbox])).
createTextNode(Text) := ~js_lang.expr(document.createTextNode.[Text]).

:- export(elem/2).
:- pred elem/2 :: t_string * js_object + (detfun, argsbox([unbox, unbox])).
elem(Id) := ~js_lang.expr(document.getElementById.[Id]).

:- export(set_title/1).
set_title(Text) :- document.title <- Text.

% An event handler that executes in the 'global_worker'
% TODO: when assigning a prolog object to a js variable it is necessary to know what is the expected type, and do a casting
:- export(event_handler/2).
:- pred event_handler/2 :: term * js_object + (detfun, argsbox([box, unbox])).
% TODO: use a queue?
event_handler(Event) := ~js_lang.expr(function([], [
  global_worker.reset.[], % TODO: wrong, resets the worker
  global_worker.solve.[Event]
])).

:- export(alert/1).
% Note: for internal program errors, consider use of 'console_log' or
%       'console_error' rather than 'alert'
:- pred alert/1 :: t_string + (det, argsbox([unbox])).
alert(String) :- js_lang.stats([ alert.[String] ]).

:- export(console_log/1).
:- pred console_log/1 :: t_string + (det, argsbox([unbox])).
console_log(String) :- js_lang.stats([ console.log.[String] ]).

:- export(console_error/1).
:- pred console_error/1 :: t_string + (det, argsbox([unbox])).
console_error(String) :- js_lang.stats([ console.error.[String] ]).

:- export(ajax_text/3).
ajax_text(Url, Text, Goal) :-
        Request = ~xml_http_request,
	request_open(Request, "GET", Url, ~js_foreign.jscons.true__),
        Request.onreadystatechange <- ~event_handler(on_ready_state_change(Request, Text, Goal)),
        request_send(Request, ~js_foreign.jscons.null).

on_ready_state_change(Request, Text, Goal) :-
        ( @(~Request.readyState) = 4 ->
            Text = @(~Request.responseText),
            Goal
        ; true
        ).

:- export(xml_http_request/1).
:- pred xml_http_request/1 :: js_object + (detfun, argsbox([unbox])).
xml_http_request := ~js_lang.expr(new('XMLHttpRequest', [])).

% TODO: create an object for requests
:- export(request_open/4).
:- pred request_open/4 :: js_object * t_string * t_string * js_object + (det, argsbox([unbox, unbox, unbox, unbox])).
request_open(Request, Method, Url, Async) :-
        js_lang.stats([ Request.open.[Method, Url, Async] ]).

% TODO: create an object for send
:- export(request_send/2).
:- pred request_send/2 :: js_object * js_object + (det, argsbox([unbox, unbox])).
request_send(Request, Body) :-
        js_lang.stats([ Request.send.[Body] ]).

:- export(load_css/1).
% Dynamically load a CSS
load_css(Url) :-
	Ln = ~createElement("link"),
	Ln.href <- Url,
	Ln.rel <- "stylesheet",
	Ln.type <- "text/css",
	(@(~document.body)).append_child(Ln).

:- export(load_js/1).
% Dynamically load a JavaScript source
% TODO: use ajax instead?
% TODO: this version is asynchronous, define a synchronous version
load_js(Url) :-
	Ln = ~createElement("script"),
	Ln.type <- "text/javascript",
	Ln.src <- Url,
	(@(~document.body)).append_child(Ln).

:- export(add_h1/2).
add_h1(Where, Title) :-
	H1 = ~createElement("h1"),
	TextNode = ~createTextNode(Title),
	H1.append_child(TextNode),
	Where.append_child(H1).

:- export(add_p/2).
add_p(Where, Text) :-
	H1 = ~createElement("p"),
	TextNode = ~createTextNode(Text),
	H1.append_child(TextNode),
	Where.append_child(H1).

:- export(add_div_with_class/3).
add_div_with_class(Where, Id, Class) :-
	Div = ~createElement("div"),
	Div.id <- Id,
	Div.className <- Class,
	Where.append_child(Div).

:- export(add_div/2).
add_div(Where, Id) :-
	Div = ~createElement("div"),
	Div.id <- Id,
	Where.append_child(Div).

:- export(add_br/1).
add_br(Where) :-
	Br = ~createElement("br"),
	Where.append_child(Br).
