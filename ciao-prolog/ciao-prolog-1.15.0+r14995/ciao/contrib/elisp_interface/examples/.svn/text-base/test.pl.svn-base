:- module(test, [], [dcg, fsyntax, assertions, regtypes, isomodes]).

:- doc(title, "Testing elisp Interface").
:- doc(author, "Jose F. Morales").

% Usage: 
% - load ciao_elisp_mode.el from Emacs
% - open a new buffer
% - M-x ciao-elisp-start

:- use_module(library(lists), [append/3]).
:- use_module(library(elisp_interface)).

:- export(main/0).
main :-
	elisp_init,
	% Define our own function
	elisp_defun('insert-with-face'(face, msg), [
          comment("Insert msg with a given face"),
	  setq(p0, call(point)),
	  insert(msg),
	  setq(p1, call(point)),
	  'add-text-properties'(p0, p1, face)
        ]),
	elisp_defun('insert-with-face-pointmax'(face, msg), [
          'save-excursion'(
            'goto-char'(call('point-max')),
	    'insert-with-face'(face, msg),
	    insert(string("\n")))]),
	% Arith from elisp
	arith,
	%
	% Define some buttons
	layout,
%	hello_world,
	% Enter the event loop,
	event_loop.

layout :-
	% Define the button text
	elisp_defun('bhandler'(button), [
          'ciao-elisp-event'('button-label'(button))
        ]),
	% Define the button type
	elisp_eval('define-button-type'(
          quote('my-button'),
	  quote('action'), quote('bhandler'),
	  quote('face'), quote('underline'),
	  quote('mouse-face'), quote('underline'),
	  quote('follow-link'), t,
	  quote('help-echo'), string("Click button"))),
	% Insert the button
        elisp_eval('insert-text-button'(string("Smaller"), ':type', quote('my-button'))),
	elisp_insert(" "),
        elisp_eval('insert-text-button'(string("Bigger"), ':type', quote('my-button'))),
	elisp_insert("\n"),
	elisp_insert("\n"),
	elisp_eval('defvar'(mymarker1, call('make-marker'))),
	elisp_eval('defvar'(mymarker2, call('make-marker'))),
	elisp_insert("Text: "),
	elisp_eval('set-marker'(mymarker1, call('point'))),
	elisp_insert("(none)"),
	elisp_insert(" "),
	elisp_eval('set-marker'(mymarker2, call('point'))).

arith :-
	elisp_insert("Arithmetic test:\n"),
	R = ~elisp_eval(3 + 4),
	elisp_insert(R), elisp_insert("\n").

% hello_world :-
% 	elisp_eval('set-frame-parameter'(nil, quote('cursor-type'), nil)),
% 	%
% 	elisp_insert("Hello World\n"),
% 	%
% 	( faces(_, Face),
% 	    elisp_eval('insert-with-face-pointmax'(quote(face(Face)), string("Hello World"))),
% 	    fail
% 	; true
% 	),
% 	%
% 	elisp_eval('set-frame-parameter'(nil, quote('cursor-type'), quote('box'))).

faces(5, 'ciao-face-sectioning-0-face').
faces(4, 'ciao-face-sectioning-1-face').
faces(3, 'ciao-face-sectioning-2-face').
faces(2, 'ciao-face-sectioning-3-face').
faces(1, 'ciao-face-sectioning-4-face').
faces(0, 'ciao-face-sectioning-5-face').

%:- export(elisp_insert/1).
:- pred elisp_insert(+Msg) :: string # "Invokes @tt{(insert Msg)}".
elisp_insert(Msg) :-
	elisp_eval(insert(string(Msg))).

% ---------------------------------------------------------------------------

event_loop :-
	Event = ~elisp_event_wait,
	( Event = "\"Smaller\"" ->
	    smaller
	; Event = "\"Bigger\"" ->
	    bigger
	; elisp_eval('save-excursion'(
            'goto-char'(call('point-max')),
	    insert(string("\n")),
            insert(string(~append(Event, "\n")))))
	),
	event_loop.

:- data level/1.
set_level(N) :- retractall_fact(level(_)), assertz_fact(level(N)).
get_level(N) :- ( level(N0) -> N = N0 ; N = 0 ).

smaller :-
	get_level(N), N1 is N - 1,
	( N1 < 0 -> N2 = 0 ; N2 = N1 ),
	set_level(N2),
	show(N2).

bigger :-
	get_level(N), N1 is N + 1,
	( N1 > 5 -> N2 = 5 ; N2 = N1 ),
	set_level(N2),
	show(N2).

show(N) :-
	faces(N, Face),
	elisp_eval('save-excursion'(
          'goto-char'(mymarker1),
          'delete-region'(mymarker1, (mymarker2 - 1)),
          'insert-with-face'(quote(face(Face)), string("Hello World")))).
%	elisp_insert(" - level="),
%	elisp_insert(~number_codes(N)),
%	elisp_insert("\n").
