% TODO: merge with process_sentence.pl! (I need aliases or interfaces for that...)

% (a copy of process_sentences.pl where process_sentence is renamed to f_process_sentence
%  and modread_stream_ctx contains other variables)

:- mixin f_modread_stream_ctx {
    % note: f_modread_ctx must be defined externally
    :- extends f_modread_ctx.
    :- fluid process :: any.
    % :- fluid stream :: any.
    :- fluid in :: accum.
    :- fluid out :: accum. % new
    % :- fluid errs :: errlog.
}.

{
:- extends f_modread_stream_ctx.
f_process_sentences :-
	f_fetch_sentence(Sentence), !,
	f_process_sentence(Sentence),
 	f_process_sentences.
f_process_sentences.

% Fetch a sentence from the 'in' stream
:- fluid in :: accum.
f_fetch_sentence(Sentence) :-
	in.add(Sentence). % TODO: This is not 'add' (but.. take?)

f_process_sentence__then(ElseProcess) :-
	f_fetch_sentence(Sentence), !,
	Sentence = sentence(X, _, _, _, _),
	( X = (':-'(endif)) ->
	    true
	; X = (':-'(elif(Cond))) ->
	    call((
              process :: any <- ElseProcess,
	      f_process_sentence__if(Cond)
	    ))
	; X = (':-'(else)) ->
	    call((
              process :: any <- ElseProcess,
	      f_process_sentence__else
	    ))
	; f_process_sentence(Sentence),
	  f_process_sentence__then(ElseProcess)
	).
f_process_sentence__then(_) :-
	f_process_sentences__error(['end of file or block found, expecting \':- endif.\' or \':- elif.\' or \':- else.\'']).

f_process_sentence__else :-
	f_fetch_sentence(Sentence), !,
	Sentence = sentence(X, _, _, _, _),
	( X = (':-'(endif)) ->
	    true
	; f_process_sentence(Sentence),
	  f_process_sentence__else
	).
f_process_sentence__else :-
	f_process_sentences__error(['end of file or block found, expecting \':- endif.\'']).
	    
f_process_sentence__if(Cond) :-
	( ~process = true ->
	    process_cond(Cond, CondS),
	    ( CondS = true ->
	        % enabled 'then', disabled 'else' 
	        Process = true, ElseProcess = false
	    ; CondS = fail ->
	        % disabled 'then', enabled 'else'
	        Process = false, ElseProcess = true
	    ; f_process_sentences__error(['cannot evaluate at compile time condition goal ', Cond])
	    )
	; % disabled 'then' and 'else'
          Process = false, ElseProcess = false
	),
	call((
          process :: any <- Process,
	  f_process_sentence__then(ElseProcess)
	)).

f_process_sentence(Sentence) :-
	Sentence = sentence(X, _, _, _, _),
	( X = ':-'(if(Cond)) ->
	   f_process_sentence__if(Cond)
	; ~process = true ->
	    f_treat_sentence(Sentence)
	; true
	).
}.

