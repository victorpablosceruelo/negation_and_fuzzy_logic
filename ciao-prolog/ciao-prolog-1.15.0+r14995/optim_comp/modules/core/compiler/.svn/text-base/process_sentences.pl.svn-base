% This file is included in both the Prolog and imProlog reader

% TODO: should this be a class?

:- '$def_binder'(modread_ctx__from_stream(Stream),
	(process :: any <- true,
	 stream :: any <- Stream,
	 in :: m_any <- [])).
:- '$def_binder'(modread_ctx__from_list(Sentences),
	(process :: any <- true,
	 stream :: any <- _NoStream,
	 in :: m_any <- Sentences)).

% TODO: allow a flag to disable conditional code declarations

% TODO: improve conditional code at declaration level
% TODO: should semantics be equivalent at declaration level than at predicate level?
:- mixin modread_stream_ctx {
    % note: modread_ctx must be defined externally
    :- extends modread_ctx.
    :- fluid process :: any.
    :- fluid stream :: any. % TODO: have either 'stream' or 'in', not both
    :- fluid in :: accum.
    :- fluid errs :: errlog.
}.

{
:- extends modread_stream_ctx.
process_sentences :-
	fetch_sentence(Sentence), !,
	process_sentence(Sentence),
 	process_sentences.
process_sentences.

% Fetch a sentence from the 'in' stream (a list) or stream/1 (a file)
fetch_sentence(Sentence) :-
	( var(~stream) ->
	    in.add(Sentence) % TODO: This is not 'add' (but.. take?)
	; read_sentence(Sentence)
	).

process_sentence__then(ElseProcess) :-
	fetch_sentence(Sentence), !,
	Sentence = sentence(X, _, _, _, _),
	( X = (':-'(endif)) ->
	    true
	; X = (':-'(elif(Cond))) ->
	    call((
              process :: any <- ElseProcess,
	      process_sentence__if(Cond)
	    ))
	; X = (':-'(else)) ->
	    call((
              process :: any <- ElseProcess,
	      process_sentence__else
            ))
	; process_sentence(Sentence),
	  process_sentence__then(ElseProcess)
	).
process_sentence__then(_) :-
	process_sentences__error(['end of file or block found, expecting \':- endif.\' or \':- elif.\' or \':- else.\'']).

process_sentence__else :-
	fetch_sentence(Sentence), !,
	Sentence = sentence(X, _, _, _, _),
	( X = (':-'(endif)) ->
	    true
	; process_sentence(Sentence),
	  process_sentence__else
	).
process_sentence__else :-
	process_sentences__error(['end of file or block found, expecting \':- endif.\'']).
	    
process_sentence__if(Cond) :-
	( ~process = true ->
	    process_cond(Cond, CondS),
	    ( CondS = true ->
	        % enabled 'then', disabled 'else' 
	        Process = true, ElseProcess = false
	    ; CondS = fail ->
	        % disabled 'then', enabled 'else'
	        Process = false, ElseProcess = true
	    ; process_sentences__error(['cannot evaluate at compile time condition goal ', Cond])
	    )
	; % disabled 'then' and 'else'
          Process = false, ElseProcess = false
	),
	call((
          process :: any <- Process,
	  process_sentence__then(ElseProcess)
        )).

process_sentence(Sentence) :-
	Sentence = sentence(X, _, _, _, _),
	( X = ':-'(if(Cond)) ->
	    process_sentence__if(Cond)
	; ~process = true ->
	    treat_sentence(Sentence)
	; true
	).
}.

