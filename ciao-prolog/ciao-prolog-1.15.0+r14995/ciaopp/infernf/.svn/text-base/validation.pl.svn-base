:- data('$already_validated$'/1).

% Validation of data:

validate_data(TAB, _, _, []) :-
	var(TAB), !.
validate_data([Entry|TAB], T, NewTAB, No_Analyzable_Preds) :-
	nonvar(Entry),
	get_nfentry_info(Entry, Pred, Clauses, _Type, _Test, _Nfail_flag,
	    _Cover_flag),
	Pred = F/A,
%% Commented out by PLG 3 Aug 99 (ICLP99 demo). 
%%        functor(Head, F, A),
%%        (is_a_builtin(Head) ->
%%             warning_message("Predicate ~q is a builtin and appears in the source program.", [F/A])
%%             ; true),
%% End of commented out by PLG 3 Aug 99 (ICLP99 demo). 
	validate_predicate(Entry, Res),
	validate_all_clauses(Clauses, T),
	( Res = clauses_but_no_mode_type_info
	->
	    NewTAB = TemTAB,
	    No_Analyzable_Preds = [Pred|R_No_Analyzable_Preds]
	;
	    NewTAB = [Entry|TemTAB],
	    No_Analyzable_Preds = R_No_Analyzable_Preds
	),
	asserta_fact('$already_validated$'(F/A)),
	validate_data(TAB, T, TemTAB, R_No_Analyzable_Preds).

%% validate_data(TAB, _, _):-
%%        var(TAB), !.
%% validate_data([Entry|TAB], T, NewTAB):-
%%        nonvar(Entry),
%%        Entry = st(F/A, Clauses, _Type, _Ann_clauses, _Test,
%%        _Nfail_flag, _Cover_flag),
%%        functor(Head, F, A),
%%        (is_a_builtin(Head) ->
%%            warning(['Attempt to redefine builtin ' , (F/A), ' -> Not redefined']),
%%            NewTAB = TemTAB
%%            ;
%%            validate_predicate(Entry),
%%            validate_all_clauses(Clauses, T),
%%            NewTAB = [Entry|TemTAB]
%%        ),
%%        asserta_fact('$already_validated$'(F/A)),
%%        validate_data(TAB, T, TemTAB).

validate_predicate(Entry, Res) :-
	get_nfentry_info(Entry, Pred, Clauses, Type, Test, Nfail_flag,
	    Cover_flag),
	Pred = F/A,
	( there_are_no_more_clauses(Clauses)
	->
	    Test = false,
	    set_flag_value_false(Nfail_flag),
	    set_flag_value_false(Cover_flag),
	    warning(['Predicate ', ~~(Pred),
		    ' not defined. Assumed that it fails.']),
	    ( nonvar(Type)
	    ->
		Res = no_clauses_but_mode_type_info,
		warning(['However, there is a modetype declaration for it: ',
			~~(Type)])
	    ;
		Res = no_clauses_no_mode_type_info,
		set_in_top_modetype(F, A, Type)
	    )
	;
	    ( var(Type)
	    ->
		Res = clauses_but_no_mode_type_info,
		set_in_top_modetype(F, A, Type)
% warning_message("There is no call mode/type info for predicate ~q, assumed: ~q", [Pred, Type]) 
	    ;
		Res = ok
	    )
	).

%% validate_predicate(OutEntry, (F/A)):-
%%    functor(Lit, F, A), % delete
%%    (known_test(Lit)
%%        -> validate_known_test(OutEntry, (F/A))
%%        ;
%%        (is_builtin_no_test(Lit) ->
%%              validate_builtin_no_test(OutEntry, (F/A))
%%              ;
%%              validate_user_defined(OutEntry, (F/A))
%%        )
%%    ).

validate_all_clauses(Clauses, _T) :-
	there_are_no_more_clauses(Clauses),
	!.
validate_all_clauses([Clause|CList], T) :-
	get_body_and_vars_of_clause(Clause, Body, ClVars),
	validate_body(Body, ClVars, T),
	validate_all_clauses(CList, T).

validate_body([],              _,      _TAB) :- !.
validate_body([Litinfo|LList], ClVars, TAB) :-
	get_literal(Litinfo, Literal),
	get_literal_key(Litinfo, PPKey),
	functor(Literal, F, A),
	( ('$already_validated$'(F/A) ; is_a_builtin(Literal, ClVars, PPKey))
	-> true
	; find_entry(TAB, F/A, OutEntry),
	    ( var(OutEntry) ->
		warning(['User predicate ', ~~(F/A),
			' not defined. Assumed that it fails.']),
		asserta_fact('$already_validated$'(F/A))
	    ;
		true )
	),
	validate_body(LList, ClVars, TAB).

%% validate_known_test(OutEntry, (F/A)):-
%%  (var(OutEntry) 
%%                -> true
%%                ;
%%                warning(['Attempt to redefine test ' , (F/A), ' -> Not redefined'])
%%            ).
%%          
%% validate_builtin_no_test(OutEntry, (F/A)):-
%%  (var(OutEntry) -> 
%%                true
%%                ;
%%                warning(['Attempt to redefine builtin ' , (F/A), ' -> Not redefined'])
%%              ).
