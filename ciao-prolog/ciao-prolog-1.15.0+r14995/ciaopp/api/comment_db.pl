:- module( comment_db,
	[ comment_db/3 
	, add_comment/3
	, get_comment/3
	, cleanup_comment_db/0
	],
	[ ] ).

:- use_package( ciaopp( 'api/api_types' ) ).

%% ---------------------------------------------------------------------------
:- pred comment_db( Where, Type, Comment )
: (t_acomm(Where), member(Type,[simple, double, c]),atom_or_str(Comment))

# "Each fact represents a comment. The text of the comment is
@var{Comment}, which can be an atom or an string. @var{Type} defines
the kind of comment. It can be:
@begin{enumerate}
@item simple: With only one %
@item double: With two %
@item c: like c-style /* */
@end{enumerate}

@var{Where} specifies where the comments goes. It can be at
@tt{begin}ing, @tt{end}, @tt{before(X)} or @tt{after(X)}, where X is a
Key from a clause or, in the future, a literal Key.".

:- data comment_db/3. 

%% ---------------------------------------------------------------------------

:- pred cleanup_comment_db # "Cleanups the database.".

cleanup_comment_db :-
	retractall_fact( comment_db(_,_,_) ).


:- pred add_comment( Where , Type , Comment )
	: (t_acomm(Where), 
	   member(Type,[simple, double, c]),
	   atom_or_str(Comment))
# "Add comment to the data base.".

add_comment( A , B , C ) :-
	assertz_fact( comment_db(A, B, C) ).


:- pred get_comment( Where , Type , Comment )
	: t_acomm(Where)
        => (member(Type,[simple, double, c]),
	    atom_or_str(Comment))
# "Retrives the comment corresponding to @var{Where}.".

get_comment( A , B , C ) :-
	current_fact( comment_db(A, B, C) ).

