:- module(dblp, [dblp_person/2, 
	         dblp_item/2, 
		 dblp_field/3,
		 stats/1
		 ], [dcg, assertions]).

:- doc( title,  "DBLP Interface").
:- doc( author, "Remy Haemmerle").
:- doc( version(0*1+0,2011/05/07), "First implementation.").

:- doc(module, "This module retrieves online bibliography data from
   DBLP @href{http://dblp.uni-trier.de/}. For efficiency reasons,
   requests are automatically cached. The module uses the following
   conventions: 
   @begin{itemize} 

   @item{} An @bf{author} is identified by an atom corresponding to to
   the DBLP full name where non ASCII characters are encoded by XML
   character reference.

   @item{} A @bf{publication key} is identified by an atom
   corresponding to the DBLP key.

   @end{itemize} ").

:- use_module(library(lists), [append/3]).
:- use_module(library(pillow(http)), [fetch_url/3]).
:- use_module(library(pillow(html)), [xml2terms/2]).

tokenize_name(Head, Curr, ":"||Head, "", Curr) --> {true}.
tokenize_name(Head, Curr, CurrTail, "_"||Curr, Res) --> " ", !, 
	tokenize_name(Head, Next, Next, CurrTail, Res).
tokenize_name(Head, Curr, [D|CurrTail], PreTail, Res) --> [C],
	{ member(C, "-&.;") -> D = 0'= ; D = C},
	tokenize_name(Head, Curr, CurrTail, PreTail, Res).
 
name_to_dblpID(Name, IDString):-
	tokenize_name(X, X, X, _, IDString, Name, []).


:- pred dblp_person(Name, Keys) : (atom(Name), string(DblpID), list(Keys, atom)) 
# "Unifies @var{Keys} with the list of the keys of all publications of
   author @var{Name}.".

dblp_person(Name, Keys):-
	cached_call(dblp_person_fact(Name, _Keys)), 
	Keys = _Keys.

:- pred dblp_item(Key, Fields) : (atom(Key), list(List)) 
# "Unifies @var{Fields} with the the list of all fields of publication
   @var{ID}.".

dblp_item(Key, List):-
	cached_call(dblp_item_fact(Key, _List, _Date)), 
	List = _List.

:- pred dblp_field(Key, Field, Value) : (atom(Key), atom(Field)) 
# "Unifies @var{Value} with the value of field @var{Field} of
   publication @var{Key}. If this field is not present, it unifies
   @var{Value} with the value of the field @var{Field} in the first
   cross-referenced publication.".

dblp_field(Id, Field, Value):-
	dblp_item(Id, List), 
	(
	    member([Field, _], List) ->
	    member([Field, Value], List)
	; 
	    member([crossref, Crossref], List), !,
	    dblp_item(Crossref, List2), 
	    member([Field, Value], List2)
	).
	
stats([[total_requests, R], [dblp_request, DR], [cached_requests, CR]]):-
	current_fact(dblp_requests(DR)), 
	current_fact(cached_requests(CR)), 
	R is DR + CR.

:- data dblp_person_fact/4.
:- data dblp_item_fact/3.
:- data db_non_updated/0.

states_on.
:- data dblp_requests/1.
:- data cached_requests/1.
dblp_requests(0).
cached_requests(0).

get_db(dblp_person_fact(Name, DblpId, Keys)):-
	atom_codes(Name, StringN),
	name_to_dblpID(StringN, DblpId),
	first_letter(DblpId, L),
	append(DblpId, "/xk", Tail),
	fetch_dblp("pers/" ||  [ L |  "/" || Tail], Term), 
	member(env(dblpperson, Attr, Content), Term), 
	member(name=_Name, Attr), 
	process_dblp_keys(Content,Keys).
get_db(dblp_item_fact(Id, List, MDate)):-
	atom_codes(Id, String), 
	append(String, ".xml", Path), 
	fetch_dblp("bibtex/" || Path, Term), 
	member(env(dblp, _, DblpContent), Term), 
	process_dblp_item(DblpContent, List, MDate).

cached_call(T) :- 
	current_fact(T), !,
	(
	    states_on ->
	    retract_fact(cached_requests(N)), 
	    M is N+1, 
	    assertz_fact(cached_requests(M))
	;
	    true
	).
cached_call(T) :- 
	retractall_fact(T),
	retractall_fact(db_non_updated),
	get_db(T), 
	assertz_fact(T).	
	
process_dblp_keys([], []).
process_dblp_keys(["\n"|T], S):-!,
	process_dblp_keys(T, S).
process_dblp_keys([env(dblpkey, _Attr, [String])| T], [dblpkey(Key)|S]):-
	atom_codes(Key, String), 
	process_dblp_keys(T, S).

process_dblp_item(Term, [[dblp_type, DblpType]|List], MDate):-
	member(env(DblpType, Attr, Content), Term), 
	member(mdate=String, Attr), 
	string_date(String, MDate), 
	process_dblp_fields(Content, List).

process_dblp_fields([], []).
%process_dblp_fields([end(Field, _, _) | T], S):-
%	member(Field, []), !,
%	process_dblp_fields(T, S).
process_dblp_fields([env(Field, _, [String])|T], [[Field, Value]|S]):- !, 
	(
	    member(Field, [crossref]) ->
	    atom_codes(Value, String)
	;
	    Value = String
	),
	process_dblp_fields(T, S).
process_dblp_fields([_|T], S):- !, 
	process_dblp_fields(T, S).

string_date(String1, Year/Month/Day):-
	append(YearS, "-" || String2,  String1), 
	append(MonthS, "-" || DayS, String2), 
	number_codes(Year, YearS), 
	number_codes(Month, MonthS), 
	number_codes(Day, DayS).
	
first_letter([L|_], M):-
	(
	    0'a @< L, L @< 0'z 
	->  
	    M = L 
	;
	    0'A @< L, L @< 0'Z 
	->  
	    M is L - 0'A + 0'a
	; 
	    throw(error(first_letter/1))
	).

:- use_module(library(write)).
fetch_dblp(Ref, Term):-
	(
	    states_on ->
	    retract_fact(dblp_requests(N)), 
	    M is N+1, 
	    assertz_fact(dblp_requests(M))
	;
	    true
	),
	write(http('dblp.uni-trier.de', 80, "/rec/" || Ref)), nl,
	fetch_url(http('dblp.uni-trier.de', 80, "/rec/" || Ref), [], Response), 
	member(content(String), Response), 
	xml2terms(String, Term).
