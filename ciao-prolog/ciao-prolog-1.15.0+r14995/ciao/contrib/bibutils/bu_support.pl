:- module(bu_support, _,
	% [fixname/2],
	[assertions, dcg, fsyntax]).

%% CIAO Specific
:- use_module(library(format), [format/3]).
:- use_module(library(dynamic),[assert/1]).

:- use_module(bu_databases, [url/3]).
:- use_module(bu_options, [get/2]).
:- use_module(bu_messages).

:- comment(module,"This library contains miscellaneous support
   predicates.").


%-----------------------------------------------------------------------------
:- pred sort_papers(+list,-list,+Type) # "Sort papers. @var{Type}
        determines which fields will be used for the comparison.".
%-----------------------------------------------------------------------------

sort_papers([],[],_Type).
sort_papers([X|L],R,Type) :-
	meta_partition(L,X,L1,L2,Type),
	sort_papers(L1,SL1,Type),
	sort_papers(L2,SL2,Type),
	append(SL1,[X|SL2],R).

meta_partition([],_,[],[],_Type).
meta_partition([X|L],Y,[X|L1],L2,Type) :-
	meta_test(Type,X,Y),
	!,
	meta_partition(L,Y,L1,L2,Type).
meta_partition([X|L],Y,L1,[X|L2],Type) :-
	meta_partition(L,Y,L1,L2,Type).

%-----------------------------------------------------------------------------
:- comment(meta_test/3, "Different comparisons of bib fields in
           papers.  Papers assumed to be in a standard format.").
%-----------------------------------------------------------------------------

:- pred meta_test(^(main_author),Paper1,Paper2) # "Main author of
        @var{Paper1} precedes alphabetically main author of
        @var{Paper2}.".

meta_test(main_author,X,Y) :-
	X=bibitem(_KeywordX,ListX),
	Y=bibitem(_KeywordY,ListY),
	member_check([author,[MainAX|_]],ListX),
	member_check([author,[MainAY|_]],ListY),
        delete_element(MainAX,T1X,0' ),
        delete_element(MainAY,T1Y,0' ),
        delete_element(T1X,T2X,0'{),
        delete_element(T1Y,T2Y,0'{),
        delete_element(T2X,T3X,0'}),
        delete_element(T2Y,T3Y,0'}),
        % Get rid of initials... (up to 3 >different< ones -- 
        % here is where a regexp would be better)
        replace_list(T3X,T4X,[_,0'.],""),
        replace_list(T4X,T5X,[_,0'.],""),
        replace_list(T5X,AuthXS,[_,0'.],""),
        replace_list(T3Y,T4Y,[_,0'.],""),
        replace_list(T4Y,T5Y,[_,0'.],""),
        replace_list(T5Y,AuthYS,[_,0'.],""),
	% format(user_error,'comparing ~s and ~s ~n',[AuthXS,AuthYS]),
        name(AuthX,AuthXS),
        name(AuthY,AuthYS),
	!,
        AuthX @< AuthY.
	% format(user_error,"TEST SUCCEEDED~n",[]), ttyflush.

:- pred meta_test(^(year_month),Paper1,Paper2) # "Publication date of
        @var{Paper1} precedes that of @var{Paper2}.".

meta_test(year_month,X,Y) :-
	X=bibitem(YearX,MonthX,_,_),
	Y=bibitem(YearY,MonthY,_,_),
	(
	    YearX < YearY ->
	    true
	;
	    YearX =:= YearY,
	    MonthX < MonthY
	).

%-----------------------------------------------------------------------------
% URL database access / handling
%-----------------------------------------------------------------------------

add_url(Author,URLAuthor) :-
	fix_name(Author,PlainName),
	find_url(Author,PlainName,URLAuthor).

:- dynamic url_not_found_reported/1.

find_url(_Author,PlainName,RURL) :-
	bu_databases:url(PlainName,HTTP,_EMAIL),
	HTTP \== "",
	!,
	RURL = HTTP.
find_url(Author,PlainName,Author) :-
	url_not_found_reported(PlainName), %% Error already reported...
	!, 
	fail.
find_url(Author,PlainName,Author) :-
	assert(url_not_found_reported(PlainName)),
	bu_messages:warning("URL not found for ~s (~s)", [Author, PlainName]), 
	fail.

fix_name(Name,PlainName) :- 
	delete_elements(Name,PlainName,"\\~'{} -,():"|| [10,9]).

:- pred exists_www_copy(Keyword,MAINURL,AUXURL) :: atm * string * string

   # "Checks for the existence of a downloadable (.pdf) file with the
      name @var{Keyword}.pdf (or also @var{Keyword}.ps or
      @var{Keyword}.ps.gz) in a public directory, defined as option
      @var{pdf_dir} and @var{pdf_url}.2. If a good quality .pdf is available, it
      returns its url in @var{MAINURL} and [] in @var{AUXURL}. If only
      a .ps or .ps.gz is available, it returns its url in
      @var{MAINURL} and [] in @var{AUXURL}. Finally, if only a bad
      quality .pdf is available (flagged by the fact that the file is
      called @var{Keyword}_bitmap.pdf) but @var{Keyword}.ps or
      @var{Keyword}.ps.gz are also available then
      @var{Keyword}_bitmap.pdf is returned in @var{MAINURL} and
      @var{Keyword}.ps or @var{Keyword}.ps.gz is returned also in
      @var{AUXURL}.".

exists_www_copy(Keyword,MAINURL,AUXURL) :- 
	Dir = ~atom_codes(~(bu_options:get(pdf_dir))),
	DirURL = ~atom_codes(~(bu_options:get(pdf_url))),
	name(Keyword,KeywordS),
	append("/", KeywordS, L),	
	append(Dir, L,FileBase),
	append(DirURL,L,URLBase),
	nofileerrors,
	handle_www_copy_cases(Keyword,FileBase,URLBase,MAINURL,AUXURL),
	fileerrors.
exists_www_copy(Keyword,_MAINURL,_AUXURL) :-
	fileerrors,
	bu_messages:warning("No pdf (nor ps) file found for ~w.",[Keyword]), 
	fail.

handle_www_copy_cases(_Keyword,FileBase,URLBase,MAINURL,[]) :-
	can_be_opened(FileBase,URLBase,".pdf",MAINURL),
        !.
handle_www_copy_cases(Keyword,FileBase,URLBase,MAINURL,AUXURL) :-
	can_be_opened(FileBase,URLBase,"_bitmap.pdf",MAINURL),
	can_be_opened(FileBase,URLBase,".ps",AUXURL),
	bu_messages:note("Only bitmap.pdf (and .ps) file found for ~w",[Keyword]),
        !.
handle_www_copy_cases(Keyword,FileBase,URLBase,MAINURL,AUXURL) :-
	can_be_opened(FileBase,URLBase,"_bitmap.pdf",MAINURL),
	can_be_opened(FileBase,URLBase,".ps.gz",AUXURL),
	bu_messages:note("Only bitmap.pdf (and .ps.gz) file found for ~w",[Keyword]),
        !.
handle_www_copy_cases(Keyword,FileBase,URLBase,MAINURL,[]) :-
	can_be_opened(FileBase,URLBase,"_bitmap.pdf",MAINURL),
	bu_messages:warning("Only _bitmap.pdf file found for ~w",[Keyword]),
        !.
handle_www_copy_cases(Keyword,FileBase,URLBase,MAINURL,[]) :-
	can_be_opened(FileBase,URLBase,".ps",MAINURL),
	bu_messages:note("Only .ps file found for ~w.",[Keyword]),
        !.
handle_www_copy_cases(Keyword,FileBase,URLBase,MAINURL,[]) :-
	can_be_opened(FileBase,URLBase,".ps.gz",MAINURL),
	!,
	bu_messages:note("Only .ps.gz file found for ~w.",[Keyword]).

can_be_opened(FileBase,URLBase,Suffix,URL) :-
	append(FileBase,Suffix,FileNameS),
	name(FileName,FileNameS),
	open(FileName,read,Stream),
	close(Stream),
	append(URLBase,Suffix,URL).
	

%-----------------------------------------------------------------------------
% get beginpage and endpage and/or number of pages
%-----------------------------------------------------------------------------

get_page_number(_,List,Number,Init,End) :-
	member_check([beginpage,Init],List), !,
	member_check([endpage,End],List),
	Number is (End - Init) + 1.
get_page_number(_,List,Number,[],[]) :-
	member_check([npages,Number],List), !.
get_page_number(K, _L, 'N/A', [], []):-
	bu_messages:warning("Page number not available on entry ~w",[K]).


%-----------------------------------------------------------------------------
% String x List of Strings
%-----------------------------------------------------------------------------

is_an_author(Name,[String|_Rest]) :-
%%	sublist(Name,String), %% Wrong!
	list_contained(Name,String),
	!.
is_an_author(Name,[_String|Rest]) :-
	is_an_author(Name,Rest).

%-----------------------------------------------------------------------------
% Miscellaneous other support predicates
%-----------------------------------------------------------------------------

% InputList x OutputList x InputSubListA x InputSublistB
% OutputList is InputList with InputSublistA replaced by InputSublistB
% (ideally, we should add regexps)
replace_list([],[],_,_).
replace_list(IL,OL,X,Y) :-
	X = [_|_],
        prefix_rest(X,IL,RIL),
	!,
	replace_list(RIL,NOL,X,Y),
        append(Y,NOL,OL).
replace_list([Z|L],[Z|NL],X,Y) :-
        replace_list(L,NL,X,Y).

% InputPrefix x InputList x List
% InputPrefix is a prefix of InputList and List is the rest.
prefix_rest([], R, R).
prefix_rest([X|PT], [X|T], R) :-
	prefix_rest(PT, T, R).

% ... Now, several specializations of the above...

% InputList x OutputList x Char
delete_element([],[],_).
delete_element([X|L],NL,X) :- 
	!,
	delete_element(L,NL,X).
delete_element([Y|L],[Y|NL],X) :- 
	delete_element(L,NL,X).

% InputList x OutputList x Char
delete_elements([],[],_).
delete_elements([X|T],NT,L) :-
	member(X, L),
	!,
	delete_elements(T,NT,L).
delete_elements([Y|T],[Y|NT],L) :- 
	delete_elements(T,NT,L).

% InputList x OutputList x Char x List
replace_element([],[],_,_).
replace_element([X|L],R,NX,Y) :- 
	X==NX,
	!,
	replace_element(L,NL,X,Y),
	append(Y,NL,R).
replace_element([Z|L],[Z|NL],X,Y) :- 
	replace_element(L,NL,X,Y).

% ISO escaping...
% *** change to use replace element
prolog_escape([],[]).
prolog_escape([0'\\|T],[0'\\,0'\\|NT]) :- 
	!,
	prolog_escape(T,NT).
prolog_escape([0'"|T],[0'\\,0'"|NT]) :- 
	!,
	prolog_escape(T,NT).
prolog_escape([Y|L],[Y|NL]) :- 
	prolog_escape(L,NL).

delete_spaces([],[]).
delete_spaces([9|R],RD) :-
	!,
	delete_spaces(R,RD).
delete_spaces([10|R],RD) :-
	!,
	delete_spaces(R,RD).
delete_spaces([32|R],RD) :-
	!,
	delete_spaces(R,RD).
delete_spaces([X|R],[X|RD]) :-
	!,
	delete_spaces(R,RD).

% 32 is space
delete_extra_spaces([],[]).
delete_extra_spaces([32,32|T],NT) :-
	!,
	delete_extra_spaces([32|T],NT).
delete_extra_spaces([X|T],[X|NT]) :-
	delete_extra_spaces(T,NT).

list_contained(List1, List2) :-
	append(X,_,List2),
	append(_,List1,X).

sublist(List, List).
sublist(Sub, [Head|Tail]) :- sublist_(Tail, Head, Sub).

sublist_(Sub, _, Sub).
sublist_([Head|Tail], _, Sub) :- sublist_(Tail, Head, Sub).
sublist_([Head|Tail], X, [X|Sub]) :- sublist_(Tail, Head, Sub).

nreverse([],[]).
nreverse([X|L0],L) :- 
	nreverse(L0,L1), 
	append(L1,[X],L).

%% % Now built into most systems (and Ciao).
append([],L,L).
append([X|L1],L2,[X|L3]) :- 
	append(L1,L2,L3).

% Watch out: member_check, really!
member_check(X,[X|_]) :- !.
member_check(X,[_|T]) :- member_check(X,T).

% write_strlist([],_Separator,End,Stream) :-
% 	format(Stream,"~s",End).
% write_strlist([Y|RY],Separator,End,Stream) :-
% 	format(Stream,"~s~s",[Y,Separator]),
% 	write_strlist(RY,Separator,End,Stream).

to_lower_case([],[]).
to_lower_case([H|T],[NH|NT]) :-
	is_upper_case(H),
	!,
	NH is H+32,
	to_lower_case(T,NT).
to_lower_case([H|T],[H|NT]) :-
	to_lower_case(T,NT).

add_capital_bracing([],[]).
add_capital_bracing([C|T],[0'{,C|NT]) :-
	is_upper_case(C),
	!,
	continue_uppercase(T,NT).
add_capital_bracing([C|T],[C|NT]) :-
	add_capital_bracing(T,NT).


continue_uppercase([],[0'}]).
continue_uppercase([C|T],[125,C|NT]) :-
	is_lower_case(C),
	!,
	add_capital_bracing(T,NT).
continue_uppercase([C|T],[C|NT]) :-
	continue_uppercase(T,NT).

is_upper_case(Ch) :-
	Ch >= 65, Ch =< 90.

is_lower_case(Ch) :-
	Ch >= 97, Ch =< 122.

%----------------------------------------------------------------------

:- use_module(library(aggregates), [findall/3]). 
:- use_module(library(ttyout)).
:- use_module(library(lists),[length/2]).
:- use_module(bu_databases, [bibitem/2]).


% :- use_module(.(ranking(bu_ranking)), ...). does not work properly
:- use_module(library(bibutils(ranking(bu_ranking))), [
	    get_acronym/2,
	    publication_type/2,
	    ranks/3]). 

% Select all papers of a given type for a given topic and author
extract_papers_bytype(Type,Keywords,RSPapers,N) :- 
	findall(bibitem(Year,Month,Keyword,List),
 	       (
		 member(Keyword, Keywords),
		 bu_databases:bibitem(Keyword,List),
		 (filter_type(Type, List) -> true),
		 (member([year,Year],List) -> true),
	         (member([month,Month],List) -> true;Month = 0)
              ),
              Papers),
	!,
        nreverse(Papers,RPapers),  % Todo : optimize
	sort_papers(RPapers,SPapers,year_month),
        nreverse(SPapers,RSPapers),
	length(RSPapers,N).

filter_author(Author, List):-
	    member([author,Authors],List),
	    is_an_author(Author,Authors).
		 

filter_type(level(ILevel), List) :- !,
	       member([butype,Type],List),
	       bu_ranking:get_acronym(List, Acronym),
	       bu_ranking:ranks(Acronym, Type, Level),
	       ILevel=Level.
filter_type(butype(IType), List) :- !,
	       member([butype,Type],List),
               bu_ranking:publication_type(Type,IType).

filter_topic(Topic, List):-
	member([butopics,TopicList],List),
	member(Topic,TopicList).
 
filter_year(previous, List):-!,
	member([year,Year],List), Year =< 1989.
filter_year(Year, List):-
	member([year,Y],List), 
	(
	    Year = [YearMin, YearMax] ->
	    YearMin =< Y, Y =< YearMax
	;
	    Year = Y
	).

filter_project(Project, List):-
	member([projects,ProjectList],List),
	member(Project,ProjectList).
 

filter_common(List):-
	(filter_author(~(bu_options:get(author)), List) -> true),
	(member([butype, _], List) -> true),
	(filter_year([~(bu_options:get(year_min)), ~(bu_options:get(year_max))],
	             List) -> true).
 
prepare_output(I, Format, O, Stream):-
	open(O, write, Stream),
	bu_messages:note("Writing ~w...", [O]),
	comment_tags(Format, InTag, OutTag),
	format(Stream, "~s----------------------------------------------------~s~n",[InTag, OutTag]),
	format(Stream, "~sWARNING: Do not edit this file~s~n",[InTag, OutTag]),
	format(Stream, "~sIt has been generated automatically from file ~w~s~n",[InTag, I, OutTag]),
	format(Stream, "~s----------------------------------------------------~s~n",[InTag, OutTag]),
	ttyflush.

comment_tags(pl, "%%", "").
comment_tags(tex, "%%", "").
comment_tags(html, "<!-- ", " -->").
	

exists_article_with_author_and_topic(Author,Topic) :- 
	bu_databases:bibitem(_Keyword,List),
	(filter_author(Author, List) -> true), 
	filter_topic(Topic, List), !.

output_stats([N1,N2,N3,NB,NI,NC,NW,NT], Accum, NAccum):-
	N is  N1+N2+N3+NB+NC+NT+NI+NW,
	NAccum is Accum + N,
     	LTot is N1 + N2 + N3,
	( 
	    LTot = 0 -> 
	    N1P = 0, N2P = 0, N3P = 0
	; 
	    N1P is N1 / LTot * 100, N2P is N2 / LTot * 100, N3P is N3 / LTot * 100
	),
     	bu_messages:note("Done. ~w entries processed: L1=~d L2=~d L3=~d B=~d I=~d IB=~d W=~d TR=~d\t(L1=~d\%   L2=~d\%   L3=~d\%)",[N, N1,N2,N3,NB,NI,NC,NW,NT,N1P,N2P,N3P]).


:- data counter/2.

:- pred incr_counter(Name, Value) : atom(Name) => integer(Value).
incr_counter(Name, Value):-
	(
	    retract_fact(counter(Name, Value)) ->
	    true
	;
	    Value = 0 
	), 
	NewValue is Value + 1, 
	assertz_fact(counter(Name, NewValue)).



:- use_module(library(prolog_sys), [statistics/2]).


output_usertime:-
	prolog_sys:statistics(usertime, [_, Time]), 
	message(0, "[UserTime ~w]~n", [Time]), 
	prolog_sys:statistics(usertime, _).



:- meta_predicate mytrace(:).
mytrace(A):-
	debug("Trace: call (~w)", [A]), 
	call(A), 
	(
	    debug("Trace: exit (~w)", [A]); 
	    debug("Trace: redo (~w)", [A]), fail
	).
mytrace(A):- 
	debug("Trace: fail (~w)", [A]), fail.

