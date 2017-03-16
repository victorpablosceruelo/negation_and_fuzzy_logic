:- module(pp_papers_term,
 	[
 	 in_sections/5, 
 	 pretty_print/3, 
	 item_term/6, 
	 item_optional_term/6, 
	 special_info_term/5
 	],
	[assertions, dcg, fsyntax, hiord]).

:- use_module(library(bibutils(ranking(bu_ranking))), [ranks_doctree/4, get_acronym/2]).
% :- use_module(.(ranking(bu_ranking)), ...). does not work properly

:- use_module(bu_messages, [error/2, warning/2, note/2, debug/2]).
:- use_module(bu_support, [
	extract_papers_bytype/4,  
	get_page_number/5,
	exists_www_copy/3,
	add_url/2]).

:- use_module(bu_options, [get/2]).


in_sections([Type|T],Papers,[N|Stats]) -->	 
        { bu_messages:debug("<pp_papers_term> Extracting papers of type ~w", [Type])}, 
	{ bu_support:extract_papers_bytype(Type,Papers,RSPapers,N) }, 
	{ bu_messages:debug("<pp_papers_term> Formating (~w) papers of type ~w )", [N, Type])}, 
	pp_papers_in_a_section_doit(N,butype_description(Type),RSPapers), 
	in_sections(T,Papers, Stats).
in_sections([], _, []) --> [].

pp_papers_in_a_section_doit(0,_Description,_PaperList) --> [].
pp_papers_in_a_section_doit(N, Description,PaperList) -->
	{N > 0},
	[env(subsection(Description), [env(enumerate,Content)])],
 	{pp_papers_term(PaperList, 0, Content, [])}.

pp_papers_term([],  _Count) --> [].
pp_papers_term([H|T], Count) --> 
        { pp_paper_term(H, Content, []) }, !,
	[env(item,Content)],
	{ NCount is Count + 1 },
	pp_papers_term(T, NCount).

pp_papers_term([bibitem(_Y,_M,K,_L)|T], Count) -->  
	{bu_messages:error("could not format ~w.",[K])},
	pp_papers_term(T, Count). 

pp_paper_term(bibitem(_Year,_Month,KW,List)) -->
	{ bu_messages:debug("<pp_papers_term> Formatting paper '~w'", [KW])}, 
	comment_term(~(atom_codes(KW))),
        item_term(author,List, point, KW),
	item_term(title,List, point, KW),
	{member([butype,Type],List)},
	special_info_term(Type,List, KW),
	item_optional_term(publisher,List, comma, KW), 
        %% In these cases series already taken care of 
	(  {Type \== "manual",
	    Type \== "proceedings",
	    Type \== "editedbook",
	    Type \== "book",
	    Type \== "workshop",
	    Type \== "incollection",
	    Type \== "invited",
	    Type \== "inproceedings",
	    Type \== "techreport", 
	    Type \== "article"}
        -> item_optional_term(series,List, comma, KW)
        ;  [] ),	
        %% In these cases number already taken care of
	(  {Type \== "manual",
	    Type \== "proceedings",
	    Type \== "editedbook",
	    Type \== "book",
	    Type \== "workshop",
	    Type \== "incollection",
	    Type \== "invited",
	    Type \== "inproceedings",
	    Type \== "techreport", 
	    Type \== "article"}
        -> item_optional_term(number,List, comma, KW)
        ;  [] ),
	item_optional_term(edition,List, comma, KW), 
	(  {member([publisher,Publisher],List),
	    member([institution,Institution],List),
	    Publisher = Institution}
	-> []
	;  item_optional_term(institution,List, comma, KW)),	
	(  
	    {member([publisher,Publisher],List),
	     member([organization,Organization],List),
	     Publisher = Organization}
	-> []
	; 
	    item_optional_term(organization,List, comma, KW)
	),
	( {Type = "article"}
        -> item_optional_term(month,List, space, KW)
        ;  item_term(month,List, space, KW)),	
        item_term(year,List, point, KW),
        item_term(note,List, point, KW), 
	(
	    { bu_options:get(ranking, on),
	      bu_ranking:get_acronym(List, Acronym) }
	->
	    (
		{ Type = "inproceedings" } ->
		[nl], bu_ranking:ranks_doctree(Acronym, conf)
	    ;
		{ Type = "article" } ->
		[nl], bu_ranking:ranks_doctree(Acronym, jour)
	    ;
		[nl]
	    )
	; 
	    []
	).

% Format a field

item_optional_term(Item, List, Sep, KW) -->
	item_common_term(Item, List, KW),
	!, 
	[Sep].
item_optional_term(_Item, _List, _Sep, _KW) --> [].

item_term(Item, List, Sep, KW) -->
	item_common_term(Item,List, KW),
	!, 
	[Sep].
item_term(Item,_List, _Sep, KW) --> [],
	{bu_messages:warning("No ~a info in DB for ~a",[Item,KW])}.

item_common_term(pages, List, KW) -->
	{bu_support:get_page_number(KW, List,Number,In,En)},
	!,
	({In == []} -> 	
           ( {number(Number)} ->
	     [misc(pages, [number(Number)])]
	   ; {fail} )
	;  [misc(pages_fromto, [number(In),number(En)])]
	). 
item_common_term(title,List,Keyword) -->
	{member([title,Title],List)},
	!,
	(
	    {  member([url,MAINURL],List), AUXURL=[]; 
	       bu_support:exists_www_copy(Keyword,MAINURL,AUXURL) }
	-> 
	    [env(href(MAINURL), em(tex_string(Title)))]
	;
	    [em(tex_string(Title))]
	).
item_common_term(booktitle,List,_Keyword) -->
	{member([booktitle,Title],List)}, !,
	(
	    {  member([bookurl,URL],List) }
	->
	    [env(href(URL), tex_string(Title))]
        ; 
	    [tex_string(Title)]
	).
item_common_term(volume, List,  _Keyword) --> !,
	{member([volume, AList], List)}, !, 
	[ misc(volume_short, [tex_string(AList)]) ].
item_common_term(number, List,  _Keyword) --> !,
	{member([number, AList], List)}, !, 
	[ misc(number_short, [tex_string(AList)]) ].
item_common_term(month, List, _Keyword) --> !,
	{ member([month,AList],List) }, !,
	[ month(AList) ]. 
item_common_term(author, List, _Keyword) --> !,
	{ member([author,AList],List) }, !,
        people_term(AList).
item_common_term(editor, List, _Keyword) --> !,
	{ member([editor,AList],List) }, !,
        people_term(AList).
item_common_term(year, List, _Keyword) --> !, 
	{ member([year,AList],List) }, !,
	[ number(AList) ].
item_common_term(Item, List,  _Keyword) --> 
	{ member([Item,AList],List) }, !,
	[ tex_string(AList) ]. 

people_term([A]) --> !,
	possibly_add_person_url(A).
people_term([A|T]) -->
	possibly_add_person_url(A), 
	[comma],
	people_term(T).

possibly_add_person_url(A) -->
	(
	    {bu_support:add_url(A, URL)} ->
	    [env(href(URL), [tex_string(A)])]
	;
	    [tex_string(A)]
	).
	    

possibly_add_person_url(A) --> [A].

% Format specialized info

special_info_term("manual",List,Keyword) --> !,
	special_info_term("book",List,Keyword).
special_info_term("proceedings",List,Keyword) --> !,
	special_info_term("book",List,Keyword).
special_info_term("editedbook",List,Keyword) --> !,
	special_info_term("book",List,Keyword).
special_info_term("book",List,Keyword) --> !,
	item_optional_term(series,List, comma, Keyword),
	item_optional_term(volume,List, comma, Keyword), 
	item_optional_term(number,List, comma, Keyword),
	item_optional_term(pages,List, comma, Keyword).
special_info_term("workshop",List,Keyword) --> !,
	item_term(booktitle,List, comma, Keyword),
	item_optional_term(series,List, comma, Keyword),
	item_optional_term(volume,List, comma, Keyword),
	item_optional_term(number,List, comma, Keyword),
	item_optional_term(pages,List, comma, Keyword).

special_info_term("incollection",List,Keyword) --> !,
	special_info_term("inproceedings",List,Keyword).
special_info_term("invited",List,Keyword) --> !,
	special_info_term("inproceedings",List,Keyword).
special_info_term("invitedtalk",List,Keyword) --> !,
	special_info_term("inproceedings",List,Keyword).
special_info_term("inproceedings",List,Keyword) --> !,
	item_term(booktitle,List,comma, Keyword),
	item_optional_term(series,List,comma, Keyword),
	item_optional_term(volume,List,comma, Keyword),
	item_optional_term(number,List,comma, Keyword),
	item_term(pages,List,comma, Keyword).

special_info_term("techreport",List,Keyword) --> !,
	item_term(number,List,comma, Keyword),
	item_optional_term(pages,List,comma, Keyword).

special_info_term("article",List,Keyword) --> !,
	item_term(journal,List,comma, Keyword),
        ( 
	    {member([volume,_],List)} ->
	    item_term(volume,List,comma, Keyword),
	    item_term(number,List,comma, Keyword),
	    item_term(pages,List,comma, Keyword)
        ;
	    [misc(inpress, []), comma]
        ).
special_info_term("mastersthesis",List,Keyword) --> !,
	[misc(msthesis, [])], [comma],
        item_term(school,List, comma, Keyword),
	item_optional_term(address,List, comma, Keyword),
	item_optional_term(pages,List, comma, Keyword).
special_info_term("phdthesis",List,Keyword) --> !,
	[misc(phdthesis, [])], [comma], 
        item_term(school,List, comma, Keyword),
	item_optional_term(address,List, comma, Keyword),
	item_optional_term(pages,List, comma, Keyword).

special_info_term(Type,_List,Keyword)  --> 
{bu_messages:warning("Don\'t know how to format ~s in ~a",[Type,Keyword])}.

comment_term(String) --> [comment(String)].

%%% Pretty print %%%

:- use_module(library(format), [format/3]).

pretty_print([Head|Tail], N, Stream):-!,
	M is N + 2, 
	format(Stream, "[~n~*c", [M, 0' ]), 
	pretty_print(Head, M, Stream),
	pretty_print_tail(Tail, M, Stream), 
	format(Stream, "~n~*c]", [N, 0' ]).
pretty_print(env(Env, Content), N, Stream) :- !,
	M is N + 2, 
	format(Stream, "env(", []), 
	pretty_print(Env, M, Stream),
	format(Stream, ", ", []),
	pretty_print(Content, M, Stream),
	format(Stream, ")", []).
pretty_print(tex_string(S), _N, Stream):-!,
	format(Stream, "tex_string(\"~s\")", [S]).
pretty_print(string_esc(S), _N, Stream):-!,
	format(Stream, "string_esc(\"~s\")", [S]).
pretty_print(S, _N, Stream):-!,
	format(Stream, "~w", S).

pretty_print_tail([], _N, _Stream).
pretty_print_tail([Head|Tail], N, Stream):-
	format(Stream, ",~n~*c", [N, 0' ]),
	pretty_print(Head, N, Stream),
	pretty_print_tail(Tail, N, Stream).
	    
