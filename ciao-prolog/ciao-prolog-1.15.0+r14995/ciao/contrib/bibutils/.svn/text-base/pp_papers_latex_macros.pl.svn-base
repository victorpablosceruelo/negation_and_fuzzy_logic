:- module(pp_papers_latex_macros, 
	[in_macros/4], 
	[assertions, dcg]).

:- use_module(pp_papers_term, 
	[item_term/6, item_optional_term/6, special_info_term/5]).

:- use_module(bu_doctree, [from_term/2]).
:- use_module(bu_doctree2tex, [doctree2tex/3]).
:- use_module(library(bibutils(ranking(bu_ranking))), [ranks_doctree/4, get_acronym/2]).
:- use_module(bu_support, [
	extract_papers_bytype/4,  
	get_page_number/5,
	exists_www_copy/3,
	add_url/2]).

:- use_module(bu_messages, [error/2, warning/2, note/2, debug/2]).

:- use_module(bu_options, [get/2]). 

in_macros([], _Papers) --> 
	"\\BU_ranking_explanation{",
        term2tex([ranking_explanation]),
	"}\n\n".
in_macros([Type|T], Papers) -->	 
	{bu_support:extract_papers_bytype(Type, Papers, RSPapers,_)},
	in_macros_(RSPapers, Type), 
	in_macros(T, Papers).

in_macros_([], _Type) --> [].
in_macros_([bibitem(_Year,_Month,KW,List)| Papers], Type) --> 
	{ bu_messages:debug("<in_macros> Formatting paper '~w'", [KW])}, 
	{atom_codes(KW, KWS)},	
	"\\BUpubli{", insert(KWS), "}\n", 
	"      {", 
	           {item_term(title,List, point, KW, DocStr1_, [])}, 
		    term2tex(DocStr1_),
              "}\n", 
	"      {", 
	           {item_term(author,List, point, KW, DocStr2_, [])}, 
		    term2tex(DocStr2_),
              "}\n", 	
	"      {", type2macro(Type), "}\n", 
        "      {", 	
	           {ranking(List, DocStr3_, [])}, 
		   term2tex(DocStr3_),
              "}\n", 
        "      {",   	
	           {extra(List, KW, DocStr4_, [])}, 
		   term2tex(DocStr4_),
              "}\n\n\n", 	
	in_macros_(Papers, Type).
   

extra(List, KW) -->
	{member([butype,Type],List)},
	special_info_term(Type,List, KW),
	item_optional_term(publisher,List, comma, KW), 
%        In these cases series already taken care of 
	(  
	    {
		Type \== "manual",
		Type \== "proceedings",
		Type \== "editedbook",
		Type \== "book",
		Type \== "workshop",
		Type \== "incollection",
		Type \== "invited",
		Type \== "inproceedings",
		Type \== "techreport", 
		Type \== "article"
	    } ->
	    item_optional_term(series,List, comma, KW)
	;  
	    []
	),	
        %% In these cases number already taken care of
	(
	    {
		Type \== "manual",
		Type \== "proceedings",
		Type \== "editedbook",
		Type \== "book",
		Type \== "workshop",
		Type \== "incollection",
		Type \== "invited",
		Type \== "inproceedings",
		Type \== "techreport", 
		Type \== "article"
	    } ->
	    item_optional_term(number,List, comma, KW)
        ;  
	    []
	),
	item_optional_term(edition,List, comma, KW), 
	( 
	    {
		member([publisher,Publisher],List),
		member([institution,Institution],List),
		Publisher = Institution
	    } -> []
	;  
	    item_optional_term(institution,List, comma, KW)
	),	
	(  
	    {
		member([publisher,Publisher],List),
		member([organization,Organization],List),
		Publisher = Organization
	    } ->
	    []
	; 
	    item_optional_term(organization,List, comma, KW)
	),
	( 
	   {Type = "article"} -> 
	   item_optional_term(month,List, space, KW)
        ;  
	    item_term(month,List, space, KW),	
	    item_term(year,List, space, KW),
	    item_term(note,List, space, KW)
	).

ranking(List) -->
	(
	    { bu_options:get(ranking, on),
	      bu_ranking:get_acronym(List, Acronym) }
	->
	    "\n",
	    (
		{Type = "inproceedings"}->
		bu_ranking:ranks_doctree(Acronym, conf)
	    ;
		{Type = "article"} ->
		bu_ranking:ranks_doctree(Acronym, jour)
	    ;
		[]
	    )
	; 
	    []
	).


insert([]) --> [].
insert([H|T]) --> [H], insert(T).

term2tex(X) -->
	{bu_doctree:from_term(X, DocTree)}, 
	bu_doctree2tex:doctree2tex(DocTree). 
	

type2macro(level(1)) -->
	"\\BuTypeFirstLevel ".
type2macro(level(2)) -->
	"\\BuTypeSecondLevel ".
type2macro(level(3)) -->
	"\\BuTypeThirdLevel ".
type2macro(butype(String)) -->
	"\\BuType", insert(String), " ".