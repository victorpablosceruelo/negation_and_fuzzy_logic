:- module(pl2macros, [pp_papers_all/3], [assertions, dcg, fsyntax]).

:- use_module(bu_databases, [bibitem/2]).

:- use_module(pp_papers_latex_macros, [in_macros/4]).

:- use_module(bu_messages, [error/2, warning/2, note/2, debug/2]).
:- use_module(bu_support).

:- use_module(library(aggregates), [findall/3]).

pp_papers_all(0) -->
        {findall(Keyword, 
	      (
		  bu_databases:bibitem(Keyword,List),
		  (bu_support:filter_common(List) -> true)
	      ),
	      Papers)},

        pp_papers_latex_macros:in_macros([level(1),
 	                       level(2),
                               butype("book"),
                               butype("invited"),
                               butype("incollection"),
                               level(3),
                               butype("workshop"),
                               butype("techreport")],
 	                      Papers).

