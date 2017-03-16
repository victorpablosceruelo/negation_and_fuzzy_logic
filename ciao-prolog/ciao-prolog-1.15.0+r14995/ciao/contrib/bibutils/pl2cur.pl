:- module(pl2cur, [pp_papers_all/3], [assertions, dcg, fsyntax]).

:- use_module(bu_databases, [bibitem/2]).

:- use_module(pp_papers_term, [in_sections/5]).

%:- use_module(bu_options, [get/2]).
:- use_module(bu_messages, [error/2, warning/2, note/2, debug/2]).
:- use_module(bu_support).

:- use_module(library(aggregates), [findall/3]).

pp_papers_all(Total) -->
	pp_papers(Stats),
	{bu_support:output_stats(Stats, 0, Total) -> true ; true}.

pp_papers(Stats) -->
        {findall(Keyword, 
	      (
		  bu_databases:bibitem(Keyword,List),
		  (bu_support:filter_common(List) -> true)
	      ),
	      Papers)},

	[ranking_explanation],

        pp_papers_term:in_sections([level(1),
 	                       level(2),
                               butype("book"),
                               butype("invited"),
                               butype("incollection"),
                               level(3),
                               butype("workshop"),
                               butype("techreport")],
 	                      Papers, Stats).

