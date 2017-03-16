:- module(pl2pubsbyyear, [pp_papers_all/3], 
	[assertions, dcg, fsyntax]).

:- use_module(bu_messages,  [note/2, warning/2, error/2, debug/2]).
:- use_module(bu_databases, [bibitem/2]).

:- use_module(bu_support, [nreverse/2]).
:- use_module(bu_support, [filter_year/2, filter_common/1]).
:- use_module(bu_support, [output_stats/3]).

:- use_module(library(aggregates), [findall/3]).

%:- use_module(bu_options, [get/2]).

:- use_module(pp_papers_term, [in_sections/5]).
:- use_module(bu_set, [add/2, collect/2]).

:- use_module(library(sort), [sort/2]).

old(1989).

pp_papers_all(Total) -->  
	{
	    bu_messages:note("Sort bibitem by by year", []),
	    %%% Get all papers %%%

	    bu_messages:debug("<byyear> Get all papers", []), 
	    findall(Keyword, 
                (
		    bu_databases:bibitem(Keyword,List),
                    bu_support:filter_common(List),
		    (member([year,Year], List) -> bu_set:add(pl2pubsbyyear, Year))
                ),
	    Papers),

            bu_set:collect(pl2pubsbyyear, Years_), sort(Years_,Years)
	}, !,
        [env(section(misc(ranking_explanation_title, [])), ranking_explanation)],
	{
	    nreverse(Years,RYears),
	    filter_years(RYears,FYears),
	    bu_messages:note("Papers found in database for years: ~w",[FYears])
	},
	pp_papers_years(FYears, Papers, 0, Total).
pp_papers_all(_Total) -->
	{bu_messages:warning("no papers found!",[])}.

filter_years([Year|_],[previous]) :- 
	Year =< ~old, !.
filter_years([],[]).
filter_years([Year|RY],[Year|NRY]) :-
	filter_years(RY,NRY).

% Years loop
pp_papers_years([],_Papers, X, X) --> [].
pp_papers_years([Y|RY], Papers, Accum, Result) --> 
	[env(section(Title), Content)], 
	{
	    (
		Y = previous ->
		Title = misc(year_upto_section_title, [author, number(~old)])
	    ;
		Title = misc(year_section_title, [author, number(Y)])
	    ),
            pp_papers_year(Y, Papers, Stats, Content, []),
	    bu_support:output_stats(Stats, Accum, NAccum)
	},
	pp_papers_years(RY, Papers, NAccum, Result).

% Pretty print papers for a given year
pp_papers_year(Year, Papers, Stats) -->
	{ bu_messages:note("Processing year ~w",Year)},
	{ bu_messages:debug("<byyear> Get all papers for year ~w", [Year])}, 
	{ findall(Keyword, 
	        (
		    member(Keyword, Papers), 
		    bu_databases:bibitem(Keyword,List),
		    (bu_support:filter_year(Year, List) -> true)
		), 
		FilteredPapers ) },
        { bu_messages:debug("<byyear> Formating  papers for year ~w in a term", [Year])}, 
	 (pp_papers_term:in_sections([level(1),
	                          level(2),
				  butype("book"),
				  butype("invited"),
				  butype("incollection"),
				  level(3),
				  butype("workshop"),
				  butype("techreport")],
				  FilteredPapers, Stats)).

