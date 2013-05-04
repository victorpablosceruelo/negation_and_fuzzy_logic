:- module(pl2pubsbyproject, [pp_papers_all/3],
	[ciaopaths, assertions, dcg, fsyntax]).

:- use_module(library(aggregates), [findall/3, setof/3, (^)/2]).

:- use_module(bu_databases, [bibitem/2]).
:- use_module(bu_options, [get/2]).
:- use_module(bu_messages, [error/2, warning/2, note/2]).

:- use_module(bu_support).
:- use_module(pp_papers_term).

pp_papers_all(Total) -->
	{
	    bu_options:get(project, PName),
	    
	    %%% Get all papers %%%
	    findall(Keyword, 
	      (
		  bu_databases:bibitem(Keyword, List),
		  (bu_support:filter_project(_, List) -> true),
		  (bu_support:filter_common(List) -> true)
	      ),
	      Papers)
	},
	(
	    {var(PName)} ->
            {
		setof(Project,
		  Keyword^List^( 
		      member(Keyword, Papers),				
                      bu_databases:bibitem(Keyword,List),
		      bu_support:filter_project(Project,List)
                  ),
		  Projects),

                bu_messages:note("Projects found in papers database: ~w", [Projects])
            },
 	    [env(section(misc(ranking_explanation_title, [])), ranking_explanation)],
	    pp_papers_projects(Projects,Papers,0,Total)
	;
	    { bu_messages:note("Processing project ~w",[PName])},
            [env(section(misc(project_section_title, [string_esc(~atom_codes(PName))])), 
	                      [ranking_explanation | Content ])],
	    {
		pp_papers_project(PName,Papers, Stats, Content, []),
		output_stats(Stats, 0, Total)
	    }
	),
	
	!.
pp_papers_all(0) -->
	{bu_messages:warning("no projects found!",[])}.

% Projects loop
pp_papers_projects([],_Papers,X,X) --> [].
pp_papers_projects([P|RP],Papers,Accum,Result) -->
	    {
		bu_messages:note("Processing project ~w",[P])
	    },
            [env(section(misc(project_section_title, [string_esc(~atom_codes(P))])), Content)],
	    {
		pp_papers_project(P,Papers, Stats, Content, []),
		bu_support:output_stats(Stats, Accum, NAccum)
	    },
	    pp_papers_projects(RP,Papers,NAccum,Result).

% Pretty print papers for a given project
pp_papers_project(Project, Papers, Stats) -->
	{
	    findall(Keyword, 
	        (
		    member(Keyword, Papers), 
		    bu_databases:bibitem(Keyword,List),
		    (bu_support:filter_project(Project, List) -> true)
		), 
		FilteredPapers )
	},
        pp_papers_term:in_sections([level(1),
	                       level(2),
                               butype("book"),
                               butype("invited"),
                               butype("incollection"),
                               level(3),
                               butype("workshop"),
                               butype("techreport")],
	                       FilteredPapers, Stats).


