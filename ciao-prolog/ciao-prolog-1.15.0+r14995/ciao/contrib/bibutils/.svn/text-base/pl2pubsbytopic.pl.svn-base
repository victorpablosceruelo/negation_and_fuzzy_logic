:- module(pl2pubsbytopic, [pp_papers_all/3], [assertions, dcg, fsyntax]).
  
:- use_module(bu_messages,  [debug/2, note/2, warning/2, error/2]).
:- use_module(bu_databases, [bibitem/2, load_topic/1, topic/2]).

:- use_module(bu_support, [filter_topic/2, filter_common/1]).
:- use_module(bu_support, [output_stats/3]).

:- use_module(library(aggregates), [findall/3]).

:- use_module(bu_options, [get/2]).

:- use_module(pp_papers_term, [in_sections/5]).

:- use_module(bu_set, [add/2, collect/2]).

:- use_module(library(prolog_sys), [statistics/2]).

%% Paper Selection and Output
pp_papers_all(Total) -->
        {
	    bu_options:get(topic_db, TopicDb),
	    bu_messages:debug("<pl2pubsbytopic> Load Topic DB (~w)", [TopicDb]),
	    bu_databases:load_topic(TopicDb),

	    %%% Get all papers %%%
	    bu_messages:debug("<pl2pubsbytopic> Filter papers to output", []),
	    findall(Keyword, 
	      (
		  bu_databases:bibitem(Keyword,List),
		  (
		      bu_support:filter_topic(Topic, List), 
		      bu_databases:topic(Topic, _Text),
		      bu_set:add(topic, Topic), 
		      fail
		  ; 
		      true
		  ),
		  (bu_support:filter_common(List) -> true)
	      ),
	      Papers),

	      bu_set:collect(topic, Topics_),
              findall(Topic, 
	        (
		    bu_databases:topic(Topic, _Text), 
		    member(Topic, Topics_)
		),
		Topics
	       )
          },
	  (
	      {Topics = []} ->
	      {bu_messages:warning("no topics found!",[])}
	  ;
	      {bu_messages:note("Topics found in topics database: ~w",[Topics])},

	      [env(section(misc(ranking_explanation_title, [])), ranking_explanation)],
	      
	      %% Process those topics
	      pp_papers_topics(Topics,Papers,0,Total),
	      {statistics(usertime, [_, Z]), bu_messages:error("time: ~w", [Z])}
	  ), !.

% Topics loop
pp_papers_topics([],_Papers,X,X) --> [].
pp_papers_topics([T|RT],Papers,Accum,Result) -->
	{bu_messages:debug("<pl2pubsbytopic> Processing topic ~w",[T])},
	[env(section(misc(topic_section_title, [tex_string(TopicText)])), Content)],
	{
	    bu_databases:topic(T,TopicText),
	    pp_papers_topic(T, Papers, Stats, Content, []),
	    bu_support:output_stats(Stats, Accum, NAccum)
	},
	{bu_messages:debug("<pl2pubsbytopic> topic ~w processed",[T])},
	pp_papers_topics(RT,Papers,NAccum,Result).

% Pretty print papers for a given topic
pp_papers_topic(Topic,Papers,Stats) -->
	{bu_messages:debug("<pl2pubsbytopic> Filter papers of topic ~w", [Topic])},
	{ findall(Keyword, 
	        (
		    member(Keyword, Papers), 
		    bu_databases:bibitem(Keyword,List),
		    (bu_support:filter_topic(Topic, List) -> true)
		), 
		FilteredPapers ) },
        % First level articles
        pp_papers_term:in_sections([level(1),
	                        level(2),
                                butype("book"),
                                butype("invited"),
                                butype("incollection"),
                                level(3),
                                butype("workshop"),
                                butype("techreport")],
	                        FilteredPapers, Stats).
