
:- module(pl2pubs, 
	_, %[main/1], 
	[ciaopaths, assertions, fsyntax]).

:- use_module(pl2pubsbytopic, [pp_papers_all/3]). 
:- use_module(pl2pubsbyyear, [pp_papers_all/3]).
:- use_module(pl2pubsbyproject, [pp_papers_all/3]).
:- use_module(pl2cur, [pp_papers_all/3]).
:- use_module(pl2macros, [pp_papers_all/3]).
:- use_module(library(bibutils(ranking(bu_ranking))), [init/0]).

:- use_module(pp_papers_term, [pretty_print/3]).
:- use_module(bu_doctree, [from_term/2, pretty_print/3, doctree_substitute/4, parse_docstring/2]).
:- use_module(bu_doctree2tex, [doctree2tex/3]).
:- use_module(bu_doctree2html, [doctree2html/2, output_splitted_html/3]).

:- use_module(bu_options, [parse/1, get/2, usage/0]).
:- use_module(bu_messages, [debug/2, error/2, warning/2, note/2]).
:- use_module(bu_databases,[load_bibitem/1, load_url/1, load_document/1, doc_string/2, reset/1]).
:- use_module(bu_support, [prepare_output/4]).

:- use_module(library(format), [format/3]).
:- use_module(library(aggregates), [findall/3]).

main(Args):-
	push_prolog_flag(write_strings,on),
	catch(
	  catch((
		  bu_options:parse(Args) -> 
		  main_, 
		  halt(0)
		;
		  bu_options:usage,
		  halt(1)
	      ), 
	      error(E, bu_options:_), 
	      (
		  bu_messages:error("options parse error: ~w", [E]), 
		  bu_options:usage,
		  halt(1)
	      )),
	  E, 
	  bu_messages:error("uncaught exception: ~w", [E])).

pp_papers_all_(bytopic, Total, Content) :- 
	pl2pubsbytopic:pp_papers_all(Total, Content, []).
pp_papers_all_(byyear, Total, Content) :- 
	pl2pubsbyyear:pp_papers_all(Total, Content, []).
pp_papers_all_(byproject, Total, Content) :- 
	pl2pubsbyproject:pp_papers_all(Total, Content, []).
pp_papers_all_(cur, Total, Content):-
	pl2cur:pp_papers_all(Total, Content, []).
pp_papers_all_(macros, Total, Content):-
	pl2macros:pp_papers_all(Total, Content, []).



%%% Load Papers Db %%%
load_papers :-
	bu_options:get(papers, I), 
	bu_databases:load_bibitem(I),
	bu_messages:debug("<pl2pub> Load People DB (~w)", [I]),
	bu_databases:load_url(~(bu_options:get(people_db))).

%% Init ranking system %%%
init_ranking :- 
	bu_messages:debug("<pl2pub> Init ranking system", []),
	bu_ranking:init.

%% Process papers
process_papers(ContentPl, Total) :-
	bu_options:get(mode, M),
	bu_messages:debug("<pl2pub> Process papers (mode: ~w)", [M]),	
  	pp_papers_all_(M, Total, ContentPl).

output(ContentPl, Context) :-
	bu_options:get(input, I),
	bu_options:get(output, O),
	bu_messages:debug("<pl2pub> ouput in ~w", [O]),
	(
            % In case of macros mode, bypass output process 
	    bu_options:get(mode, macros) ->
	    prepare_output(I, tex, O, Stream),
	    format(Stream,  "~s~n~n", [ContentPl]),
	    close(Stream)
	;
	    % Normal process
	    bu_options:get(format, pl ) ->
	    prepare_output(I, pl, O, Stream),
	    format(Stream, "document(~n  ", []),
	    pp_papers_term:pretty_print(ContentPl, 2, Stream), 
	    format(Stream, "~n).~n", [])
	;
	    bu_doctree:from_term(ContentPl, ContentDocTree_),
	    (
		bu_options:get(content_table, on) ->
		Content = [content_table | ContentDocTree_]
	    ;
		Content = ContentDocTree_
	    ),
	    bu_doctree:doctree_substitute(Context, ["publications"], [Content], ContentDocTree),
	    (
		bu_options:get(format, doc) ->
		prepare_output(I, pl, O, Stream),
		format(Stream, "document(~n  ", []),
		bu_doctree:pretty_print(ContentDocTree, 2, Stream),
		format(Stream, "~n).~n", []), 
		close(Stream)
	    ;
		bu_options:get(format, tex) ->
		bu_doctree2tex:doctree2tex(ContentDocTree, TeXStr, []), 
		prepare_output(I, tex, O, Stream),
		format(Stream, "~s", [TeXStr]),
		close(Stream)
	    ;
		bu_options:get(format, html) ->
		bu_doctree2html:doctree2html(ContentDocTree, HTMLTree), 
		bu_doctree2html:output_splitted_html(I, O, HTMLTree)
	    ;
		bu_messages:error("Selected output format not implemented", []), 
		halt(1)
	    )
	).


get_doc(Doc):-
	findall(String_, 
	   (
	       bu_databases:doc_string(Mode, String), 
	       (
		   Mode = lpdoc ->
		   bu_doctree:parse_docstring(String, String_)
	       ;
		   Mode = raw ->
		   String_ = raw(String)
	       ;
		   throw(error(string(Mode, String), get_doc/1))
	       )
	   ), Doc).
	
main_ :-
        bu_options:get(input, I),
        bu_options:get(output, O),
	load_document(I),
	load_papers, 
	init_ranking,
	get_doc(Doc),
	process_papers(Content, Total), 
	output(Content, Doc),
	
	bu_messages:note("Done. ~w entries processed. wrote ~w", [Total, O]).
