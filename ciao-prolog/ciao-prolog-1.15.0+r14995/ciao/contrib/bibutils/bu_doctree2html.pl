:- module(bu_doctree2html, _, [dcg, fsyntax]).

:- use_module(bu_support, [incr_counter/2, prepare_output/4]).
:- use_module(bu_options, [get/2]).
:- use_module(bu_messages, [note/2, warning/2, error/2]).

:- use_module(library(lists), [append/3]).

insert([]) --> [].
insert("&" || T) --> !, "&amp;", insert(T).
insert([H|T]) --> [H], insert(T).
 

:- discontiguous begin_env/3.
:- discontiguous end_env/3.

begin_env(enumerate) --> "\n<OL>\n".
end_env(enumerate) --> "\n</OL>\n".
begin_env(itemize) --> "\n<UL>\n".
end_env(itemize) --> "\n</UL>\n".

:- use_module(bu_doctree, [doctree_substitute/4]).

doctree2html(DocTree, Html):-
	bu_doctree:doctree_substitute(var([string_esc("html_headers")]), [], [], Headers_), 
	doctree2html_(Headers_, t([], []), Headers, []),
	doctree2html_(DocTree, t(DocTree, Headers), Html_, []), 
	append(Headers, Html_, Html).

try_to_label(Label) :-
	(
	    nonvar(Label) ->
	    true
	;
	    bu_options:get(split, on) ->
	    Label = "node" || (~(lists:append(~number_codes(~(bu_support:incr_counter(label_section))), ".html")))
	;
	    Label = "bu_label_" || (~number_codes(~(bu_support:incr_counter(label_section))))
	).

doctree2html_([], _ExtraArgs) --> !.
doctree2html_([H|T], ExtraArgs) --> !, 
	doctree2html_(H, ExtraArgs), 
	doctree2html_(T, ExtraArgs).
doctree2html_(string_esc(Str), _ExtraArgs) --> !,
 	insert(Str).
doctree2html_(section_env([level(1)], [local_label(Label)], Title, Content), ExtraArgs) -->
	{	
	    ExtraArgs = t(_DocTree, Headers),
	    try_to_label(Label)
	},
	(
	    {bu_options:get(split, on)} ->
	    {
		doctree2html_section(Label, Title, Content, ExtraArgs, HtmlContent, []),
	        append(Headers, HtmlContent, HtmlContent_)
	    }, 
	    [file(Label, HtmlContent_)]
	;
	    doctree2html_section(Label, Title, Content, ExtraArgs)
	).
doctree2html_(section_env([level(2)], _REF_, Title, Content), ExtraArgs) --> !,
	"<H3>", doctree2html_(Title, ExtraArgs), "</H3>\n\n" ,
	doctree2html_(Content, ExtraArgs), "\n\n".
doctree2html_(''''(Str), _ExtraArgs) --> !,
	"&", insert(Str), "acute;".
doctree2html_(^'~'(Str), _ExtraArgs) --> !,
	"&", insert(Str), "tilde;".
doctree2html_('..'(Str), _ExtraArgs) --> !,
	"&", insert(Str), "uml;".
doctree2html_('AA'([]), _ExtraArgs) --> !,
	"&Aring;".
doctree2html_(env_(Env, Content), ExtraArgs) --> !,
	(
	    begin_env(Env)
        -> 
	    doctree2html_(Content, ExtraArgs), 
	    end_env(Env), "\n"
	;
	    { bu_messages:error("DocTree environement \"~w\" not recognized by HTML translator", [Env]) }, 
	      doctree2html_(Content, ExtraArgs)
	).
doctree2html_(bf(Content), ExtraArgs) --> !, 
%	"<SPAN  CLASS=\"textbf\">", doctree2html_(Content, ExtraArgs), "</SPAN>".
	"<B>", doctree2html_(Content, ExtraArgs), "</B>".
doctree2html_(em(Content), ExtraArgs) -->  !,
%	"<SPAN  CLASS=\"emph\">", doctree2html_(Content, ExtraArgs), "</SPAN>".
	"<I>", doctree2html_(Content, ExtraArgs), "</I>".
doctree2html_(tt(Content), ExtraArgs) -->  !,
	"<SPAN  CLASS=\"texttt\">", doctree2html_(Content, ExtraArgs), "</SPAN>".
doctree2html_(item(Content), ExtraArgs) -->  !,
	"\n<LI>", doctree2html_(Content, ExtraArgs), "</LI>\n".
doctree2html_(p([]), _ExtraArgs) --> !, "\n<p>\n".
doctree2html_(href(URL, Content), ExtraArgs) --> !,
	"<A HREF=\"", insert(URL), "\">", doctree2html_(Content, ExtraArgs), "</A>".
doctree2html_(href(URL), _ExtraArgs) --> !,
	"<A HREF=\"", insert(URL), "\">", insert(URL), "</A>".
doctree2html_(raw(String), __ExtraArgs) --> !,
	raw(String).

doctree2html_(comment(String), _ExtraArgs) --> !,
	" <!-- ", 
        insert(String), 
	" -->\n".
% content_table is not a valid doctree
doctree2html_(content_table, ExtraArgs) --> !,
	{ ExtraArgs=t(DocTree, _Headers) }, 
	"\n<UL>\n", content_table(DocTree, ExtraArgs) , "\n</UL>\n\n".  

doctree2html_(X, _ExtraArgs) --> !,
	{ bu_messages:error("DocTree ~w not recognize by HTML translator", [X]) },
	"<!-- **** Error **** -->".

doctree2html_section(Label, Title, Content, ExtraArgs) -->   
	"<H2><SPAN ID=\"", insert(Label), "\">",
	doctree2html_(Title, ExtraArgs), 
	"</H2></SPAN><P>\n\n" ,
	doctree2html_(Content, ExtraArgs), "\n\n".

raw([]) -->  [].
raw([H|T]) --> [H], raw(T).


content_table([H|T], DocTree) --> !,
	content_table(H, DocTree), 
	content_table(T, DocTree).
content_table(section_env([level(1)], [local_label(Label)], Title, _Content), DocTree) --> !,
	"<LI><A HREF=\"", 
	(
	    {bu_options:get(split, on)} ->
	    "" 
	;
	    "#"
	),
	{try_to_label(Label)}, 
	insert(Label), "\">", 
	doctree2html_(Title, DocTree),  
	"</A></LI>\n".
content_table(_, _) --> [].


:- use_module(library(format), [format/3]).
:- use_module(library(dirutils), [split_path_and_name/3]).

output_splitted_html(I, O, Html):-
	prepare_output(I, html, O, Stream), 
	dirutils:split_path_and_name(O, Dir, _File), 
	output_splitted_html_(I, Dir, ToPrint, Html, []), 
	format(Stream, "~s", [ToPrint]), 
	close(Stream).

output_splitted_html_(_, _Dir, []) --> [].
output_splitted_html_(I, Dir, ToPrint) -->
	[file(Label, Content)], 
	{
	    output_splitted_html_(I, Dir, ToPrint_, Content, []), 
	    atom_codes(FileName, Label),
	    atom_concat(Dir, FileName, Path),
	    prepare_output(I, html, Path, Stream), 
	    format(Stream, "~s", [ToPrint_]), 
	    close(Stream)
	}, 
	output_splitted_html_(I, Dir, ToPrint).
output_splitted_html_(I, Dir, [H|T]) -->
	[H], 
	output_splitted_html_(I, Dir, T).

