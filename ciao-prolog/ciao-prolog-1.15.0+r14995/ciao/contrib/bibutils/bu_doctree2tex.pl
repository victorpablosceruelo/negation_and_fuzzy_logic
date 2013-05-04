:- module(bu_doctree2tex, _, [dcg, fsyntax]).

:- use_module(bu_messages, [note/2, warning/2, error/2]).

insert([]) --> [].
insert("%" || T) --> !, "\\%", insert(T).
insert("&" || T) --> !, "\\&", insert(T).
insert("_" || T) --> !, "\\_", insert(T).
insert([H|T]) --> [H], insert(T).
 
raw([]) -->  [].
raw([H|T]) --> [H], raw(T).

env2tex(H) --> insert(H).

begin_env(Env) -->
	"\\begin{", env2tex(~atom_codes(Env)), "}".
end_env(Env) -->
	"\\end{", env2tex(~atom_codes(Env)), "}".


doctree2tex([]) --> [].
doctree2tex([H|T]) -->
	doctree2tex(H), 
	doctree2tex(T).
doctree2tex(string_esc(Str)) -->
	insert(Str).
doctree2tex(section_env([level(I)], _, Title, Content)) -->
	(
	    {I = 1} 
	->
   	    "\\BUsection"
	;
   	    "\\BUsubsection"
	), 
	"{", doctree2tex(Title), "}\n", doctree2tex(Content), "\n\n".
doctree2tex(''''(Str)) -->
	"\\'{", insert(Str), "}".
doctree2tex('`'(Str)) -->
	"\\`{", insert(Str), "}".
doctree2tex(^'~'(Str)) -->
	"\\~{", insert(Str), "}".
doctree2tex('..'(Str)) -->
	"\\\"{", insert(Str), "}".
doctree2tex('AA'([])) -->
	"{\\AA}".
doctree2tex('c'(Str)) -->
	"\\c{", insert(Str), "}".
doctree2tex(env_(Env, Content)) -->
	begin_env(Env), "\n", 
	doctree2tex(Content), 
	end_env(Env), "\n".
doctree2tex(bf(Content)) --> 
	"\\textbf{", doctree2tex(Content), "}".
doctree2tex(em(Content)) --> 
	"\\emph{", doctree2tex(Content), "}".
doctree2tex(tt(Content)) --> 
	"\\texttt{", doctree2tex(Content), "}".
doctree2tex(item(Content)) --> 
	"\\item ", doctree2tex(Content), "\n".
doctree2tex(p([])) --> "\n\n".
doctree2tex(href(URL, Content)) -->
	"\\href{", raw(URL), "}{", doctree2tex(Content), "}".
doctree2tex(href(URL)) -->
	"\\href{", raw(URL), "}{", insert(URL), "}".
doctree2tex(raw(TeX)) --> !, 
	raw(TeX).

doctree2tex(comment(String)) -->
	" %% ", 
        insert(String), 
	" %%\n".

doctree2tex(content_table) --> 
	" %% DocTree content_table ignored %%\n" .

doctree2tex(X) --> 
	{ bu_messages:error("Doctree ~w not recognize by TeX translator", [X]) },
	"%% **** ERROR **** \n".
