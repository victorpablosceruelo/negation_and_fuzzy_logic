:- module(latex_convert, [], [dcg, fsyntax, assertions, regtypes, isomodes]).

:- doc(title, "Convert LaTeX into Images").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module provides basic functionality to transform
   LaTeX pieces into images. LaTeX is specified as Prolog
   terms. Currently, @tt{math} and @tt{tikz} are supported.").

% TODO: Complete this implementation:
%  - Complete definition of math formulae, tikz pictures, or other
%    packages.
%  - Share definitions in preamble/ending.
%  - Move as a library on its own.

:- use_module(library(strings)).
:- use_module(library(system)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lists), [append/3]).

% ===========================================================================

:- doc(section, "Generate images from LaTeX terms").

% Some custom definitions
% TODO: we should be able to customize it
:- export(cache_dir/1).
cache_dir('latex-cache').
%fg_color('white').
fg_color('black').

:- export(latex_to_png/2).
:- pred latex_to_png(LaTeX, OutFile) # "Transforms @var{LaTeX} into a
   PNG image. @var{OutFile} is unified with the name of the generated
   image".

latex_to_png(LaTeX, OutFile) :-
	CacheDir = ~cache_dir,
	system(~atom_concat(['mkdir -p ', CacheDir])),
	working_directory(Prev, CacheDir),
	( latex_to_png_(LaTeX, OutFile) -> Ok = yes ; Ok = no ),
	working_directory(_, Prev),
	( Ok = yes -> true ; throw(failed_latex_to_png) ).
 
latex_to_png_(LaTeX, OutFile) :-
	open('temp.tex', write, S),
	preamble(LaTeX, S),
	body(LaTeX, S),
	end(LaTeX, S),
	close(S),
	%
	new_png_name(OutFile),
	%
	convert(LaTeX, OutFile).

% Obtain a fresh new PNG name
new_png_name(OutFile) :-
	( retract_fact(png_count(N)) -> true ; N = 0 ),
	N1 is N + 1,
	assertz_fact(png_count(N1)),
	%
	number_codes(N, Nc),
	atom_codes(Na, Nc),
	OutFile = ~atom_concat([out, Na, '.png']).

% A counter to number images
:- data png_count/1.

:- export(latex_cache_reset/0).
latex_cache_reset :-
	retractall_fact(png_count(_)).

% ----------------------------------------------------------------
:- doc(subsection, "Tranlation and Conversion of Different LaTeX Pieces").

:- discontiguous preamble/2.
:- discontiguous body/2.
:- discontiguous end/2.
:- discontiguous convert/2.

:- doc(subsubsection, "Math formulae").

preamble(math(_), S) :-
	write_string(S,
"\\documentclass{article}\n"||
"\\usepackage{amsmath}\n"||
"\\usepackage{amsthm}\n"||
"\\usepackage{amssymb}\n"||
"\\usepackage{bm}\n"||
% TODO: those do not seem to be required
"\\newcommand{\\mx}[1]{\\mathbf{\\bm{#1}}} % Matrix command\n"||
"\\newcommand{\\vc}[1]{\\mathbf{\\bm{#1}}} % Vector command \n"||
"\\newcommand{\\T}{\\text{T}}                % Transpose\n"||
%
"\\usepackage{color}\n"||
"\\pagestyle{empty}\n"||
"\\begin{document}\n"),
	%
	set_fg_color(S).
end(math(_), S) :-
	write_string(S, "\\newpage\n"),
	write_string(S, "\\end{document}\n").
body(math(Text), S) :-
	math_to_string(Text, Formula, []),
	write_string(S, "$"),
	write_string(S, Formula),
	write_string(S, "$").
convert(math(_), OutFile) :-
	% Size = '1200',
	Size = '1400',
	system('latex temp.tex > latex.log 2>&1'),
	BG='Transparent',
	%	FG='\'rgb 1.0 1.0 1.0\'',
	%	BG='\'rgb 0.0 0.0 0.0\'',
	atom_concat(['dvipng -T tight -x ',
	             Size,
		     ' -z 9',
		     % ' -fg ', FG,
		     ' -bg ', BG,
		     ' -o ', OutFile,
		     ' temp.dvi',
		     ' > dvipng.log 2>&1'], Png),
	system(Png).

:- doc(subsubsection, "TikZ pictures").

preamble(tikz(_), S) :-
	write_string(S,
"\\documentclass{article}\n"||
"\\usepackage[x11names]{xcolor}\n"||
"\\usepackage{tikz}\n"||
"\\usetikzlibrary{trees}\n"||
"\\usepackage[graphics,tightpage,active]{preview}\n"||
"\\PreviewEnvironment{tikzpicture}\\PreviewEnvironment{equation}\\PreviewEnvironment{equation*}\n"||
"\\newlength{\\imagewidth}\n"||
"\\newlength{\\imagescale}\n"||
"\\usepackage{color}\n"||
"\\pagestyle{empty}\n"||
"\\thispagestyle{empty}\n"||
"\\begin{document}\n"),
	%
	set_fg_color(S).
end(tikz(_), S) :-
	write_string(S, "\\end{document}\n").
body(tikz(X), S) :-
	tikz_to_string(X, String, []),
	write_string(S, String).
convert(tikz(_), OutFile) :-
	system('pdflatex temp.tex > latex.log 2>&1'),
	system('pdftops -eps temp.pdf'),
	atom_concat(['convert -density 90 temp.eps ',
	             OutFile,
		     ' > dvipng.log 2>&1'], Png),
	system(Png).

set_fg_color(S) :-
	fg_color(Color),
	atom_codes(Color, Cs),
	write_string(S, "\\color{"||(~append(Cs, "}\n"))).

% ===========================================================================

:- doc(section, "LaTeX formulae to String").

:- export(math_to_string/3).
:- pred math_to_string(A, Str, Str0) # "Translate the LaTeX formula
   @var{A} into a string".

math_to_string(A) --> { number(A) }, !,
	{ number_codes(A, Cs) },
	emit_string(Cs).
math_to_string(A) --> { atom(A) }, !,
	{ atom_codes(A, Cs) },
	emit_string(Cs).
math_to_string(sqrt(X)) -->
	"\\sqrt{", math_to_string(X), "}".
math_to_string(sqrt(N,X)) -->
	"\\sqrt[", math_to_string(N), "]{", math_to_string(X), "}".
math_to_string(apply(N,X)) -->
	math_to_string(N), "(", math_to_string(X), ")".
math_to_string(A=B) -->
	math_to_string(A), "=", math_to_string(B).
math_to_string(A+B) -->
	math_to_string(A), "+", math_to_string(B).
math_to_string(sup(A,N)) -->
	math_to_string(A), "^{", math_to_string(N), "}".
math_to_string(sub(A,N)) -->
	math_to_string(A), "_{", math_to_string(N), "}".
math_to_string(frac(A,B)) -->
	"\\frac{", math_to_string(A), "}{", math_to_string(B), "}".

emit_string([]) --> [].
emit_string([X|Xs]) --> [X], emit_string(Xs).

:- export(tikz_to_string/3).
:- pred tikz_to_string(A, Str, Str0) # "Translate a Tikz picture
   @var{A} into a string".

tikz_to_string(A) --> { number(A) }, !,
	{ number_codes(A, Cs) },
	emit_string(Cs).
tikz_to_string(A) --> { atom(A) }, !,
	{ atom_codes(A, Cs) },
	emit_string(Cs).
tikz_to_string(tikzpicture(Opts, Body)) -->
	"\\begin{tikzpicture}[", tikz_to_string_list(Opts), "]\n",
	tikz_to_string_list(Body),
	"\\end{tikzpicture}\n".
	%
tikz_to_string(node(Opts, Main, More)) -->
	"\\", node_to_string(Opts, Main, More), ";\n".

tikz_to_string_list([]) --> !, [].
tikz_to_string_list([X|Xs]) --> !,
	tikz_to_string(X),
	tikz_to_string_list(Xs).

node_to_string(Opts, Main, More) -->
	"node [", tikz_to_string_list(Opts), "] {",
	tikz_to_string(Main),
	"}",
	tikz_to_string_more(More).
	
tikz_to_string_more([]) --> [].
tikz_to_string_more([X|Xs]) -->
	" ", tikz_to_string_more_(X),
	tikz_to_string_more(Xs).

tikz_to_string_more_(child(node(Opts, Main, More))) -->
	"child {", node_to_string(Opts, Main, More), "}".

% "    \\node [circle,draw] {hello}\n"||
% "      child {\n"||
% "        node [circle,draw,fill=red] {2}\n"||
% "            child {node [circle,draw] {1}}\n"||
% "            child {node [circle,draw] {3}}\n"||
% "      }\n"||
% "      child {
% "        node [circle,draw] {6}\n"||
% "        child {node [circle,draw] {5}}\n"||
% "        child {node [circle,draw] {9}\n"||
% "          child {node [circle, draw] {7}} \n"||
% "          child [missing]\n"||
% "        }\n"||
% "      };\n"||
%


% "\\begin{tikzpicture}[level/.style={sibling distance=60mm/#1}]\n"||
% "    \\node [circle,draw] {hello}\n"||
% "      child {\n"||
% "        node [circle,draw,fill=red] {2}\n"||
% "            child {node [circle,draw] {1}\n"||
% "       }\n"||
% "        child {\n"||
% "            node [circle,draw]{3}\n"||
% "        }\n"||
% "      }\n"||
% "      child {node [circle,draw] {6}\n"||
% "        child {node [circle,draw]  {5}\n"||
% "        }\n"||
% "      child {node [circle,draw]  {9}\n"||
% "        child {node [circle, draw]  {7}} \n"||
% %"        child [missing]\n"||
% "        }\n"||
% "    };\n"||
% %
% "\\end{tikzpicture}\n").
