:- module(bu_options, 
	[parse/1, usage/0, 
	 get/2, set/2,
	 mode/1, format/1], 
	[assertions, regtypes, fsyntax, hiord, optparse]).

:- use_module(.(bu_databases), [doc_option/2]).

:- use_module(library(system), [file_exists/1]).
:- use_module(library(terms), [atom_concat/2]).

halt_(_):-throw(halt).

:- regtype mode/1.

mode(bytopic).
mode(byyear).
mode(byproject).
mode(cur).
mode(macros).

:- regtype format/1.

format(html).
format(tex).
format(doc).
format(pl).

:- pred parse(Args) : 
	list(Args, atom).
parse(Args):-
	reset,
	(
	    parse_args(Args) -> 
	    true
	;
	    throw(error(bad_options(Args),
	    bu_options:parse_arguments/2-1))
	).

:- default_action(true, []).
:- default_action(set(input, Input), [Input]).

:- discontiguous default_option/2. 
:- discontiguous exec_option/4. 

:- simple_option(['-h', '--help'], 
	(usage, halt_(0)), 
	"Show this help.",
	finished, Args, Args).
:- simple_option(['-v', '--verbose'], 
	set(verbose_level, 2), 
	"Verbose mode (incompatible with debug/quiet mode).", 
	continue, Args, Args).
:- simple_option(['-q', '--quiet'], set(verbose_level, 0), 
	"Quiet mode (incompatible with debug/verbose mode).", 
	continue, Args, Args).
:- simple_option(['-d', '--debug'], 
	set(verbose_level, 10), 
	"Debug mode (incompatible with quiet/verbose mode).", 
	continue, Args, Args).
default_option(verbose_level, 1).
:- simple_option(['--papers'], 
	set(papers, Arg), 
	"<file> set the papers file", 
	continue, [Arg|Args], Args).
%default_option(config, 'pl2pubs_config.pl').
%default(config, pl2pubconfig).
:- simple_option(['--mode'], 	
	(
	    mode(Mode) ->
	    set(mode, Mode)
	;
	    throw(error(unknown_mode(Mode), 
	                bu_options:parse_arguments/2-1))
	),
	"<sorting-mode>. set sorting mode (bytopic/byyear/byproject/cur/macros).",
	continue, [Mode|Args], Args).
:- simple_option(['--html'], set(format, html), 
	"HTML output format", 
	continue, Args, Args).
:- simple_option(['--tex'], 
	set(format, tex), 
	"LaTeX output format", 
	continue, Args, Args).
:- simple_option(['--doc'], set(format, doc), 
	"DocTree output format (for debug)",  
	continue, Args, Args).
:- simple_option(['--pl'], set(format, pl), 
	"Language independent Prolog output format (for debug)",  
	continue, Args, Args).
default_option(format, tex).
:- simple_option('--content',
	set(content_table, on),
 	"Generate table of content", 
	continue, Args, Args).
:- simple_option('--no-content', 
	set(content_table, off),
	"Do not generate table of content",
	continue, Args, Args).
default_option(content_table, off).
:- simple_option(['--split'], 
	( set(split, on), set(content_table, on) ),
	"spit output in several files", 
	continue, Args, Args).
:- simple_option(['--no-split'], 
	set(split, off),
	"do not spit output in several files", 
	continue, Args, Args).
default_option(split, on).
:- simple_option('-o', set(output, File),
	"<file>. Set output file.", 
	continue, [File | Args], Args).
default_option(output, Output):-
	get_(mode, Mode), 
	(
	    get_(format, doc) ->
	    terms:atom_concat([Mode, '_doc', '.pl'], Output)
	;
	    get_(format, Format) ->
	    terms:atom_concat([Mode, '.', Format], Output)
	).
:- simple_option('--author',
	( (Atom = '-' -> true ; atom_codes(Atom, String)),
	   set(author, String) ), 
	"<string>. set string for selecting author (or - fo no selection).",
	continue, [Atom|Args], Args).
default_option(author, _).
:- simple_option('--author-string', 
	set(author_string, Author), 
	"<string>.  Author name to use in section titles.",
	continue, [Author|Args], Args).
:- simple_option('--lang', set(lang, Lang), 
	"<lang>. Set output language (eng/esp).", 
	continue, [Lang | Args], Args).
default_option(lang, eng).
:- simple_option('--pdf-dir',
	set(pdf_dir, ~atom_codes(Dir)), 
	"<dir>. Set dir where the papers' .pdf (or .ps/.ps.gz) files are.", 
	continue, [Dir |Args], Args).
:- simple_option('--pdf-url', 
	set(pdf_url, ~atom_codes(Url)), 
	"<url>. url of papers' pdf.", 
	continue, [Url | Args], Args).
:- simple_option('--people-db', set(people_db,Db), 
	"<file>.  Set people's urls database to <file>.", 
	continue, [Db | Args], Args).
:- simple_option('--topic-db', set(topic_db,Db), 
	"<file>.  Set people's urls database to <file>.", 
	continue, [Db | Args], Args).
:- simple_option('--project', 
	( (Atom = '-' -> true ; Project = Atom),
	   set(project, Project) ), 
	"<string>. Set string for selecting project (or - fo no selection).",
	continue, [Atom|Args], Args).
default_option(project, _).
:- simple_option('--year-min', 
	(atom_number(AMin, Min),
	 set(year_min, Min)),
	 "<int>, Set Start year",
	 continue, [AMin|Args], Args). 
default_option(year_min, 1950).
:- simple_option('--year-max', 
	(atom_number(AMax, Max), 
	 set(year_max, Max)),
	 "<int>, Set end year",
	 continue, [AMax|Args], Args). 
default_option(year_max, 2050).
default_option(ranking, on).
:- base_message(true, "\
	pl2pubs <Options> [file]
"
%Supported Modes
%---------------
%bytopic    Generate publication list sorted by topic
%byyear     Generate publication list sorted by year
%byproject  Generate publication list sorted by project
%cur        Generate publication list for CV
%macros     Generate publication list under the form of LaTeX macros
%
|| "
Possible Options
-----------------").

:- data option/2.

reset:-
	retractall_fact(option(_, _)).


:- pred set(Key, _Value) : atom(Key).
set(Key, Value):-
	(
	    current_fact(option(Key, _)) ->
	    throw(error(already_set_option(Key), 
	                options:parse_arguments/2-1))
	;
	    assertz_fact(option(Key, Value))
	).

:- pred get(Key, _Value) : atom(Key).
get(Key, Value):-
	    (
		get_(Key, Value_) ->
		Value = Value_
	    ;
		throw(error(undefined_option(Key), 
                            options:get/2-1))
	    ).

get_(Key, Value):-
	(
	    current_fact(option(Key, Value)), !
	;
	    bu_databases:doc_option(Key, Value), !
	;
	    default_option(Key, Value)
	).

