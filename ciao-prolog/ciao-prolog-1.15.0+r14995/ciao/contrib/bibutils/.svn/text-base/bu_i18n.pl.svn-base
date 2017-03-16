:- module(bu_i18n, _, [assertions]).

:- push_prolog_flag(multi_arity_warnings,off).

defined_lang(eng).
defined_lang(esp).

%-----------------------------------------------------------------------------
:- pred butype_description(Butype,Text) # "@var{Text} is the textual
   description of @var{Butype} in English. This description should be
   short (will probably be used as a section title).".
%-----------------------------------------------------------------------------

butype_description(A,B) :- butype_description(A,eng,B).

%-----------------------------------------------------------------------------
:- pred butype_description(Butype,Lang,Text) # "@var{Text} is the
   textual description of @var{Butype} in language @var{Lang}. This
   description should be short (will probably be used as a section
   title).".
%-----------------------------------------------------------------------------

butype_description(level(I), L, S):-!,
	butype_description(I, L, S). 
butype_description(butype(BuType), L, S):-!,
	butype_description(BuType, L, S). 
% 
%%% The new classification method, based on the conf/jou rankings

%% - Articles published in a first level conference or journal:
butype_description(1,eng,
	"Articles in First-Level Refereed Conferences and
	Journals"
).
butype_description(1,esp,
	"Art@'{i}culos con Revisi@'{o}n Estricta en Congresos y
         Revistas de Primer Nivel"
).
%% - Articles published in a second level conference or journal:
butype_description(2,eng,
	"Articles in Second-Level Refereed Conferences and
	Journals" 
).
butype_description(2,esp,
	"Art@'{i}culos con Revisi@'{o}n Estricta en Congresos y
         Revistas de Segundo Nivel"
).

%% - Articles published in a third level conference or journal:
butype_description(3,eng,

	"Articles in Third-Level (or Non-Indexed) Refereed Conferences
         and Journals"
%% }}"
%%         ++ ~ranking_explanation(3,eng)
).
butype_description(3,esp,
	"Art@'{i}culos con Revisi@'{o}n Estricta en Congresos y
         Revistas de Tercer Nivel" 
).



%% 
%%% The old classification method, based on butype
%% 
%% 1 * Refereed/serious publications:
%% ----------------------------------
%% - Articles published in a first level conference or journal (old method):
butype_description("pubs",eng,"Articles in Refereed Conferences and Journals").
butype_description("pubs",esp,
	"Art@'{i}culos con Revisi@'{o}n Estricta").
%% - Only refereed articles published in a journal:
butype_description("article",eng,"Articles in Refereed Journals").
butype_description("article",esp,"Art@'{i}culos en Revistas").
%% - Only refereed articles published in conference proceedings. 
butype_description("inproceedings",eng,"Articles in Refereed Conferences").
butype_description("inproceedings",esp,
	"Art@'{i}culos en Congresos con Revisi@'{o}n Estricta").
%% - Books >with editor<, including formal proceedings
butype_description("book",eng,"Books and Monographs").
butype_description("book",esp,"Libros y Monograf@'{i}as").
%% 2 * Unrefereed, but important publications:
%% -------------------------------------------
%% - Invited *papers* (inc. talks/tutorials when a paper is published).
%%   These have ``inproceedings'' bibtex type but ``invited'' butype. 
butype_description("invited",eng,"Invited Papers and Tutorials").
butype_description("invited",esp,"Art@'{i}ulos Invitados y Tutoriales").
%% - Paper in book >with editor<
butype_description("incollection",eng,
	"Articles in Books and Other Collections").
butype_description("incollection",esp,
	"Art@'{i}culos en Libros y Otras Colecciones").
%% - Workshop papers. These have ``inproceedings'' bibtex
%%   type but ``workshop'' butype.
butype_description("workshop",eng,"Publications in Refereed Workshops").
butype_description("workshop",esp,"Publicaciones en Workshops con Revisi@'{o}n").
butype_description("techreport",eng,"Technical Reports and Manuals").
butype_description("techreport",esp,"Informes T@'{e}cnicos y Manuales").
%% These are sometimes collapsed to the above:
butype_description("manual",eng,"Technical Reports and Manuals").
butype_description("manual",esp,"Informes T@'{e}cnicos y Manuales").
butype_description("editedbook",eng,"Edited Books and Proceedings").
butype_description("editedbook",esp,"Libros Editados y Otras Colecciones").
butype_description("mastersthesis",eng,"Master's Theses").
butype_description("mastersthesis",esp,"Tesis de Master").
butype_description("phdthesis",eng,"Ph.D. Theses").
butype_description("phdthesis",esp,"Tesis Doctorales").
%% - No need for publisher, etc. -> e.g., workshop proceedings?:
butype_description("proceedings",eng,"Proceedings").
butype_description("proceedings",esp,"Actas").
%% - Invited talks and tutorials at major conferences, when
%%   there is no paper associated with them (in that case
%%   they are ``invited''). These have ``inproceedings'' bibtex  
%%   type but ``invitedtalk'' butype.
butype_description("invitedtalk",eng,"Invited Talks and Tutorials").
butype_description("invitedtalk",esp,"Charlas Invitadas y Tutoriales").
%% Accepts anything (e.g., internet postings)
butype_description("misc",eng,"Miscellaneous").
butype_description("misc",esp,"Otras Publicaciones").
%% Note required! For manuscripts...
butype_description("unpublished",eng,"Other Documents").
butype_description("unpublished",esp,"Otros Documentos").


%-----------------------------------------------------------------------------
% Month translation (English/Spanish for now)
%-----------------------------------------------------------------------------

% Backwards compat
spanish_month(Month,TranslatedMonth):- 
	month_to_lang(Month,esp,TranslatedMonth).

month_to_lang(0, _, _):-!, fail.

month_to_lang(1,  eng, "January"):- !.
month_to_lang(2,  eng, "February"):- !.
month_to_lang(3,  eng, "March"):- !.
month_to_lang(4,  eng, "April"):- !.
month_to_lang(5,  eng, "May"):- !.
month_to_lang(6,  eng, "June"):- !.
month_to_lang(7,  eng, "July"):- !.
month_to_lang(8,  eng, "August"):- !.
month_to_lang(9,  eng, "September"):- !.
month_to_lang(10, eng, "October"):- !.
month_to_lang(11, eng, "November"):- !.
month_to_lang(12, eng, "December"):- !.

month_to_lang(1,  esp, "Enero"):- !.
month_to_lang(2,  esp, "Febrero"):- !.
month_to_lang(3,  esp, "Marzo"):- !.
month_to_lang(4,  esp, "Abril"):- !.
month_to_lang(5,  esp, "Mayo"):- !.
month_to_lang(6,  esp, "Junio"):- !.
month_to_lang(7,  esp, "Julio"):- !.
month_to_lang(8,  esp, "Agosto"):- !.
month_to_lang(9,  esp, "Septiembre"):- !.
month_to_lang(10, esp, "Octubre"):- !.
month_to_lang(11, esp, "Noviembre"):- !.
month_to_lang(12, esp, "Diciembre"):- !.
% Default: do not translate and warn.
month_to_lang(A,L,A) :-
	throw(error(cannot_translate(A, L), support:month_to_lang/2)).
	

%-----------------------------------------------------------------------------
:- pred misc_text(Id,Lang,Text, Arg) # "@var{Text} is the textual
   description of @var{Id} in language @var{Lang}. ".
%-----------------------------------------------------------------------------

misc_text(volume_short,eng, "Vol. @var{N}", ["N"]).
misc_text(volume_short,esp, "Vol. @var{N}", ["N"]).
misc_text(volume_long,eng, "Volume @var{N}", ["N"]).
misc_text(volume_long,esp, "Vol@'{u}men @var{N}", ["N"]).

misc_text(number_short,eng, "Num. @var{N}", ["N"]).
misc_text(number_short,esp, "N@'{u}m. @var{N}", ["N"]).
misc_text(number_long,eng, "Number @var{N}", ["N"]).
misc_text(number_long,esp, "N@'{u}mero @var{N}", ["N"]).

misc_text(inpress,eng, "To Appear", []).
misc_text(inpress,esp, "En Prensa", []).

misc_text(pages,eng, "pages @var{N}", ["N"]).
misc_text(pages,esp, "p@'{a}ginas @var{N}", ["N"]).

misc_text(pages_fromto, eng, "pages @var{B}--@var{E}", ["B", "E"]).
misc_text(pages_fromto, esp, "p@'{a}ginas @var{B}--@var{E}", ["B", "E"]).

misc_text(msthesis,eng, "Ms. Thesis", []).
misc_text(msthesis,esp, "Tesis de Licenciatura", []).

misc_text(phdthesis,eng, "Ph.D. Thesis", []).
misc_text(phdthesis,esp, "Tesis Doctoral", []).

misc_text(publications_title, eng, "Publications (in reverse chronological order)", []).
misc_text(publications_title, esp, "Publicac@'{i}ones (en el orden chronol@'{o}gic invertido)", []).

misc_text(project_section_title, eng, "Publications presenting results
  of the project @var{Project}", ["Project"]).
misc_text(project_section_title, esp, "Publicaciones que presentan los
  resultados del projecto @var{Project}", ["Project"]).
 
misc_text(topic_section_title, eng, "Publications in @bf{@var{T}}", ["T"]).
misc_text(topic_section_title, esp, "Publicaciones en @bf{@var{T}}", ["T"]).

misc_text(year_upto_section_title, 'eng', "@var{A}'s Publications up to @bf{@var{Y}}", ["A", "Y"]).
misc_text(year_section_title, 'eng', "@var{A}'s Publications in @bf{@var{Y}}", ["A", "Y"]).

%% How to solve the problem of de/del in Spanish
%% Ex: "Publicaciones de Manuel en 2011" 
%%     "Publicaciones del Grupo CLIP en 2011" 
misc_text(year_upto_section_title_upto, 'esp', "Publicaciones hasta @bf{@var{Y}}", ["A", "Y"]).
misc_text(year_section_title, 'esp', "Publicaciones en @bf{@var{Y}}", ["A", "Y"]).

misc_text(ave_pos_rank, eng, "Average position: top @var{P}.", ["P"]).
misc_text(ave_pos_rank, esp, "posici@'{o}n media: top @var{P}.", ["P"]).

misc_text(jcr_rank, eng, "JCR: position (ave) top @var{Pos}%, impact (ave) @var{Impact}, subject(s): @var{Subjects}.",
	["Pos", "Impact", "Subjects"]).
misc_text(jcr_rank, esp, "JCR: posici@'{o}n (media) top @var{Pos}%, impacto (medio) @var{Impact}, tema(s): @var{Subjects}.", 
	["Pos", "Impact", "Subjects"]).
misc_text(core_rank, _,  "CORE: @var{R}.", ["R"]).
misc_text(citeseer_rank, eng, "Citeseer: position @var{Pos}/@var{Tot} (top @var{Top}%), impact @var{Imp}.", 
	["Pos", "Tot", "Top", "Imp"]).
misc_text(citeseer_rank, esp,  "Citeseer: posici@'{o}n @var{Pos}/@var{Tot} (top @var{Top}%), impacto @var{Imp}.",
	["Pos", "Tot", "Top", "Imp"]).

misc_text(citeseerx_rank, eng, "CiteseerX: position (ave) top @var{Top}%, impact (ave) @var{Imp}. ",
 ["Top", "Imp"]).
misc_text(citeseerx_rank, esp, "CiteseerX: posici@'{o}n (media) top @var{Top}%, impacto (medio) @var{Imp}. ",
 ["Top", "Imp"]).

misc_text(ranking_explanation_title, eng, "Explanation of Ranking System Used", []).
misc_text(ranking_explanation_title, esp, "Explicaci@'{o}n del M@'{e}todo de Clasificaci@'{o}n Empleado", []).

misc_text(ranking_explanation, eng, 
"Publications are classified according to four ranking databases:
@begin{itemize}
@item{the @href{http://portal.isiknowledge.com}{JCR}
    listing (using for each publication venue the average position in
    the list in 1998-2008)}
@item{the @href{http://www.core.edu.au/}{CORE} listings,
    and}
@item{the
  @href{http://citeseer.ist.psu.edu/impact.html}{CiteSeer} impact listings
  (see also the upgraded CiteSeerX listing).}
@end{itemize}

Each of these databases (except CORE) maps venues to a number between
0 and 1 (or 0 and 100%) which corresponds to the position of the
corresponding venue divided by the total number of ranked venues (the
lower the position the better).  CORE classifies venues, instead, into
four discrete ranking categories: @bf{A+} (or @bf{A*}), @bf{A}, @bf{B}
and @bf{C}. In order to have a numerical figure with which to compare
to the other databases and be able to compute an average value, we
have mapped CORE conference rankings @bf{A} to @var{CoreRankA}%,
@bf{B} to @var{CoreRankB}% and @bf{C} to @var{CoreRankC}%, and CORE
journal rankings @bf{A*} to top 5%, @bf{A} to 20%, @bf{B} to 64%
and @bf{C} to 100% We obtain an overall numerical ranking for each
publication as @var{CurrentRankingMethod} of all @em{available}
rankings for the corresponding venue (some venues do not appear in all
ranking databases).  Finally, publications are classified according to
this average.  Publications with average ranking 0--@var{RankingThresholdA}%
are considered as @bf{first level},
@var{RankingThresholdA}--@var{RankingThresholdB}% are considered as @bf{second
level} and the rest are considered as @bf{third level}.  In the
listings, for each publication we report the individual rankings
available for the corresponding venue, as well as the global average
position, in the form of a percentage.", 
  ["CoreRankA", "CoreRankB", "CoreRankC", 
   "CurrentRankingMethod", 
   "RankingThresholdA", "RankingThresholdB"]).

misc_text(ranking_explanation, esp, 
"Las publicaciones se han clasificado teniendo en cuenta tres bases
 de datos diferentes sobre rankings (de revistas y/o congresos):
 @begin{itemize}
   @item{las listas @href{http://portal.isiknowledge.com}{JCR}
     (utilizando para cada revista la posici@'{o}n media en las listas
     1994-2008),}
   @item{ las listas @href{http://www.core.edu.au/}{CORE} de congresos 
     y revistas, y}
   @item{la lista de impactos 
     @href{http://citeseer.ist.psu.edu/impact.html}{CiteSeer} (ver 
     tambi@'{e}n la lista actualizada CiteSeerX)}
   @end{itemize}

   Estas son tambi@'{e}n las listas propuestas por el Ministerio en la
   convocatoria de diciembre de 2009 para la evaluaci@'{o}n de la
   calidad de la investigaci@'{o}n (\"sexenios\").  Cada una de estas
   bases de datos (excepto CORE) le asigna a cada revista/congreso un
   n@'{u}mero entre 0 y 1 (0--100%) que corresponde a la
   posici@'{o}n absoluta que ocupa la revista/congreso en la lista,
   dividida por el n@'{u}mero total de elementos en ella (mejor cuanto
   m@'{a}s baja sea la posici@'{o}n).

   A diferencia de las otras, CORE clasifica las revistas/congresos en
   cuatro categor@'{i}as: @bf{A+} (or @bf{A*}), @bf{A}, @bf{B} y
   @bf{C}. Para poder comparar con las otras bases de datos y calcular
   medias aritm@'{e}ticas, se ha utilizado la siguiente asignaci@'{o}n
   de valores num@'{e}ricos a las diferentes categor@'{i}as de
   congresos CORE @bf{A} = @var{CoreRankA}% @bf{B} = @var{CoreRankB}%
   y @bf{C} = @var{CoreRankC}%, y de revistas CORE @bf{A+} = 10%,
   @bf{A} = 20%, @bf{B} = 64% y @bf{C} = 100%.  Con estos valores se
   ha creado un ranking num@'{e}rico global para cada publicaci@'{o}n
   calculando @bf{@var{CurrentRankingMethod}} de todos los
   @em{rankings} disponibles para dicha publicaci@'{o}n (algunas
   revistas/congresos no aparecen en todas las bases de datos), y se
   han clasificado las publicaciones seg@'{u}n esta media. Las
   publicaciones con un ranking medio en el rango
   0--@var{RankingThresholdA}% se han considerado de @bf{primer
   nivel}, las del rango 
   @var{RankingThresholdA}--@var{RankingThresholdB}%, de
   @bf{segundo nivel}, y el resto se han considerado de
   @bf{tercer nivel}.  Para cada publicaci@'{o}n tambi@'{e}n se
   muestran los rankings individuales disponibles de la
   correspondiente revista/congreso, as@'{i} como la media global en
   forma de porcentaje.", 
   ["CoreRankA", "CoreRankB", "CoreRankC", "CurrentRankingMethod",
   "RankingThresholdA", "RankingThresholdB"]).




:- pop_prolog_flag(multi_arity_warnings).
