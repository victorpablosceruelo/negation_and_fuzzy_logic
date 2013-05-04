option(input, '/home/clip/PlDbs/clip_papers_auto.pl').
option(people_db,  '/home/clip/PlDbs/people_urls.pl').
option(topic_db, '/home/clip/PlDbs/clip_topics.pl').
option(author_string, 'CLIP Group').
option(pdf_dir, '/home/clip/public_html/papers').
option(pdf_url, '/papers').
option(verbose_level, 1).
option(year_min, 1950).
option(year_max, 2050).
option(lang, eng).
option(format, doc).
option(content_table, on).

option(citeseer_db_files, ['/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_citeseer_auto.pl']).
option(citeseer_aliases_file,
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_citeseer_aliases.pl').

option(citeseerx_db_files, [
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_citeseerx_1993_auto.pl',
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_citeseerx_1994_auto.pl',
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_citeseerx_1995_auto.pl',
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_citeseerx_1996_auto.pl',
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_citeseerx_1997_auto.pl',
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_citeseerx_1998_auto.pl',
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_citeseerx_1999_auto.pl',
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_citeseerx_2000_auto.pl',
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_citeseerx_2001_auto.pl',
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_citeseerx_2002_auto.pl',
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_citeseerx_2003_auto.pl',
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_citeseerx_2004_auto.pl',
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_citeseerx_2005_auto.pl',
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_citeseerx_2006_auto.pl']).
option(citeseerx_aliases_file,
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_citeseerx_aliases.pl').
option(core_db_files, [
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_core_conf_auto.pl',
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_core_jour_auto.pl',
         '/home/clip/PlDbs/rankings/bu_ranking_db/core_jour_aliases_auto.pl']).
option(core_aliases_file, '/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_core_aliases.pl').
option(jcr_db_files, [
	'/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_jcr_auto.pl', 
	'/home/clip/PlDbs/rankings/bu_ranking_db/jcr_aliases_auto.pl']).
option(jcr_aliases_file, '/home/clip/PlDbs/rankings/bu_ranking_db/ranking_db_jcr_aliases.pl').

option(papers, '/home/clip/PlDbs/clip_papers_auto.pl').

newcommand("date", "10 septembre 2008", []).

document("@var{bibliography}").