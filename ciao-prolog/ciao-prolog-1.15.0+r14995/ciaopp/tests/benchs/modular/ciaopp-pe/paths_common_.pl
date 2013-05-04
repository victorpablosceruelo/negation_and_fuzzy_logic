%% ciaopplib('/home/herme/TryCVS/Systems/ciaopp').
% ciaopplib(E):-
% 	library_directory(L),
%  	atom_concat(L,'/ciaopp',E).


% :- include(version_manual).
% substituted by mypath.pl and the following 2 lines:

:- multifile library_directory/1.
:- dynamic library_directory/1.
library_directory(E) :- ciaopplib(E).

:- multifile file_search_path/2.
:- dynamic file_search_path/2.

file_search_path(ciaopp,E):-
	ciaopplib(E).
file_search_path(ctchecks,E):-
	ciaopplib(L),
	atom_concat(L,'/ctchecks',E).
file_search_path(domain,E):-
	ciaopplib(L),
	atom_concat(L,'/plai/domains',E).
file_search_path(granul,E):-
	ciaopplib(L),
	atom_concat(L,'/tr_granul',E).
file_search_path(infer,E):-
	ciaopplib(L),
	atom_concat(L,'/infer',E).
file_search_path(infernf,E):-
	ciaopplib(L),
	atom_concat(L,'/infernf',E).
file_search_path(infercost,E):-
	ciaopplib(L),
	atom_concat(L,'/infercost',E).
file_search_path(plai,E):-
	ciaopplib(L),
	atom_concat(L,'/plai',E).
file_search_path(program,E):-
	ciaopplib(L),
	atom_concat(L,'/p_unit',E).
file_search_path(spec,E):-
	ciaopplib(L),
	atom_concat(L,'/spec',E).
file_search_path(syntax,E):-
	ciaopplib(L),
	atom_concat(L,'/tr_syntax',E).
file_search_path(typeslib,E):-
	ciaopplib(L),
	atom_concat(L,'/plai/domains/typeslib',E).
file_search_path(test,E):-
	ciaopplib(L),
	atom_concat(L,'/test',E).
file_search_path(auto_interface,E):-
	ciaopplib(L),
	atom_concat(L,'/auto_interface',E).
file_search_path(rtchecks,E):-
	ciaopplib(L),
	atom_concat(L,'/rtchecks',E).
file_search_path( api ,E):-
	ciaopplib(L),
	atom_concat(L,'/api',E).
