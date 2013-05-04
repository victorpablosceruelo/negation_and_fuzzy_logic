THIS MODULE IS BROKEN

% TODO: revive
:- use_module(library(aggregates), [findall/3]).

:- export(make_actmod/2).
make_actmod(ModuleSpec, PublishMod) :-
        % nonvar() below is always true for files
        process_file(ModuleSpec, nop, module, false, nonvar, false, false),
        element_spec_to_base(prolog_source, ModuleSpec, Base),
        base_res(Base, Res),
	element_query(executable, Res, ExecRes),
	element_filename(ExecRes, ExecName),
        create_main(Base, PublishMod, MainSpec),
        make_exec([MainSpec], ExecName).

%%% --- Making main file for active modules --- %%%

create_main(Base, PublishMod, MainFile) :-
%        findall(:-(multifile(F/A)), Itf.get_def_multifile(F, A, _),
%                Specific_code, ExeFacts),
        findall(exe(Pred,Pred), actmod_serves(Base, Pred), ExeFacts),
	base_res(Base, Res),
	element_spec(Res, Spec),
        temp_filename(MainFile),
        file_terms(MainFile, [
          :-(use_package([])),
          :-(use_module(Spec)),
          :-(use_module(library(PublishMod))),
          :-(use_module(library(actmods(actmod_server)), [actmodmain/0])),
          :-(main, actmodmain),
          :-(meta_predicate(exe(?,fact)))
          | ExeFacts]),
	MainSpec = MainFile,
	element_find(prolog_source, MainSpec, Res),
	element_query(split__itf, Res, ItfRes),
	element_filename(ItfRes, ItfName),
        assertz_fact(tmp_file(ItfName)),
	element_query(prolog_object, Res, PoRes),
	element_filename(PoRes, PoName),
        assertz_fact(tmp_file(PoName)).

actmod_serves(Base,Pred) :-
        trust(Sym instance_of module_sym),
	Sym.exports(F, A, _),
        functor(Pred, F, A).
actmod_serves(Base,Pred) :-
	trust(Itf instance_of module_itf),
	Itf.export(F, A, _, _, _, true),
        functor(Pred, F, A).        

:- data tmp_file/1.

temp_filename(File) :-
        mktemp('tmpciaoXXXXXX', File),
        assertz_fact(tmp_file(File)).

delete_temp :-
        retract_fact(tmp_file(File)),
        delete_file(File),
        fail.
delete_temp.
