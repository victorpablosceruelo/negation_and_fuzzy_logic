portray_message(informational,_).
:-use_module(library(lists)).
:- dynamic
        gx_data/2.
generalise_call(Filters,Call,GenCall) :-
        Call=..[F|Args],
        gen_filters_list(Filters,Args,GArgs,_),
        GenCall=..[F|GArgs].
filter_call(Filters,Call,GCall,FCall) :-
        Call=..[F|Args],
        GCall=..[F|GArgs],
        gen_filters_list(Filters,Args,GArgs,FArgs),
        gensym(F,NewF),
        FCall=..[NewF|FArgs].
generalise_and_filter(Filters,Call,GenCall,FCall) :-
        Call=..[F|Args],
        gen_filters_list(Filters,Args,GenArgs,FilArgs),
        GenCall=..[F|GenArgs],
        gensym(F,NewF),
        FCall=..[NewF|FilArgs].
gen_filters_list(Filters,Args,GArgs,FArgs) :-
        (   gen_filters_l(Filters,Args,GArgs,FArgs) ->
            true
        ;   print_message(informational,'### GENERALISATION/FILTERING FAILED !'),
            print_message(infromational,gen_filters_list(Filters,Args,GArgs,FArgs)),
            GArgs=Args,
            FArgs=Args
        ).
gen_filters_l([Fil|___Fils],Args,GenArgs,FilArgs) :-
        gen_filter_l(Fil,Args,GenArgs,FilArgs).
gen_filter_l([],[],[],[]).
gen_filter_l([static|Fs],[A|Args],[A|GenArgs],FArgs) :-
        gen_filter_l(Fs,Args,GenArgs,FArgs).
gen_filter_l([dynamic|Fs],[_|Args],[A|GenArgs],[A|FArgs]) :-
        gen_filter_l(Fs,Args,GenArgs,FArgs).
gen_filter_l([semi|Fs],[A|Args],[A|GenArgs],[A|FArgs]) :-
        gen_filter_l(Fs,Args,GenArgs,FArgs).
gen_filter_l([nonvar|Fs],[A|Args],[G|GenArgs],FilteredArgs) :-
        nonvar(A),
        A=..[Op|AArgs],
        same_length(AArgs,FreshArgs),
        G=..[Op|FreshArgs],
        append(FreshArgs,FArgs,FilteredArgs),
        gen_filter_l(Fs,Args,GenArgs,FArgs).
gensym(H,NewHead) :-
        (   gx_data(sym,Sym) ->
            NewSym is Sym+1,
            retract(gx_data(sym,Sym))
        ;   NewSym=0
        ),
        my_assert(gx_data(sym,NewSym)),
        add_id(H,NewSym,NewHead).
add_id(H,Sym,NH) :-
        atom_concat(H,'__',H1),
        name(H1,H1S),
        name(Sym,SymS),
        append(H1S,SymS,NHS),
        name(NH,NHS).
same_length([],[]).
same_length([_|T],[_|T2]) :-
        same_length(T,T2).
build_unfold_call(Call,SpecCode,LogenData,UnfoldCall) :-
        Call=..[Func|Args],
        atom_concat(Func,'_u',NewFunc),
        append(Args,[SpecCode,LogenData],NewArgs),
        UnfoldCall=..[NewFunc|NewArgs].
build_request_call(Call,Req,ResCall,LogenData,RequestCall) :-
        Call=..[Func|Args],
        atom_concat(Func,'_request',NewFunc),
        append(Args,[Req,ResCall,LogenData],NewArgs),
        RequestCall=..[NewFunc|NewArgs].
:- dynamic
        memo_table/4.
:- dynamic
        spec_clause/2.
:-use_module(library(terms)).
:-use_module(library(terms_check)).
find_pattern(ID,Call,ResCall,_Requestor) :-
        copy_term(Call,CallCopy),
        memo_table(ID,Call,ResCall,_MEMODATA),
        variant(CallCopy,Call).
insert_pattern(ID,GCall,FCall,MEMODATA) :-
        my_assert(memo_table(ID,GCall,FCall,MEMODATA)).
update_status(ID,GCall,FCall,Req) :-
        retract(memo_table(ID,GCall,FCall,MEMODATA)),
        get_memodata_requestor(MEMODATA,pending(Req)),
        set_memodata_requestor(MEMODATA,Req,MEMODATAPRIME),
        my_assert(memo_table(ID,GCall,FCall,MEMODATAPRIME)).
spec_driver :-
        memo_table(ID,GCall,FCall,[pending(Req)|_MEMODATA]),
        update_status(ID,GCall,FCall,Req), !,
        generate_code(GCall,FCall),
        spec_driver.
spec_driver.
generate_code(Call,ResCall) :-
        copy_term((Call,ResCall),(CCall,CResCall)),
        build_unfold_call(Call,Res,[[CCall],CResCall],UnfoldCall),
        findall((ResCall:-Res),UnfoldCall,Clauses),
        (   Clauses=[] ->
            save_clauses([(ResCall:-fail)])
        ;   save_clauses(Clauses)
        ).
save_clauses([]).
save_clauses([C|Cs]) :-
        (   C=(Head:-_Body) ->
            true
        ;   Head=C
        ),
        my_assert(spec_clause(Head,C)),
        save_clauses(Cs).
print_memo_table(S) :-
        memo_table(A,B,C,D),
        format(S,[47,42,32,126,119,46,32,42,47,126,110],[memo_table(A,B,C,D)]),
        fail.
print_memo_table(_).
print_clauses(S) :-
        memo_table(_,Orig,Head,_),
        format(S,[126,110,47,42,32,126,119,32,58,45,32,126,119,46,32,42,47,126,110],[Orig,Head]),
        spec_clause(Head,C),
        portray_clause(S,C),
        fail.
print_clauses(S) :-
        (   gx_error(_) ->
            write(S,'/* ------------------------------------------ */'),
            nl(S),
            write(S,'/* Error(s) occurred during specialization !! */'),
            nl(S),
            write(S,'/* ------------------------------------------ */'),
            nl(S)
        ;   true
        ).
:- dynamic
        gx_error/1.
:- dynamic
        gx_warning/1.
add_gx_error(Err) :-
        my_assert(gx_error(Err)).
add_gx_error(Err) :-
        my_assert(gx_warning(Err)).
get_logendata_id([_,ID|_LogenData],ID).
set_logendata_id(ID,[H,_OLDID|LogenData],[H,ID|LogenData]) :- !.
set_logendata_id(H,LD,LD2) :-
        write(user_error,failed_set_logendata_id(H,LD,LD2)),
        nl(user_error),
        LD2=LD.
get_logendata_history([History|_LogenData],History).
set_logendata_history(History,[_|LogenData],[CHistory|LogenData]) :-
        copy_term(History,CHistory), !.
set_logendata_history(H,LD,LD2) :-
        write(user_error,failed_set_logendata_history(H,LD,LD2)),
        nl(user_error),
        LD2=LD.
get_memodata_requestor([REQ|_MEMODATA],REQ).
set_memodata_requestor([_OLD|MEMODATA],Req,[Req|MEMODATA]).
get_memodata_id([_Req,ID|_MEMODATA],ID).
set_memodata_id([Req,_|MEMODATA],ID,[Req,ID,MEMODATA]).
init_memodata(Requestor,Parent,[Requestor,Parent]).
test_request(A,Requestor,ResidualCall,__LOGENDATA) :-
        generalise_call([[dynamic]],test(A),GenCall),
        (   find_pattern(rev_term,test(A),ResidualCall,Requestor) ->
            true
        ;   __LOGENDATA=[_,ParentID|_],
            filter_call([[dynamic]],test(A),GenCall,ResidualCall),
            true,
            true,
            init_memodata(pending(Requestor),ParentID,B),
            insert_pattern(rev_term,GenCall,ResidualCall,B)
        ),
        GenCall=test(A).
rev_u(A,B,C,__LOGENDATA) :-
        revacc_u(A,[],B,C,__LOGENDATA).
revacc_u([],A,A,true,__LOGENDATA).
revacc_u([D|E],A,B,C,__LOGENDATA) :-
        revacc_u(E,[D|A],B,C,__LOGENDATA).
test_u(A,(B,C),__LOGENDATA) :-
        length_u(D,10000,B,__LOGENDATA),
        rev_u(D,A,C,__LOGENDATA).
length_u([],0,true,__LOGENDATA).
length_u([_|C],A,B,__LOGENDATA) :-
        A>0,
        D is A-1,
        length_u(C,D,B,__LOGENDATA).
read_from_chars(String,Term) :-
        mktemp('/tmp/readatomXXXXXX',TmpFile),
        open(TmpFile,write,TmpOut),
        display(TmpOut,String),
        display(TmpOut,' .
'),
        close(TmpOut),
        open(TmpFile,read,TmpIn),
        read(TmpIn,Term),
        close(TmpIn).
rev_term_entry(Goal,ResCall) :-
        build_request_call(Goal,crossmodule,ResCall,[[],entry],REQ),
        REQ,
        spec_driver,
        true,
        print_clauses(user).
entry(Goal,ResCall) :-
        rev_term_entry(Goal,ResCall).
main([AtomGoal]) :-
        read_from_chars(AtomGoal,Goal),
        rev_term_entry(Goal,_Res).
my_assert(X) :- 
 	write(X),
	assert(X).
