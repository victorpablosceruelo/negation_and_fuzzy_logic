:- module(check_links, [main/1,check_links/2,check_links/3],[]).

:- use_package(pillow).

:- use_module(library(lists)).
:- use_module(library(write)).

:- set_prolog_flag(multi_arity_warnings, off).

main([URL]) :- !,
        write('Counting links and reporting bad links from '), 
        write(URL), nl, 
        check_links(URL, AllLinks, BadLinks),
        length(AllLinks, N),
        write('Checked '), write(N), write(' links'), nl,
        report_bad_links(BadLinks).
main(_) :-
        error(['Correct usage: check_links <URL>']).

check_links(URL, BadLinks):- check_links(URL, _, BadLinks).

check_links(URL, AllLinks, BadLinks) :-
        url_info(URL,URLInfo),
        fetch_url(URLInfo,[],Response),
        member(content_type(text,html,_),Response),
        member(content(Content),Response),
        html2terms(Content,Terms),
        check_source_links(Terms,URLInfo,[],AllLinks,[],BadLinks).

check_source_links([],_,AL,AL,BL,BL).
check_source_links([E|Es],BaseURL,AL0,AL,BL0,BL) :-
        check_source_links1(E,BaseURL,AL0,AL1,BL0,BL1),
        check_source_links(Es,BaseURL,AL1,AL,BL1,BL).

check_source_links1(env(a,AnchorAtts,_),BaseURL,AL0,AL,BL0,BL) :-
        member((href=URL),AnchorAtts), !,
        check_link(URL,BaseURL,AL0,AL,BL0,BL).
check_source_links1(env(_Name,_Atts,Env_html),BaseURL,AL0,AL,BL0,BL) :- !,
        check_source_links(Env_html,BaseURL,AL0,AL,BL0,BL).
check_source_links1(_,_,AL,AL,BL,BL).

check_link(URL,BaseURL,AL0,[URL|AL0],BL0,BL) :-
        url_info_relative(URL,BaseURL,URLInfo), !,
        fetch_url_status(URLInfo,Status,Phrase),
        ( Status \== success ->
          atom_codes(P,Phrase),
          atom_codes(U,URL),
          BL = [badlink(U,P)|BL0]
        ; BL = BL0
        ).
check_link(_,_,AL,AL,BL,BL).

fetch_url_status(URL,Status,Phrase) :-
        fetch_url(URL,[head,timeout(20)],Response), !,
        member(status(Status,_,Phrase),Response).
fetch_url_status(_,timeout,timeout).

report_bad_links([]).
report_bad_links([badlink(U,P)|BLs]) :-
        message(['URL ',U,' : ',P]),
        report_bad_links(BLs).
