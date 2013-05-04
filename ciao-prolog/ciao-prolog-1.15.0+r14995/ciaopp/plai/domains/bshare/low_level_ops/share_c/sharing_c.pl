:- module(_,_,
%	[cshare_amgu/4], 
	[foreign_interface]).

:- use_module(domain(share_amgu_aux), [peel_equations/3]).
:- use_module(domain(share), [share_sort/2]).
:- use_module(domain(bshare(bshare)), [right_side_to_bits/3]).
:- use_module(domain(bshare(bshare_utils))).
:- use_module(domain(bshare(config_paths))).

:- doc(title, "A C Set-Sharing implementation").
:- doc(author, "Jorge Navas").
:- doc(module, "This module provides all abstract operations
           implemented in C of the Set-sharing domain defined by Jacobs and
           Langen.").

:- doc(bug,"So far, only the gluecode for amgu has been implemented").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
amgu(_X,_Ts,'$bottom','$bottom').
amgu(X,Ts,(Sh,Vars),Sh0):-
     files_path(File),
     shvars2shbits(Sh,Vars,Sh1),
     write_neg_db(File,Sh1),
     atom_codes(File,FileS),
     code_left_side(X,Vars,X0),
     code_right_side(Ts,Vars,Ts0),
     cshare_amgu_c(FileS,X0,Ts0),
     read_negdb(File,Vars,(Sh_bits,_)),
     bits_to_vars(Sh_bits,Vars,ShE_u),
     delete_empty_set(ShE_u,Sh0_u),
     share_sort(Sh0_u,Sh0).

amgu_file(X,Ts,Vars):-
     files_path(File),
     atom_codes(File,FileS),
     code_left_side(X,Vars,X0),
     code_right_side(Ts,Vars,Ts0),
     cshare_amgu_c(FileS,X0,Ts0).

delete_empty_set([],[]).
delete_empty_set([[]|Ss],Ss).
delete_empty_set([S|Ss],[S|Rs]):-
	delete_empty_set(Ss,Rs).

code_left_side(X,Vars,NX):-
     vars_to_bits([X],Vars,V),
     binlist_to_atm(V,V0),
     NX = V0.
    % atom_codes(V0,NX).

code_right_side(Ts,Vars,NTs):-
     right_side_to_bits(Ts,Vars,T0),
     T0 = NTs.
    % atom_codes(T0,NTs).
	
:- true pred cshare_amgu_c(in(F), in(X), in(T)) :: string * atm * atm
   + foreign(sharing_amgu_prolog).

:- use_foreign_source(sharing_c).
	
% :- true pred cshare_amgu__c(in(Sh), in(X), in(T),go(ASub)) :: 
% 	string * int * int * string + (foreign(ciao_sharing_amgu),returns(ASub)) 
% # "Unifies in @var{ASub} the variable @var{X} with the term @var{T} given
% the abstract substitution @var{Sh}".
% :- use_foreign_library('csharing_c',m).





