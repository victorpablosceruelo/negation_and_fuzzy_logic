:- module(fixpo_dx_check_basic, [advance_in_body/3], []).

%-------------------------------------------------------------------------
% advance_in_body(+,+,-)
% advance_in_body(Ch_Key,OldBody,NewBody)
%  NewBody is OldBody where the literals that do not need to be 
% re-analyzed are removed. Then we can use entry_to_exit with NewBody
%-------------------------------------------------------------------------
advance_in_body(Ch_Key,g(Ch_Key,Vars,Info,SgKey,Sg),NewBody):-!,
	NewBody = g(Ch_Key,Vars,Info,SgKey,Sg).
advance_in_body(Ch_Key,(g(Ch_Key,Vars,Info,SgKey,Sg),Goals),NewBody):-!,
	NewBody = (g(Ch_Key,Vars,Info,SgKey,Sg),Goals).
advance_in_body(Ch_Key,(_,Goals),NewBody):-
	advance_in_body(Ch_Key,Goals,NewBody).
