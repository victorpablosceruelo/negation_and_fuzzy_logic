:- multifile menu_default/3.
:- multifile menu_opt/6.
:- meta_predicate menu_opt( ? , ? , ? , pred(1) , pred( 0 ) , pred(2) ).

% :- discontiguous pp_flag/2.
% :- discontiguous current_pp_flags/2.
% :- discontiguous valid_flag_values/2.

% :- multifile pp_flag/2 , 
% 	     pp_flag/1,
% 	     current_pp_flags/2,
% 	     valid_flag_values/2.

:- set_prolog_flag( multi_arity_warnings , off ).

true( _ ).
true( A , A ).

:- set_prolog_flag( multi_arity_warnings , on  ).

%:- use_module( preprocess_flags_ ).

:- use_package( argnames ).

:- argnames menu_opt( menu , flag , message , guard , pre , post ).
