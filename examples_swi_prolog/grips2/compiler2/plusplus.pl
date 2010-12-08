/* PLUSPLUS.PL */


:- op( 500, yfx, user:(++) ).


++( A, B, C ) :-
  append( A, B, C ).


% Make A++B in a GRIPS expression append A to B.

