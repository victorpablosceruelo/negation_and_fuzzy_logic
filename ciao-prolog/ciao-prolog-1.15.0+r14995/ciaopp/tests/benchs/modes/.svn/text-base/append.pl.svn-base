
:- module(append,[append/3],[assertions,nativeprops]).

:- entry append(As,Bs,Cs)
	: ( var(As), var(Bs), var(Cs)
	  , indep([[As,Bs],[As,Cs],[Bs,Cs]])
	  ).

append([],Y,Y).
append([X|Xs],Ys,[X|Zs]):- append(Xs,Ys,Zs).


%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

