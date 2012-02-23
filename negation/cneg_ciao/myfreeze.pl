:- module(myfreeze, [myfreeze/2], [attr, dcg]).

:- meta_predicate(myfreeze(?, goal)). 

myfreeze(X, Goal) :- 
	(
	    nonvar(X), !, 
	    call(Goal)
 	;
	    get_attr(X, Fb), !,
	    put_attr(X, (Fb, Goal))     % rescue conjunction
	;
	    put_attr(X, Goal)
	).
        
attr_unify_hook(Fa, Other) :-
	(    
	    nonvar(Other), !,
	    call(Fa) 
	;
	    get_attr(Other, Fb), !,
	    put_attr(Other, (Fa,Fb))    % rescue conjunction
	;
	    put_attr(Other, Fa)         % rescue conjunction
	).

attribute_goals(X) --> 
	[myfreeze:myfreeze(X, G)],
	{get_attr(X, G)}.


attr_portray_hook(G, Var):- 
	display(Var), 
	display('<-myfrozen('), 
	display(G), 
	display(')').