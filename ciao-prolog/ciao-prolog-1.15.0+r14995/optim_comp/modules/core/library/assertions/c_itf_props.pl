
:- module(c_itf_props,[ moddesc/1, filename/1 ],[ assertions, regtypes ]).

:- doc(title,"Some types and properties related to c_itf").

:- doc(author,"F. Bueno").

% ---------------------------------------------------------------------------
:- prop moddesc(X) + regtype # "@var{X} is a module descriptor.".
% ---------------------------------------------------------------------------

moddesc(X)       :- atom(X).
moddesc(user(X)) :- atom(X).

% ---------------------------------------------------------------------------
:- regtype filename(X) 
   # "@var{X} is an atom describing the name of a file.".
% ---------------------------------------------------------------------------

filename(X) :- 
	atm(X).
