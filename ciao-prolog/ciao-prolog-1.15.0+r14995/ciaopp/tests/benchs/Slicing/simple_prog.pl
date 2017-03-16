:-module(_,[main/0],[]).

main:-p(X),q(X).

p(a).
p(X):-p(X).

q(a).
q(b).
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

