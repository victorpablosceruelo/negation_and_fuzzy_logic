p.
p :- throw(b)

q :- catch(p, A, write(error(A))),
     r(c).

r(A) :- throw(A_), write(end_r).

s(A) :- catch(p, A, write(error(A))), write(end_s).

% | ?- catch(q, X, true).
% 
% X = c ? ;
% 
% no

% | ?- X=b, s(X).
% end_s
% X = b ? ;
% error(b)end_s
% X = b ? ;
% 
% no

% | ?- catch(s(X), E, write(err(E))).
% end_s
% E = _53,
% X = _35 ? ;
% error(b)end_s
% E = _53,
% X = b ? ;
% 
% no
% | ?- catch(s(b), E, write(err(E))).
% end_s
% E = _45 ? ;
% error(b)end_s
% E = _45 ? ;
% 
% no
% | ?- catch(s(c), E, write(err(E))).
% end_s
% E = _45 ? ;
% err(b)
% E = b ? ;
% 
% no
