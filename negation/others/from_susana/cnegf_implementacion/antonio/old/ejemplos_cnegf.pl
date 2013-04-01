%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          EJEMPLOS QUE FUNCIONAN PARA CENGF.PL            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


?- no_member(X,[1,2]).

attach_attribute(X,formula(X,[[X/2,X/1]])) ? .

no

?- no_member(s(X),[s(1)]).

attach_attribute(X,formula(X,[[X/1]])) ? .

no