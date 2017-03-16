:- use_module(library(persdb_sql(pl2sql_modifications(pl2sql)))).
:- use_module(library(strings)).

:- multifile [relation/3,attribute/4].
:- data [relation/3,attribute/4].

relation(product,3,'PRODUCT').
attribute(1,'PRODUCT','ID',int).
attribute(2,'PRODUCT','QUANTITY',int).
attribute(3,'PRODUCT','NAME',string).

main :- 
     pl2sqlstring( f(L,K), 
          ((product(L,N,a); product(L,N,b)),
	   \+ product(2,3,b), 
	   L + 2 > avg(Y, Z^product(Z,Y,a)),
	   K is N + max(X, product(X,2,b))
           ), T),
     write_string(T).

main2 :- 
     pl2sqlterm( f(L,K), 
          ((product(L,N,a); product(L,N,b)),
	   \+ product(2,3,b), 
	   L + 2 > avg(Y, Z^product(Z,Y,a)),
	   K is N + max(X, product(X,2,b))
           ), T),
     write(T).
%     display(T).
     
%%     printqueries(T).
