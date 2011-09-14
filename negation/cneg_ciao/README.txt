
El problema de tratar de implementar lo q propone Stuckey es q en ejemplos como los siguientes no funciona:

p(X) :- X \== A, A = b, X = c

neg_p(X) :- X = A ; ( X \== A, ( A \== b ; ( A = b, X \== c )))

notese q X = A devuelve X libre sin ponderar nada más. ERROR.
Tratar de usar atributos de manera 

