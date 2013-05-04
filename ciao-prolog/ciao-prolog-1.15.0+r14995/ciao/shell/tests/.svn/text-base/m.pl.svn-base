:- use_package([]).

:- set_prolog_flag(multi_arity_warnings, off).

:- data a/1.

main(Args):- catch(a(Args), no, display(no)).

a(Args) :- catch(b(Args), yes, display(yes)), display(' dan').

b([]) :- throw(no).
b([_|_]) :- throw(yes).
