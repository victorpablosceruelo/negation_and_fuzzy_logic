% A test for data structures
%
% Author: Jose F. Morales

:- module(_, [main/0], [compiler(complang)]).

% a list
:- class mylist {
    :- attr head :: single.
    :- attr tail :: pair(class(mylist)).

    :- constructor new/1.
    new(H) :- '$self' <- mylist(_, _), ~head = H, ~tail = nil.

    :- constant get_tail/1.
    get_tail := ~tail.
}.

main :-
	reading_test_with_method,
	reading_test,
	writting_test.

reading_test_with_method :-
	% TODO: get_tail.get_tail does not work
	%       (it cannot work with 'pair'...)
	push_pair(lst, class(mylist), mylist(1,mylist(2,mylist(3,nil)))) '$ctx_on' (
	  display(~lst), nl,
	  display(lst.get_tail), nl,
	  display(lst.tail.get_tail), nl
        ).

reading_test :-
	push_pair(lst, class(mylist), mylist(1,mylist(2,mylist(3,nil)))) '$ctx_on' (
	  display(~lst), nl,
	  display(lst.tail), nl,
	  display(lst.tail.tail), nl
        ).

writting_test :-
	push_pair(lst, class(mylist), _) '$ctx_on' (
          lst.new(1),
	  display(~lst), nl,
          lst.tail.new(2),
	  display(~lst), nl,
          lst.tail.tail.new(3),
	  display(~lst), nl,
          lst.new(a),
	  display(~lst), nl
        ).

