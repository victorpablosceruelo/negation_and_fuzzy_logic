
:- multifile 
        verify_attribute/2,
        combine_attributes/2,
	portray_attribute/2,
	portray/1.


p(X) :- 
	write(X), nl,
	attach_attribute(X, unknown), 
	write(X), nl,
	detach_attribute(X),
	write(X), nl,
	q(X).
q(a).