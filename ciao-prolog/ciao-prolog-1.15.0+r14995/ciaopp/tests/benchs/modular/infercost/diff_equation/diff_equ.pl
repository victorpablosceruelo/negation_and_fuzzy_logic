%
%  diff_equ.pl			Nai-Wei Lin			February, 1992
%
%  This file contains the procedures for solving linear difference equations.
%

% Added by PLG (25-Mar-97)
write_equs_general_form([]).
write_equs_general_form([equ(A, B, E)|Eqs]):-
        general_form(E, GE), nl, write(A), write(' '), write(B),
        write(' '), write(GE), nl,
        write_equs_general_form(Eqs).

% End added
% PLG
% solve_typed_diff_equ(+AnalysisType,+DE,+BE,+Var,+ST,+Pred,+Pos,-Sol):
% AnalysisType: can be comp (time complexity) or size. 
% Solve a difference equation DE with boundary equations BE.
% +DE: a normal form expression.
% +BE: a list of equation. An equation is of type equ(N, ISize, Expre),
% where N is a natural number that indicates the clause to which the 
% equation refers to, ISize is the input size (it can be a natural
% number or a variable, and Expre is a normal form expression.
% Var is the (input) variable to which the difference equation depends on.
% It corresponds to the argument number Pos.
% ST symbol table.
% Pred: predicate/arity to which the equation refers to.
   
solve_typed_diff_equ(size,DE,BE,Var,ST,Pred,Pos,Sol) :-
% Added by PLG (25-Mar-97)
        % for debugging PLG
        % debug
        % write('Size diff. equ:'),
        % general_form(DE, GDE), nl, write(GDE), nl,
        % write('Pos: '), write(Pos), nl, 
        % write('Size bound. equ:'),
        % write_equs_general_form(BE),
        % debug
        % write('Reducible to Var: '), write(Var), nl, 
% End added
	(product_diff_equ(DE,Pred,NDE) ->
		(diff_equ_type(NDE,Var,Pred,A1n,A2n,Bn,Dtype),
		 log_base_equs(BE,NBE),
		 solve_one_index_size_diff_equ(Dtype,NBE,Var,A1n,A2n,Bn,
			ST,Pred,Pos,Sol1),
		 exp_solution(Sol1,Sol)
                 % Added by PLG (14-May-97)
                 % debug
                 % write('Size diff. Prod. solution:'),
                 % general_form(Sol, GSol), nl, write(GSol), nl
                 % End added
                );
                % Hook here the determination of a new type of
                % difference equation
		(diff_equ_type(DE,Var,Pred,A1n,A2n,Bn,Dtype),
		 solve_one_index_size_diff_equ(Dtype,BE,Var,A1n,A2n,Bn,
			ST,Pred,Pos,Sol)
                 % Added by PLG (14-May-97)
                 % debug:
                 % write('Size diff. solution:'),
                 % general_form(Sol, GSol), nl, write(GSol), nl
                 % End added
        )).

solve_typed_diff_equ(comp,DE,BE,Var,ST,Pred,Pos,Sol) :-
% Added by PLG (14-May-97)
        % for debugging PLG
        % debug
        % write('Comp diff. equ:'),
        % general_form(DE, GDE), nl, write(GDE), nl,
        % write('Comp bound. equ:'),
        % write_equs_general_form(BE),
        % write('Reducible to Var: '), write(Var), nl,         
% End added
	(product_diff_equ(DE,Pred,NDE) ->
		(diff_equ_type(NDE,Var,Pred,A1n,A2n,Bn,Dtype),
		 log_base_equs(BE,NBE),
		 solve_one_index_comp_diff_equ(Dtype,NBE,Var,A1n,A2n,Bn,
			ST,Pred,Pos,Sol1),
		 exp_solution(Sol1,Sol)
                 % Added by PLG (14-May-97)
                 % debug
                 % write('Comp diff. Prod. solution:'),
                 % general_form(Sol, GSol), nl, write(GSol), nl
                 % End added
                 );
		(diff_equ_type(DE,Var,Pred,A1n,A2n,Bn,Dtype),
		 solve_one_index_comp_diff_equ(Dtype,BE,Var,A1n,A2n,Bn,
			ST,Pred,Pos,Sol)
                 % Added by PLG (14-May-97)
                 % debug
                 % write('Comp diff. solution:'),
                 % general_form(Sol, GSol), nl, write(GSol), nl
                 % End added
        )).
%
%  Determine the type of a difference equation.
%
diff_equ_type(Equ,Var,Pred,A1n,A2n,Bn,first_order) :-
	first_order_diff_equ(Equ,Var,Pred,A1n,A2n,Bn),!.
diff_equ_type(Equ,Var,Pred,A1n,A2n,Bn,second_order) :-
	second_order_diff_equ(Equ,Var,Pred,A1n,A2n,Bn),!.
diff_equ_type(Equ,Var,Pred,A1n,A2n,Bn,higher_order) :-
	higher_order_diff_equ(Equ,Var,Pred,A1n,A2n,Bn),!.
diff_equ_type(Equ,Var,Pred,A1n,A2n,Bn,divide_conquer) :-
	divide_conquer_diff_equ(Equ,Var,Pred,A1n,A2n,Bn),!.
diff_equ_type(Equ,Var,Pred,A1n,A2n,Bn,mutual_size) :-
	mutual_size_diff_equ(Equ,Var,Pred,A1n,A2n,Bn),!.
diff_equ_type(Equ,Var,Pred,A1n,A2n,Bn,implicit_size) :-
	implicit_size_diff_equ(Equ,Var,Pred,A1n,A2n,Bn),!.
diff_equ_type(Equ,Var,Pred,A1n,A2n,Bn,explicit_size) :-
	explicit_size_diff_equ(Equ,Var,Pred,A1n,A2n,Bn),!.
diff_equ_type(Equ,Var,Pred,A1n,A2n,Bn,list_size) :-
	list_size_diff_equ(Equ,Var,Pred,A1n,A2n,Bn),!.
diff_equ_type(_,_,_,_,_,_,no_match).

% solve_diff_equ(first_order,Var,An,_,Bn,Ivalue,Sol)
% Solve a difference equation.
% Ivalue is a list
% PLG
solve_diff_equ(first_order,Var,An,_,Bn,Ivalue,Sol) :-
	solve_fode(Var,Ivalue,An,Bn,Sol).
solve_diff_equ(second_order,Var,A1n,A2n,Bn,Ivalue,Sol) :-
	solve_sode(Var,Ivalue,A1n,A2n,Bn,Sol).
solve_diff_equ(higher_order,Var,A1n,_,Bn,Ivalue,Sol) :-
	solve_hode(Var,Ivalue,A1n,Bn,Sol).
solve_diff_equ(divide-conquer,Var,A1n,A2n,Bn,Ivalue,Sol) :-
	solve_dcde(Var,Ivalue,A1n,A2n,Bn,Sol).
solve_diff_equ(mutual_size,Var,An,_,Bn,Ivalue,Sol) :-
	solve_msde(Var,Ivalue,An,Bn,Sol).
solve_diff_equ(implicit_size,Var,_,_,Bn,Ivalue,Sol) :-
	solve_isde(Var,Ivalue,Bn,Sol).
solve_diff_equ(explicit_size,Var,_,_,Bn,Ivalue,Sol) :-
	solve_esde(Var,Ivalue,Bn,Sol).
solve_diff_equ(list_size,Var,_,_,Bn,Ivalue,Sol) :-
	solve_lsde(Var,Ivalue,Bn,Sol).
% Commented out PLG
% solve_diff_equ(no_match,_,_,_,inf).
% Added by PLG
solve_diff_equ(no_match,_,_,_,Sol):-
      approximation(Approx),
        (Approx == lower -> 
           normal_form(0, Sol)
           ;
           Approx == upper, Sol = inf).
%End added.

