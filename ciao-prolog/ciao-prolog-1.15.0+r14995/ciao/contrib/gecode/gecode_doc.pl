:- use_package([assertions,regtypes, isomodes]).

:- doc(nodoc,assertions).
:- doc(nodoc,regtypes).
:- doc(nodoc,isomodes).

:- doc(title, "GECODE-based CLP library").
:- doc(author, "Pablo Chico de Guzm@'{a}n Huerta").
:- doc(copyright,"@include{Copyright.Manuals}").

:- doc(summary, "An implementation of constraint programming over
 finite domains using GeCode.").

:- doc(module,  

"This package is an interface with the GeCode finite domain
solver. Examples can be found in the source and library directories.

@begin{itemize}
@item SEND + MORE = MONEY:
@end{itemize}

@noindent
@begin{verbatim}
@includeverbatim{gecode/examples/smm.pl}
@end{verbatim}

@begin{itemize}
@item Queens:
@end{itemize}

@noindent
@begin{verbatim}
@includeverbatim{gecode/examples/queensfdpl}
@end{verbatim}

").

:- include(gecode_syntax).

:- doc(doinclude, gecode_item/1).

:- prop gecode_item(GECODE_item) + regtype # "@var{GECODE_item} is a finite domain
   entity, i.e., either a finite domain variable or an integer.".

gecode_item(GECODE_item) :- var(GECODE_item).
gecode_item(GECODE_item) :- int(GECODE_item).

:- doc(doinclude, gecode_range/1).

:- prop gecode_range(GECODE_range) + regtype # "@var{GECODE_range} is the range of
   a finite domain entity.".

gecode_range(R) :- list(R, gecode_subrange).

:- doc(doinclude, gecode_subrange/1).

:- prop gecode_subrange/1 + regtype # "A subrange is a pair representing a
   single interval.".

gecode_subrange([Lower|Upper]) :-
	int(Lower),
	int(Upper).

 %% :- doc(doinclude, gecode_store/1).
 
 %% :- prop gecode_store(GECODE_store) + regtype # "@var{GECODE_store} is a
 %% representation of the constraint store of a finite domain entity.".
 %% 
 %% gecode_store(S) :- list(S, gecode_store_entity).
 %% 
 %% :- doc(doinclude, gecode_store_entity/1).
 %% :- prop gecode_store_entity/1 + regtype # "Representation of primitive constraints. ".
 %% 
 %% gecode_store_entity(min).
 %% gecode_store_entity(max).
 %% gecode_store_entity(min_plus_c).
 %% gecode_store_entity(max_plus_c).
 %% gecode_store_entity(min_sub_c).
 %% gecode_store_entity(max_sub_c).
 %% gecode_store_entity(min_mult_c).
 %% gecode_store_entity(max_mult_c).
 %% gecode_store_entity(min_sub_max).
 %% gecode_store_entity(max_sub_min).
 %% gecode_store_entity(min_plus_min).
 %% gecode_store_entity(max_plus_max).
 %% gecode_store_entity(min_mult_min).
 %% gecode_store_entity(max_mult_max).
 %% gecode_store_entity(min_div_max).
 %% gecode_store_entity(max_div_min).

:- pred labeling(Vars) : list(gecode_item) # "Implements the labeling
   process. Assigns values to the input variables @var{Vars}. On exit
   all variables are instantiated to a consistent value. On
   backtracking, the predicate returns all possible assignments. No
   labeling heuristics implemented so far, i.e., variables are
   instantiated in their order of appearance.".

labeling(_L).

:- pred pitm(+V, -MiddlePoint) : gecode_item * int # "Returns in
   @var{MiddlePoint} the intermediate value of the range of
   @var{V}. In case @var{V} is a ground integer value the returned
   value is @var{V} itself.".

pitm(_V, _Point).

 %% :- pred retrieve_range(+Var, -Range) : var * gecode_range # "Returns in
 %% @var{Range} the range of an gecode item @var{Var}.".
 %% 
 %% retrieve_range(_Var, _Range).
 %% 
 %% :- pred retrieve_store(+Var, -Store) : var * gecode_store # "Returns in
 %% @var{Store} a representation of the constraint store of an gecode item
 %% @var{Var}.".
 %% 
 %% retrieve_store(_Var, _Store). 

:- pred glb(+Var, -LowerBound) : gecode_item * int # "Returns in
   @var{LowerBound} the lower bound of the range of @var{Var}.".

glb(_Var, _LowerBound).

:- pred lub(+Var, -UpperBound) : gecode_item * int # "Returns in
   @var{UpperBound} the upper bound of the range of @var{Var}.".

lub(_Var, _UpperBound).

:- pred bounds(+Var, -LowerBound, -UpperBound) : gecode_item * int * int 
   # "Returns in @var{LowerBound} and @var{UpperBound} the lower
      and upper bounds of the range of @var{Var}.".

bounds(_Var, _LowerBound, _UpperBound).

:- regtype dummy(T) # "@var{T} is the zero integer.".
dummy(0).

