:- module(_, _, [assertions, basicmodes]).

:- pred g(+) :: basic_props:int. % error: Property basic_props/1 
                                 % undefined in source
g(3).

:- pred g2(+) :: (basic_props:int). % error: Property basic_props/1 
                                    % undefined in source
g2(3).

:- pred h(+) :: (basic_props:int) : true.

h(3).
