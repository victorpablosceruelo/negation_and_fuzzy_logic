/*
After analysis with eterms, the output procedure breaks.


?- module(outbug), analyze(eterms), output.
{Loading current module from /home/pawel/Systems/CiaoDE/ciaopp/bugs/outbug.pl
{loaded in 2726.585 msec.}
}
{Analyzing /home/pawel/Systems/CiaoDE/ciaopp/bugs/outbug.pl
{preprocessed for plai in 6.0 msec.}
{analyzed by plai using eterms in 5.999 msec.}
}
{ERROR (api_internal_mod): Internal error: failed to write 
 as('$ref'(21279440,776),true,pred,'outbug:p'(_115452),[],
    ['basic_props:term'(_115452)],[['basic_props:list'(_115452,2)]],[[]],
    ['basic_props:term'(_115452)],[['basic_props:list'(_115452,2)]],no,
    loc(no,0,0,outbug),[],_115447)}
{written file /home/pawel/Systems/CiaoDE/ciaopp/bugs/outbug_eterms_co.pl}


HINT: Replace 2 by 'dos', and it works. 	
*/

:- module(_, [p/1], [assertions]).

p([]).
p([2|R]) :-
        p(R).




