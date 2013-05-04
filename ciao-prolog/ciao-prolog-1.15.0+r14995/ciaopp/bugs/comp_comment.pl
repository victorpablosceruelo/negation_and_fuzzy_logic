/*
?- module('~/Systems/CiaoDE/ciaopp/bugs/comp_comment').
{Loading current module from /home/pawel/Systems/CiaoDE/ciaopp/bugs/comp_comment.pl
{Reading /home/pawel/Systems/CiaoDE/ciaopp/bugs/comp_comment.pl
ERROR: (lns 1-15) Module comp_comment already defined in source /home/pawel/tmp/comp_comment
}
{In /home/pawel/Systems/CiaoDE/ciaopp/bugs/comp_comment.pl
ERROR (assrt_norm): (lns 16-17) comp assertion syntax (2) in module 
 comp_comment: 
 p/1:var+iso#[67,105,97,111,80,80,32,100,111,101,115,32,110,111,116,32,108,
              105,107,101,32,109,101,46]
}
{In /home/pawel/Systems/CiaoDE/ciaopp/bugs/comp_comment.pl
ERROR (assrt_norm): (lns 18-19) success assertion syntax (2) in module 
 comp_comment: 
 =>(p/1,
    var#[78,101,105,116,104,101,114,32,108,105,107,101,115,32,109,101,32,58,
         45,40])
}
{loaded in 4952.247 msec.}
}

yes
{trace}
?- 
*/
:- module(_,_,[assertions]).

:- comp p/1: var + iso # "CiaoPP does not like me.".

:- success p/1  => var # "Neither likes me :-(".

:- impl_defined(p/1).
