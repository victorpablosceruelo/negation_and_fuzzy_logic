:-(module(_169947,[/(neq_list,2),/(qsort,2),/(finite,2)],ciaopp)) .
:-(use_package('.'(cnegf))) .
:-(new_declaration(/(comment,2))) .
:-(op(975,xfx,=>)) .
:-(op(978,xfx,::)) .
:-(new_declaration(/(decl,1))) .
:-(op(1150,fx,decl)) .
:-(new_declaration(/(decl,2))) .
:-(op(1150,xfx,decl)) .
:-(new_declaration(/(pred,1))) .
:-(op(1150,fx,pred)) .
:-(new_declaration(/(pred,2))) .
:-(op(1150,xfx,pred)) .
:-(new_declaration(/(prop,1))) .
:-(op(1150,fx,prop)) .
:-(new_declaration(/(prop,2))) .
:-(op(1150,xfx,prop)) .
:-(new_declaration(/(modedef,1))) .
:-(op(1150,fx,modedef)) .
:-(new_declaration(/(calls,1))) .
:-(op(1150,fx,calls)) .
:-(new_declaration(/(calls,2))) .
:-(op(1150,xfx,calls)) .
:-(new_declaration(/(success,1))) .
:-(op(1150,fx,success)) .
:-(new_declaration(/(success,2))) .
:-(op(1150,xfx,success)) .
:-(new_declaration(/(comp,1))) .
:-(op(1150,fx,comp)) .
:-(new_declaration(/(comp,2))) .
:-(op(1150,xfx,comp)) .
:-(new_declaration(/(entry,1))) .
:-(op(1150,fx,entry)) .
:-(include(library(assertions))) .
:-(use_module(library('assertions/native_props'))) .
:-(include(library(nativeprops))) .
:-(redefining(/(indep,1))) .
:-(redefining(/(indep,2))) .
:-(op(950,xf,[&])) .
:-(op(975,xfx,[=>])) .
:-(use_module(library('andprolog/andprolog_rt'))) .
:-(include(library(cges))) .
:-(new_declaration(/(comment,2))) .
:-(op(975,xfx,=>)) .
:-(op(978,xfx,::)) .
:-(new_declaration(/(decl,1))) .
:-(op(1150,fx,decl)) .
:-(new_declaration(/(decl,2))) .
:-(op(1150,xfx,decl)) .
:-(new_declaration(/(pred,1))) .
:-(op(1150,fx,pred)) .
:-(new_declaration(/(pred,2))) .
:-(op(1150,xfx,pred)) .
:-(new_declaration(/(prop,1))) .
:-(op(1150,fx,prop)) .
:-(new_declaration(/(prop,2))) .
:-(op(1150,xfx,prop)) .
:-(new_declaration(/(modedef,1))) .
:-(op(1150,fx,modedef)) .
:-(new_declaration(/(calls,1))) .
:-(op(1150,fx,calls)) .
:-(new_declaration(/(calls,2))) .
:-(op(1150,xfx,calls)) .
:-(new_declaration(/(success,1))) .
:-(op(1150,fx,success)) .
:-(new_declaration(/(success,2))) .
:-(op(1150,xfx,success)) .
:-(new_declaration(/(comp,1))) .
:-(op(1150,fx,comp)) .
:-(new_declaration(/(comp,2))) .
:-(op(1150,xfx,comp)) .
:-(new_declaration(/(entry,1))) .
:-(op(1150,fx,entry)) .
:-(use_module('.'(neg))) .
:-(data(/(finite,2))) .
:-(success(trust,neg(_156785))) .
:-(pred(true,=>(:(neq_list(_156946,_156973),','(list(_156946,num),list(_156973,num))),+(','(list(_156946,num),list(_156973,num)),','(possibly_fails(neq_list(_156946,_156973)),covered(neq_list(_156946,_156973))))))) .
:-(pred(true,=>(:(neq_list(_157657,_157684),','(ground(_157657),ground(_157684))),','(ground(_157657),ground(_157684))))) .
:-(entry(:(neq_list(_158072,_158099),','(list(_158072,num),','(list(_158099,num),','(ground(_158072),ground(_158099))))))) .
:-(neq_list(_158468,_158495),','(qsort(_158468,_158566),neg(qsort(_158495,_158566)))) .
:-(pred(true,=>(:(qsort(_158816,_158843),','(list(_158816,num),var(_158843))),+(','(list(_158816,num),','(list(_158843,num),','(size_ub(_158816,length(_158816)),size_ub(_158843,-(exp(2,length(_158816)),1.0))))),','(not_fails(qsort(_158816,_158843)),','(covered(qsort(_158816,_158843)),steps_ub(qsort(_158816,_158843),-(+(+(sum($(j),1,length(_158816),*(exp(2,-(length(_158816),$(j))),$(j))),*(exp(2,-(length(_158816),1)),length(_158816))),*(2.0,exp(2,length(_158816)))),1.0)))))))) .
finite(qsort,2) .
:-(pred(true,=>(:(qsort(_160562,_160591),','(ground(_160562),','(var(_160591),mshare([[_160591]])))),','(ground(_160562),ground(_160591))))) .
:-(entry(:(qsort(_161041,_161070),','(list(_161041,num),','(var(_161070),ground(_161041)))))) .
:-(qsort([_161376|_161405],_161436),','(partition(_161405,_161376,_161541,_161570),','(qsort(_161570,_161639),','(qsort(_161541,_161708),append(_161708,[_161376|_161639],_161436))))) .
qsort([],[]) .
:-(pred(true,=>(:(partition(_162134,_162163,_162192,_162221),','(list(_162134,num),','(num(_162163),','(var(_162192),var(_162221))))),+(','(list(_162134,num),','(num(_162163),','(list(_162192,num),','(list(_162221,num),','(size_ub(_162134,length(_162134)),','(size_ub(_162163,int(_162163)),','(size_ub(_162192,length(_162134)),size_ub(_162221,length(_162134))))))))),','(not_fails(partition(_162134,_162163,_162192,_162221)),','(covered(partition(_162134,_162163,_162192,_162221)),steps_ub(partition(_162134,_162163,_162192,_162221),+(length(_162134),1)))))))) .
finite(partition,4) .
:-(pred(true,=>(:(partition(_163978,_164007,_164036,_164065),','(ground(_163978),','(ground(_164007),','(var(_164036),','(var(_164065),','(mshare([[_164036],[_164065]]),indep(_164036,_164065))))))),','(ground(_163978),','(ground(_164007),','(ground(_164036),ground(_164065))))))) .
partition([],_164820,[],[]) .
:-(partition([_164961|_164990],_165021,[_164961|_165074],_165105),','(<(_164961,_165021),','(!,partition(_164990,_165021,_165074,_165105)))) .
:-(partition([_165450|_165479],_165510,_165539,[_165450|_165592]),','(>=(_165450,_165510),partition(_165479,_165510,_165539,_165592))) .
:-(pred(true,=>(:(append(_165956,_165985,_166014),','(list(_165956,num),','(list1(_165985,num),var(_166014)))),+(','(list(_165956,num),','(list1(_165985,num),','(list1(_166014,num),','(size_ub(_165956,length(_165956)),','(size_ub(_165985,length(_165985)),size_ub(_166014,+(length(_165985),length(_165956)))))))),','(not_fails(append(_165956,_165985,_166014)),','(covered(append(_165956,_165985,_166014)),steps_ub(append(_165956,_165985,_166014),+(length(_165956),1)))))))) .
finite(append,3) .
:-(pred(true,=>(:(append(_167558,_167587,_167616),','(ground(_167558),','(ground(_167587),','(var(_167616),mshare([[_167616]]))))),','(ground(_167558),','(ground(_167587),ground(_167616)))))) .
append([],_168159,_168159) .
:-(append([_168297|_168326],_168357,[_168297|_168410]),append(_168326,_168357,_168410)) .
:-(pred(true,=>(:(finite(_168676,_168705),','(term(_168676),term(_168705))),','(term(_168676),term(_168705))))) .
:-(pred(true,:(finite(_169093,_169122),+(mshare([[_169093],[_169093,_169122],[_169122]]),fails(finite(_169093,_169122)))))) .
:-(regtype(/(t217,1))) .
:-(t217(qsort(_169671,_169700)),','(list(_169671,num),list(_169700,num))) .
