% Test case demonstration scenario %
% GUI %
% C-c C-M
% But still you have to execute this command to 
% activate interval analysis
set_pp_flag(ctchecks_intervals,on).


% Command Line %
%calling Ciao PP
use_module(ciaopp(ciaopp)).
%set ciaopp  flag to show the result of interval
set_pp_flag(ctchecks_intervals,on).
%set ciaopp  flag to show verbose result
set_pp_flag(verbose_ctchecks,on).
%examine/set ciaopp  flag to control unknown result (check)
current_pp_flag(ass_not_stat_eval,STAT).
set_pp_flag(ass_not_stat_eval,off). %other values: warning, error

%load a program file and perform analysis
%Fibonacci
module('FULLPATH/fib.pl').
analyze(eterms),analyze(shfr),analyze(nfg),analyze(steps_ualb).

%Naive reverse
module('FULLPATH/nrev.pl').
analyze(eterms),analyze(shfr),analyze(nfg),analyze(steps_ualb).

%Quick sort with sum cost function
module('FULLPATH/qsort.pl').
analyze(eterms),analyze(shfr),analyze(nfg),analyze(steps_ualb).

%resource analysis
%Reverse List
module('FULLPATH/rev_resource.pl').
analyze(eterms),analyze(shfr),analyze(nfg),analyze(resources).

%Client server
module('FULLPATH/trunk/ciaopp/resources/examples/general/client.pl'),
analyze(eterms),analyze(shfr),analyze(nfg),analyze(resources).

%check assertion, after each series of analyses is performed
%acheck.
