:- if((defined(optim_comp), backend(js_backend))).
:- if(( target_platform(v8) ; target_platform(nodejs) )).
:- include(.(testsuite_run)).
:- else.
:- include(.(testsuite_ui)).
:- endif.
:- else.
:- include(.(testsuite_run_ciao)).
:- endif.

