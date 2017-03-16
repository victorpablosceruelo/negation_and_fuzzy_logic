:- module(testsuite_1, [], condcomp).

:- compilation_fact(test_clp).
%:- compilation_fact(test_search).
%:- compilation_fact(test_rec).
%:- compilation_fact(test_arith).

:- include(.(testsuite_common)).

:- if((defined(optim_comp), backend(js_backend))).
:- if(( target_platform(v8) ; target_platform(nodejs) )).
:- else.
% TODO: share
:- inline_resource('testsuite_1.html', "
<html>
<head>
<title></title>
<script type=\"application/javascript\" src=\"testsuite_1.out.js\"></script>
</head>
<body onload=\"__ciao_start__()\"></body>
</html> 
").
:- endif.
:- endif.
