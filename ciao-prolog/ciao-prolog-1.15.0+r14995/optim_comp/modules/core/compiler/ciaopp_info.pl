%:- package(ciaopp_info).
% This package translates Ciaopp output so that ptoc understand it
% TODO: this is a kludge... and needs special flag in Ciaopp

:- add_goal_trans(ciaopp_info__goal/3, 750).
:- load_compilation_module(compiler(ciaopp_info__tr)).

