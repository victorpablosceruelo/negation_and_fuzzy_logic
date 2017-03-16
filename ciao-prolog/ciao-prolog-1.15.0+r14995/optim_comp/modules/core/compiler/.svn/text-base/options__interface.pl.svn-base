% Interface to compiler options
% (Opts is a list of sentences that are added to the module)
% note: module__options are given by toplevel__debugger, global__options are given by comp.pl (and comp_js.pl)
% TODO: this interface is very limited

% module__options(+Spec, +Module, -Opts) (det)
:- static multifile module__options/3.
:- '$ctxprj'(module__options/3, []).
% global__options(-Opts) (det)
:- static multifile global__options/1.
:- '$ctxprj'(global__options/1, []).

