% TODO: this test should check that we can use_module(di, [a/1]),
% a(X), then the same for b/1 and then a(X), b(X), everything works...
% and if we unexport a/1 here in this file and do use_module(di), only
% b/1 should be visible

% TODO: (2) this test should check that we can use_module(di, [a/1]),
% a(X), then the same for b/1 and then a(X), b(X), everything works...
% and if we unexport a/1 here in this file and do use_module(di,
% [b/1]), only b/1 should be visible

:- module(_, [], []).
:- export(a/1).
a(1).
:- export(b/1).
b(1).
