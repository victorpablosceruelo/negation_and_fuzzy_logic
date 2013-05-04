% The 'struct' type does not work. Term 
% f(a) is not found to be struct at CT, even though
% a call ?- struct(f(a)). succeeds.            

% In this program both assertions should clearly
% become checked.

% Run:

% ?- module(struct), analyze(eterms), acheck.
% {Loading current module from /home/pawel/Systems/CiaoDE/ciaopp/bugs/struct.pl
% {loaded in 1661.747 msec.}
% }
% {Analyzing /home/pawel/Systems/CiaoDE/ciaopp/bugs/struct.pl
% {preprocessed for plai in 4.999 msec.}
% {analyzed by plai using eterms in 4.999 msec.}
% }
% {Checking assertions of /home/pawel/Systems/CiaoDE/ciaopp/bugs/struct.pl
% {ERROR (ctchecks_pred): False assertion:
% :- check success p(X)
%         => basic_props:struct(X).

% because on success struct:p(X) :

% [eterms] : rt0(X) 
% with: rt0 ::= ^(f(^(a)))
% }
% {NOTE (ctchecks_pred): Assertion:
% :- check success p(X)
%         => basic_props:gnd(X).

% has changed to
% :- checked success p(X)
%         => basic_props:gnd(X).

% }
% {No mode analysis available for checking}
% {assertions checked in 66.99 msec.}
% }

% yes
% ?- 

:- module(_,[p/1],[assertions,regtypes]).

:- success p(X) => struct(X). % becomes false (not ok)
:- success p(X) => gnd(X).    % becomes checked (ok)

p(f(a)).
