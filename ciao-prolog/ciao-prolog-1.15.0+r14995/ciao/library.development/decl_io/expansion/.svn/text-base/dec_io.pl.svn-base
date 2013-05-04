:- package(dec_io).

:- op(1200, xfx, [->>]).
:- op(25, fy, 'dec').

:- load_compilation_module(dec_io_expansion).
:- add_sentence_trans(dec_io_translation/3, 750).  % TODO: Probably not right priority

% TODO: Priorities are not enough to make it work with other
%       translations, such as fsyntax. See "Modular Extensions for
%       Modular (Logic) Languages (LOPSTR'11)" paper for details.

