% interface to pcre.h

%:- '$native_weak_inline'(include('pcre.h')).
:- '$improlog_begin'.
:- lowinclude_foreign(predef_h, 'pcre.h').

:- foreigntype(constchar, 'const char'). % TODO: this is a kludge

% TODO: define as a array to char (with some properties)
% :- lowtype(cstring).
:- type(constcstring/1) + equiv.
constcstring(T) :- T = ~mut(constchar).

:- foreigntype(pcre, 'pcre').
:- foreigntype(pcre_extra, 'pcre_extra').

:- foreigntype(size, 'size_t'). % TODO: include from other file

:- pred pcre_compile/6 + foreignfun([constcstring,intmach,mut(constcstring),mut(intmach),mut(char)], ref1(pcre), 'pcre_compile').
% TODO: do not fix the 3 size in the array!
:- pred pcre_exec/9 + foreignfun([ref1(pcre),ref1(pcre_extra),mut(char),size,intmach,intmach,ref0(array(ref0(mut(intmach)),3)),intmach], intmach, 'pcre_exec').
:- pred pcre_study/4 + foreignfun([ref1(pcre),intmach,mut(constcstring)], ref1(pcre_extra), 'pcre_study').

:- pred pcre_caseless/1 + foreigncons(intmach, 'PCRE_CASELESS').

:- '$improlog_end'.

