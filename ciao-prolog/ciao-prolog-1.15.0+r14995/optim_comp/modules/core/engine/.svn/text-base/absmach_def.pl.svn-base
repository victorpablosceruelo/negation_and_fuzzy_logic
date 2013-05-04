% ---------------------------------------------------------------------------
% Abstract machine definition in ImProlog

% Author: Jose F. Morales (based on the original code in C)

% This is the abstract machine definition for optimcomp. This source
% is compiled using the ImProlog compiler (ptoc__impcomp) to efficient
% C code.

% ---------------------------------------------------------------------------
% Development notes

% To handle compiler incompatibilities during development it is
% sometimes necessary to introduce conditional code such as:
%
% :- if('$with_compiler_version'(47)).
% :- else.
% :- endif.
%
% The current compiler version is stored in frontend:compiler_version/1.

% TODO: the following syntax definitions should be shared with complang
% TODO: Use ~ instead of @ (this change is hard -- it may need :- meta_predicate foo(pair), etc.)

% ---------------------------------------------------------------------------
% Important note on syntax:
%
%   ~(_) and @(_) are functors, so "~(a).b" and "~a.b" are not equivalent!
%   Use "~ a.b" and "@ a.b" instead.
%
% TODO: Add a Prolog flag to forbid the ~ prefix operator to be used
%   as a functor? (except if it is written like '~'(_)) -- also for @,
%   or generalize for any operator.
% 
% ---------------------------------------------------------------------------

:- op(50, fx, [(~)]).
:- op(50, fx, [(@)]).
:- op(980, xfx, [(<-)]). % priority between (::) and (,)
:- op(1100, xfy, [('|')]).
:- op(1150, xfx, [(:=)]).

% TODO: merge with complang_mini.pl
:- op(1150, fx, [(pred)]).
:- op(1150, fx, [(attr)]).
:- op(1150, fx, [(mut)]).
:- op(1150, fx, [(abstract_class)]).
:- op(1150, fx, [(class)]).
:- op(1150, fx, [(union)]).
:- op(1150, fx, [(mixin)]).
:- op(978, xfx,(::)).

:- push_prolog_flag(read_hiord, on).

% Infix dot as method/attribute accessor
:- op(40, yfx, [(.)]).
:- push_prolog_flag(read_infix_dot, on).

% Curly blocks
:- push_prolog_flag(read_curly_blocks, on).

% Postfix blocks enable X[I] syntax
:- push_prolog_flag(read_postfix_blocks, on).
% TODO: the following operator definitions allows postfix read of
%       [...] and {...}  Use other way to specify it?

% note: operator name inside a list, to avoid confusing it as an empty
% list of opcodes
:- op(40, yf, ['[]']).
:- op(40, yf, ({})).

:- fun_eval((+)/1).
:- fun_eval((-)/1).
:- fun_eval(('@')/1).
:- fun_eval((*)/2).
:- fun_eval((/)/2).
:- fun_eval((+)/2).
:- fun_eval((-)/2).
:- fun_eval((/\)/2).
:- fun_eval((\/)/2).
:- fun_eval((mod)/2).
:- fun_eval((<<)/2).
:- fun_eval((>>)/2).

% Include at the beginning generated header .h file
:- lowinclude(predef_h, engine(absmach_predef)).
% Include at the end of the generated header .h file
:- lowinclude(postdef_h, engine(absmach_postdef)).

% TODO: use $predabs_with_props(...) to insert '$unfold'(Goal) or
% '$subpr'(Goal) automatically.

% TODO: generate predicate versions for treat_tuple_...
%
%   To do that, generalize the prop(unfold), prop(subpr),
%   etc. annotations. In this way:
%     - versionarg(I): a version for each value of the Ith argument
%     - subpr: a version for each call in a 'subcontext', expanded
%       inside each function
%     - unfold: a version for each call
%     - (otherwise) a single version for all calls [right now, only
%       with lowpred, etc.]

% TODO: make sure that members of imptypes for dependent structures
% are only available for the corespondent's typekey
% e.g. (numtagged(T), A = T.ptr) must not compile

% TODO: allow '$trust'(hvatagged(T)) (equivalent to '$trust_type'(T, hvatagged))

% TODO: port copy_term/2, ground/1, subsumes/2, etc. to ImProlog (also
% assert, garbage collection, etc.) since those share a lot of code.

% TODO: add non-materialized fields for a 'deref' property in tagged/1?
%   E.g. a field that is not encoded, but can be deduced from other
%   properties.

% TODO: search for fast checks for integer overflow (even if those
% require x86 assembler code)

% ---------------------------------------------------------------------------
% Abstract machine generation options
% TODO: enable/disable with compiler options

:- lowmacrofact('$use_opt'/1).

% TODO: with varops (Sometimes) GOES FASTER, sometimes slower execs
%   are a bit bigger anyway... but no loop ins are needed (it used
%   enable ins 2)
% note: it requires opt 'omerg'
% TODO: make it work even without omerg (there is a problem with 1
% unfolding when the rest is not specified)

% TODO: See 'condcomp' package, find a common style

:- decl_opt(varops) # "Enable instructions whose operands are a list
   or a counter (and require an internal loop)".

% TODO: experimental results indicate that it does not accelerate too much
:- decl_opt(varops2) # "Enable optional mergings for varops option".

:- decl_opt(iblt) # "Enable instructions for builtins".

:- decl_opt(omerg) # "Enable optim merging".

:- decl_opt(joinconts) # "Use special definitions for deref+switch
   (joins continuations)".

% TODO: incomplete (if not specified tries to unfold everything; but it is not very clean at this moment)
:- decl_opt(urules) # "Use user rules to control unfolding".

% TODO: disabled mode is a kludge
:- decl_opt(specmode) # "Specialization of read/write mode (generates
   two versions for each instruction)".

% Default option set includes:
%'$use_opt' := varops | omerg | joinconts | urules | specmode.

% All options:
%'$use_opt' := varops | iblt | omerg | tagswbit | joinconts | urules | specmode.

% ---------------------------------------------------------------------------
% Other options

% TODO: not working!! 
:- decl_opt(atomgc) # "garbage collection of atoms".
'$use_opt'(atomgc).

% Invalidate the value of local_top whenever the choice or the frame
% changes and recalculate the real local_top when really needed (if
% marked as invalid) or reuse the current one (if not marked as
% invalid) (recalculate seems to give better results)
% TODO: revisit!!! unsure what is the best option
:- decl_opt(lazy_localtop) # "lazy localtop".
'$use_opt'(lazy_localtop).

% Allow frames with uninitialized variables. A bit mask is used mark
% which frame entries are alive.

% TODO: incomplete! pending parts:
%   - only supported in the bytecode compiler
%   - the bitmask is not included in the environment frame (idea: 16
%     bit arity + 16 bit mask?)
%   - initial results (without GC) did not show any speedup in any
%     small benchmark!
:- decl_opt(uninit_y) # "allow frames with uninitialized variables".
% '$use_opt'(uninit_y).

% Add a 'previous' entry in try_node_t to make incore_insert O(1)
% TODO: make default one and remove 'else' code
:- decl_opt(incoreopt) # "optimized incore insert".
'$use_opt'(incoreopt).

% Experimental: tag emul_info and instance to remove only marked clauses
% TODO: make default one and remove 'else' code
:- decl_opt(regmod) # "tagged emul_info".
'$use_opt'(regmod).

% Enable OO extensions
:- decl_opt(oo_extensions) # "oo extensions".
'$use_opt'(oo_extensions).

:- decl_opt(atom_locks) # "use atoms as locks".
'$use_opt'(atom_locks).

% If we undefine this, only binary semaphores will be available
:- decl_opt(general_locks) # "general locks".
'$use_opt'(general_locks).

% TODO: move all profile options here
:- decl_opt(profile_calls) # "profile predicate calls".
% '$use_opt'(profile_calls).

% TODO: fix statistics! reported size of free control and trail stack
% is inaccurate! (that is why the high water mark may be higher than
% the total reported size)
:- decl_opt(mem_profile) # "profile memory usage (high water mark)".
% '$use_opt'(mem_profile).

% Use a hash table as a cache to memoize results of module resolve
% for QM=(-), empty the cache whenever the imports or defines of 
% a predicate changes
:- decl_opt(cache_local_resolve) # "cache local resolve results".
'$use_opt'(cache_local_resolve).

:- decl_opt(dynlink_mod) # "add support for dynlinks in module structure".
'$use_opt'(dynlink_mod).

:- decl_opt(computed_goto) # "Use computed goto (GCC extension) in the
   emulator loop jumps instead of switchs".

% TODO: not yet implemented
:- decl_opt(threaded_bytecode) # "Use threaded bytecode".

:- decl_opt(op32) # "Do not use 16 bit operands".

% TODO: experimental
:- decl_opt(threaded_bytecode_rel16) # "Use threaded bytecode (as 16
   bit relative offsets)".

%:- if('$with_compiler_version'(47)).
:- if(fail).
% Experimental settings
% TODO: make op32 work... implement simple threaded_bytecode (no
%       relative)... optimize code with specmode off... but use S instead of
%       rmode!!!

% note: (\+specmode, op32) does not seem slower than specmode alone
% note: do not disable padding (detect that it is not required)
'$use_opt'(op32). '$use_opt'(no_padding).
% TODO: rel16 seems to speedup around 30-50%!! but requires disabled
%       specmode, that slowdowns the same number...
% '$use_opt'(threaded_bytecode_rel16).
'$use_opt'(threaded_bytecode).
% '$use_opt'(p_in_reg). % TODO: makes things slower!
:- else.
% Default settings
'$use_opt'(computed_goto).
:- endif.

:- decl_opt(cached_h) # "Use cached H register in a local variable".
% '$use_opt'(cached_h).

% some option dependencies
:- if('$use_opt'(varops)).
'$use_opt'(varops2). % TODO: this option does not seem to optimize too much...
:- endif.
:- if('$use_opt'(specmode)).
'$use_opt'(cached_h).
:- endif.

% ---------------------------------------------------------------------------
% Options for data representation and tag scheme

:- decl_opt(atom_len) # "include atom name length in atom definition".
'$use_opt'(atom_len).

% (experimental) Use a functor table to represent functors: 
%   - much more atoms
%   - functors much larger (arity can be greater)
%   - not compatible with current thread model, locking the predicate
%     table is slow
%   - requires qtag (pointers cannot be stored as ATM since one bit is
%     reserved for blobs, in qtag mode that bit is always reserved,
%     for all tags)
%   - current_atom is disabled with this flag
%   - but 12% slower in some cases :(
:- decl_opt(functor_table) # "functor table to represent functors".
% '$use_opt'(functor_table).

% TODO: not really faster?
:- decl_opt(tagp) # "removal of bits not in pointermask from pointers by substraction".

:- decl_opt(tagswbit) # "Use tag bit tests to implement tag switch code (instead of a C switch)".
% TODO: the predefined optimal switch is only valid for the original tag scheme in Ciao
% TODO: makes benchmark Wave building (arrays) slower! (2s vs 1.5s) why?
:- decl_opt(tagtestbit) # "Use tag bit tests to implement tag tests".
% (does not work without tagswbit)
% TODO: see engine__definitions.h, this is not complete
:- decl_opt(tagtestmask) # "Use bit masks implement tag tests".
% (does not work without tagswbit)
% TODO: see engine__definitions.h, this is not complete
:- decl_opt(qtag) # "(required by the old default tag scheme) Reserve the
   subtag QMask in all taggeds (not only in ATM).
   Blob functors for floats use the NUM tag (instead of the ATM tag).
   Removes an extra bit to the smallint and pointer representation.".
% TODO: increase the smallint range used in the compiler (not only the engine) accordingly to this option
% TODO: fix: (see engine__gc.c) garbage_words may be greater than the maximum length of a bignum, split the box
% TODO: remove qtag option?

% Location of tags in the word
:- decl_opt(highbittags) # "Encode the tags in the higher bits of the word.".
:- decl_opt(lowbittags) # "Encode the tags in the lower bits of the word.".
:- decl_opt(halfwordtags) # "Encode the tags in the higher half word (for tagged64).".

% Location of GC bits
:- decl_opt(highbitgc) # "Encode the GC bits in the higher bits of the untagged part of the word.".
:- decl_opt(lowbitgc) # "Encode the GC bits in the lower bits of the untagged part of the word.".
:- decl_opt(extgc) # "Encode the GC bits in an external section.".

:- decl_opt(nestedmarksgc) # "Store nested marks (for c_term) into GC bits.".

:- decl_opt(optlst) # "Reserve a tag for '.'/2 structures".
% TODO: right now, it cannot be disabled
'$use_opt'(optlst).

:- decl_opt(tagged64) # "64 bit taggeds".
:- decl_opt(pointer64) # "Use 64 bit pointers (requires a 64 bit architecture)".

:- decl_opt(no_padding) # "Do not use padded instructions.".
% '$use_opt' := no_padding.

:- decl_opt(p_in_reg) # "Put P register in a asm register.".
% '$use_opt'(p_in_reg).

% TODO: is a change in compiler version necessary to change the tag scheme?
% TODO: There is a problem in tag scheme changes and the current $absmach definition
%       (e.g. for dynamically compiled and loaded bytecode! -- engine and 
%        library code must be synchronized!)
% note: the tag scheme is externally defined in reader absmach.pl... default: '$use_opt'(tagscheme1).

:- pred general_opts/1 + lowentrymacrocons(cstring, 'GENERAL_OPTS').
% String identifiers for enabled options
general_opts := ~'$enabled_opts_cstring'([
        varops,
	iblt,
	omerg,
	joinconts, % TODO: move to tag scheme opts?
	urules,
	specmode]).
:- pred tagscheme_opts/1 + lowentrymacrocons(cstring, 'TAGSCHEME_OPTS').
tagscheme_opts := ~'$enabled_opts_cstring'([
	% tag bits (one of)
	halfwordtags,
	highbittags,
	lowbittags,
	splitbittags,
	% gc bits (one of)
	highbitgc,
	lowbitgc,
	extgc,
	% q tag bit (optional)
	qtag,
	% padding (optional)
	no_padding, % TODO: use!!
	% pointer (optional)
	pointer64,
	tagged64,
	% tag switch (optional) (tagswbit requires one of tagtests)
	tagswbit,
	% tag test (optional)
	tagtestmask, % TODO: use!!
	tagtestbit]).

% TODO: also test padding!
% TODO: name prefix_padding the default padding option, add full_padding?? <- not easy?
%'$use_opt' := * | no_padding.

% TODO: wave_arr test is still very slow with tag schemes different than tagscheme1... why??
% TODO: improve GC efficiency for extgc!

% -- 32 bit tagged, 32 bit pointers --
% tagscheme1 is the DEFAULT TAG SCHEME. Do not modify.
:- if('$use_opt'(tagscheme1)).   '$use_opt' := highbittags | lowbitgc | qtag | tagswbit | tagtestbit.
:- elif('$use_opt'(tagscheme2)). '$use_opt' := highbittags | lowbitgc | qtag | tagtestbit.
:- elif('$use_opt'(tagscheme3)). '$use_opt' := highbittags | lowbitgc | qtag.
:- elif('$use_opt'(tagscheme4)). '$use_opt' := highbittags | lowbitgc | tagswbit | tagtestbit.
:- elif('$use_opt'(tagscheme5)). '$use_opt' := highbittags | lowbitgc | tagtestbit.
:- elif('$use_opt'(tagscheme6)). '$use_opt' := highbittags | lowbitgc.
:- elif('$use_opt'(tagscheme7)). '$use_opt' := highbittags | extgc | tagswbit | tagtestbit.
:- elif('$use_opt'(tagscheme8)). '$use_opt' := highbittags | extgc | tagtestbit.
:- elif('$use_opt'(tagscheme9)). '$use_opt' := highbittags | extgc.
:- elif('$use_opt'(tagscheme10)). '$use_opt' := highbittags | highbitgc | tagswbit | tagtestbit.
:- elif('$use_opt'(tagscheme11)). '$use_opt' := highbittags | highbitgc | tagtestbit.
:- elif('$use_opt'(tagscheme12)). '$use_opt' := highbittags | highbitgc.
:- elif('$use_opt'(tagscheme13)). '$use_opt' := lowbittags | highbitgc | tagswbit | tagtestbit.
:- elif('$use_opt'(tagscheme14)). '$use_opt' := lowbittags | highbitgc | tagtestbit.
:- elif('$use_opt'(tagscheme15)). '$use_opt' := lowbittags | highbitgc.
:- elif('$use_opt'(tagscheme16)). '$use_opt' := lowbittags | extgc | tagswbit | tagtestbit.
:- elif('$use_opt'(tagscheme17)). '$use_opt' := lowbittags | extgc | tagtestbit.
:- elif('$use_opt'(tagscheme18)). '$use_opt' := lowbittags | extgc.
:- elif('$use_opt'(tagscheme19)). '$use_opt' := lowbittags | lowbitgc | tagswbit | tagtestbit.
:- elif('$use_opt'(tagscheme20)). '$use_opt' := lowbittags | lowbitgc | tagtestbit.
:- elif('$use_opt'(tagscheme21)). '$use_opt' := lowbittags | lowbitgc.
% -- 64 bit (double word) tagged, 32 bit pointers --
% check!!
:- elif('$use_opt'(tagscheme22)). '$use_opt' := tagged64 | halfwordtags | lowbitgc | tagswbit | tagtestbit.
% check!!
:- elif('$use_opt'(tagscheme23)). '$use_opt' := tagged64 | halfwordtags | lowbitgc | tagtestbit.
:- elif('$use_opt'(tagscheme24)). '$use_opt' := tagged64 | halfwordtags | lowbitgc.
% check!!
:- elif('$use_opt'(tagscheme25)). '$use_opt' := tagged64 | halfwordtags | extgc | tagswbit | tagtestbit.
% check!! (probably it has something to do with the TagB3 macros?)
:- elif('$use_opt'(tagscheme26)). '$use_opt' := tagged64 | halfwordtags | extgc | tagtestbit.
:- elif('$use_opt'(tagscheme27)). '$use_opt' := tagged64 | halfwordtags | extgc.
% -- 64 bit tagged, 64 bit pointers --
:- elif('$use_opt'(tagscheme28)). '$use_opt' := pointer64 | tagged64 | highbittags | lowbitgc | tagswbit | tagtestbit.
:- elif('$use_opt'(tagscheme29)). '$use_opt' := pointer64 | tagged64 | highbittags | lowbitgc | tagtestbit.
:- elif('$use_opt'(tagscheme30)). '$use_opt' := pointer64 | tagged64 | highbittags | lowbitgc.
:- elif('$use_opt'(tagscheme31)). '$use_opt' := pointer64 | tagged64 | highbittags | extgc | tagswbit | tagtestbit.
:- elif('$use_opt'(tagscheme32)). '$use_opt' := pointer64 | tagged64 | highbittags | extgc | tagtestbit.
:- elif('$use_opt'(tagscheme33)). '$use_opt' := pointer64 | tagged64 | highbittags | extgc.
:- elif('$use_opt'(tagscheme34)). '$use_opt' := pointer64 | tagged64 | highbittags | highbitgc | tagswbit | tagtestbit.
:- elif('$use_opt'(tagscheme35)). '$use_opt' := pointer64 | tagged64 | highbittags | highbitgc | tagtestbit.
:- elif('$use_opt'(tagscheme36)). '$use_opt' := pointer64 | tagged64 | highbittags | highbitgc.
:- elif('$use_opt'(tagscheme37)). '$use_opt' := pointer64 | tagged64 | lowbittags | highbitgc | tagswbit | tagtestbit.
:- elif('$use_opt'(tagscheme38)). '$use_opt' := pointer64 | tagged64 | lowbittags | highbitgc | tagtestbit.
:- elif('$use_opt'(tagscheme39)). '$use_opt' := pointer64 | tagged64 | lowbittags | highbitgc.
:- elif('$use_opt'(tagscheme40)). '$use_opt' := pointer64 | tagged64 | lowbittags | extgc | tagswbit | tagtestbit.
:- elif('$use_opt'(tagscheme41)). '$use_opt' := pointer64 | tagged64 | lowbittags | extgc | tagtestbit.
:- elif('$use_opt'(tagscheme42)). '$use_opt' := pointer64 | tagged64 | lowbittags | extgc.
:- elif('$use_opt'(tagscheme43)). '$use_opt' := pointer64 | tagged64 | lowbittags | lowbitgc | tagswbit | tagtestbit.
:- elif('$use_opt'(tagscheme44)). '$use_opt' := pointer64 | tagged64 | lowbittags | lowbitgc | tagtestbit.
:- elif('$use_opt'(tagscheme45)). '$use_opt' := pointer64 | tagged64 | lowbittags | lowbitgc.
% split bit tags
:- elif('$use_opt'(tagscheme46)). '$use_opt' := splitbittags | extgc | tagswbit | tagtestbit.
:- elif('$use_opt'(tagscheme47)). '$use_opt' := splitbittags | extgc | tagtestbit.
:- elif('$use_opt'(tagscheme48)). '$use_opt' := splitbittags | extgc.
:- endif.

% ---------------------------------------------------------------------------
% Basic type definitions

% default machine integer
% TODO: replace by other types? (like atomlen_t, aritycount_t, etc.) */

:- if('$use_opt'(pointer64)).
% pointer size is 64 bits
:- pointer_size(8).
% TODO: add a mode where some integers are still 32 bit longs even in 64 bit architectures (i.e. only pointers and integers for data sizes are 64 bit long)
:- lowtype(intmach).
:- type(intmach/1) + equiv.
intmach(T) :- T = ~int64.
:- lowtype(uintmach).
:- type(uintmach/1) + equiv.
uintmach(T) :- T = ~uint64.
:- else.
% pointer size is 32 bits
:- pointer_size(4).
:- lowtype(intmach).
:- type(intmach/1) + equiv.
intmach(T) :- T = ~int32.
:- lowtype(uintmach).
:- type(uintmach/1) + equiv.
uintmach(T) :- T = ~uint32.
:- endif.

:- if('$use_opt'(tagged64)).
% align blobs so that all references to the heap are aligned
% note: this can be avoided if extgc and pointer64 are not enabled
:- pred align_blob_64/1 + lowentrymacrocons(intmach, 'ALIGN_BLOB_64').
align_blob_64 := 1.
:- endif.

:- type(bittype/1) + equiv.
bittype(T) :- T = ~bitfield(1).

%    intval: integers that can hold the value of a tagged word
:- lowtype(intval).
:- type(intval/1) + equiv.
:- if('$use_opt'(pointer64)).
intval(T) :- T = ~int64.
:- else.
intval(T) :- T = ~int32.
:- endif.

%    uintval: integers that can hold the tag of a tagged word
:- lowtype(uintval).
:- type(uintval/1) + equiv.
:- if('$use_opt'(pointer64)).
uintval(T) :- T = ~uint64.
:- else.
uintval(T) :- T = ~uint32.
:- endif.

% TODO: add as tables and order automatically using the 'most specific ordering'	
% TODO: define as assertions... and methods of the types
% TODO: do not use 'mutable arithmetic', but special mutable methods!
% TODO: think about the text below:
%   +(A,B,C) :- intval(A), intval(B), !, C = ~intval_sum(A,B). ?? <== good but 
%   (maybe adding guards with 'most specific ordering'...)
%  overapproximation(+(A,B,C) :- intval(A), intval(B), !, intval(C))
%  (but that is a pred assertion)
:- pred (+)/2 + foreignfun_spec([intval], intval, [X], X).
:- pred (+)/2 + foreignfun_spec([intmach], intmach, [X], X).
:- pred (+)/2 + foreignfun_spec([flt64], flt64, [X], X).
:- pred (-)/2 + foreignfun_spec([intval], intval, [X], -X).
:- pred (-)/2 + foreignfun_spec([intmach], intmach, [X], -X).
:- pred (-)/2 + foreignfun_spec([flt64], flt64, [X], -X).
:- pred (+)/3 + foreignfun_spec([intval, intval], intval, [X,Y], X+Y).
:- pred (+)/3 + foreignfun_spec([intmach, intmach], intmach, [X,Y], X+Y).
:- pred (+)/3 + foreignfun_spec([intval, intmach], intval, [X,Y], X+Y). % TODO: check
:- pred (+)/3 + foreignfun_spec([intmach, intval], intval, [X,Y], X+Y). % TODO: check
:- pred (+)/3 + foreignfun_spec([flt64, flt64], flt64, [X,Y], X+Y).
:- pred (+)/3 + foreignfun_spec([instance_clock, intmach], instance_clock, [X,Y], X+Y).
:- pred (+)/3 + foreignfun_spec([intmach, instance_clock], instance_clock, [X,Y], X+Y).
:- pred (-)/3 + foreignfun_spec([intval, intval], intval, [X,Y], X-Y).
:- pred (-)/3 + foreignfun_spec([intmach, intmach], intmach, [X,Y], X-Y).
:- pred (-)/3 + foreignfun_spec([intval, intmach], intval, [X,Y], X-Y). % TODO: check
:- pred (-)/3 + foreignfun_spec([intmach, intval], intval, [X,Y], X-Y). % TODO: check
:- pred (-)/3 + foreignfun_spec([flt64, flt64], flt64, [X,Y], X-Y).
:- pred (-)/3 + foreignfun_spec([instance_clock, intmach], instance_clock, [X,Y], X-Y).
:- pred (-)/3 + foreignfun_spec([intmach, instance_clock], instance_clock, [X,Y], X-Y).
:- pred (/\)/3 + foreignfun_spec([intval, intval], intval, [X,Y], X/\Y).
:- pred (/\)/3 + foreignfun_spec([intmach, intmach], intmach, [X,Y], X/\Y).
:- pred (/\)/3 + foreignfun_spec([intval, intmach], intval, [X,Y], X/\Y). % TODO: check
:- pred (/\)/3 + foreignfun_spec([intmach, intval], intval, [X,Y], X/\Y). % TODO: check
:- pred (\/)/3 + foreignfun_spec([intval, intval], intval, [X,Y], X\/Y).
:- pred (\/)/3 + foreignfun_spec([intmach, intmach], intmach, [X,Y], X\/Y).
:- pred (\/)/3 + foreignfun_spec([intval, intmach], intval, [X,Y], X\/Y). % TODO: check
:- pred (\/)/3 + foreignfun_spec([intmach, intval], intval, [X,Y], X\/Y). % TODO: check
:- pred (*)/3 + foreignfun_spec([intmach, intmach], intmach, [X,Y], X*Y).
:- pred (*)/3 + foreignfun_spec([intval, intval], intval, [X,Y], X*Y).
:- pred (*)/3 + foreignfun_spec([intval, intmach], intval, [X,Y], X*Y). % TODO: check
:- pred (*)/3 + foreignfun_spec([intmach, intval], intval, [X,Y], X*Y). % TODO: check
:- pred (*)/3 + foreignfun_spec([intval, flt64], flt64, [X,Y], X*Y).
:- pred (*)/3 + foreignfun_spec([flt64, intval], flt64, [X,Y], X*Y).
:- pred (*)/3 + foreignfun_spec([flt64, flt64], flt64, [X,Y], X*Y).
:- pred (mod)/3 + foreignfun_spec([intmach, intmach], intmach, [X,Y], X mod Y).
:- pred (mod)/3 + foreignfun_spec([intval, intmach], intmach, [X,Y], X mod Y).
:- pred (mod)/3 + foreignfun_spec([intval, intval], intval, [X,Y], X mod Y).
% note: 'quoted' is necessary to avoid C warnings about operator priority (that are false positives)
:- pred (<<)/3 + foreignfun_spec([intmach, intmach], intmach, [X,Y], quoted(X)<<quoted(Y)).
:- pred (<<)/3 + foreignfun_spec([intval, intmach], intval, [X,Y], quoted(X)<<quoted(Y)).
:- pred (<<)/3 + foreignfun_spec([intval, intval], intval, [X,Y], quoted(X)<<quoted(Y)).
% note: 'quoted' is necessary to avoid C warnings about operator priority (that are false positives)
:- pred (>>)/3 + foreignfun_spec([intmach, intmach], intmach, [X,Y], quoted(X)>>quoted(Y)).
:- pred (>>)/3 + foreignfun_spec([intval, intmach], intval, [X,Y], quoted(X)>>quoted(Y)).
:- pred (>>)/3 + foreignfun_spec([intval, intval], intval, [X,Y], quoted(X)>>quoted(Y)).
:- pred (/)/3 + foreignfun_spec([intmach, intmach], intmach, [X,Y], X/Y).
:- pred (/)/3 + foreignfun_spec([intval, intval], intval, [X,Y], X/Y).
:- pred (/)/3 + foreignfun_spec([flt64, flt64], flt64, [X,Y], X/Y).

% ---------------------------------------------------------------------------
% execute some action on each pair of elements in a heap tuple
% TODO: is this a graph-pair treatment algorithm?
% TODO: add Action to make it generic
% note: TreatTuple allow recursive processing of the last pair of each tuple in the same loop

% TODO: include code to swap nodes using the value trail inside the generic code?
% TODO: is this an algorithm to treat directed graphs?

% TODO: generalize to implement copy_term/2, ground/1, subsumes/2, etc.?

% TODO: generalize 'compile time checks' as 'can_prove_succeed' (meta-analysis-predicates?)

% TODO: USE MOST-SPECIFIC-ORDERING FOR GUARDS!

:- predspec_atom1(treat_tuple_pair__begin(unify,A,B), lowentry(det_with_failins, [tagged,tagged], 'cunify') + prop(with_worker)).
treat_tuple_pair__begin__(Action,A,B) :-
	% TODO: declaring new mut vars for X1 and X2 generates different asm code, fix; input of lowpred cannot be mut
%	'$trust_type'(A, mut(tagged)),
%	'$trust_type'(B, mut(tagged)),
	A2 = ~'$forcedmut'(A),
	B2 = ~'$forcedmut'(B),
	TreatTuple = ~'$predabs'([propargs], [Arity, PT1, PT2], [Action], treat_tuple_pair_first(Action, Arity, PT1, PT2)),
	treat_tuple_pair__action(Action, A2, B2, TreatTuple).
:- predspec_atom1(treat_tuple_pair__action_loop(unify,A,B), lowentry(det_with_failins, [tagged,tagged], 'cunify_aux') + prop(with_worker)).
treat_tuple_pair__action_loop__(Action, X1, X2) :-
	% TODO: declaring new mut vars for X1 and X2 generates different asm code, fix; input of lowpred cannot be mut
	X1m = ~'$forcedmut'(X1),
	X2m = ~'$forcedmut'(X2),
	U = ~clonemut(X1m),
	V = ~clonemut(X2m),
	treat_tuple_pair__action_with_cont(Action, U, V, X1m, X2m).
:- pred treat_tuple_pair_first/4 + prop(unfold) + prop(propargs).
treat_tuple_pair_first(Action, Arity, PT1, PT2) :-
	treat_tuple_pair__level_first(Action, Arity, PT1, PT2).
:- pred treat_tuple_pair_other/8 + prop(unfold) + prop(propargs).
treat_tuple_pair_other(Action, U, V, U2, V2, Arity, PT1, PT2) :-
	treat_tuple_pair__level_other(Action, Arity, PT1, PT2, U2, V2),
	U <- @U2,
	V <- @V2, 
	treat_tuple_pair__action_with_cont(Action, U, V, U2, V2).
:- predspec_atom1(treat_tuple_pair__action_with_cont(unify, U, V, X1, X2), prop(subpr)).
treat_tuple_pair__action_with_cont__(Action, U, V, X1, X2) :-
	TreatTuple = ~'$predabs'([propargs], [Arity, PT1, PT2], [Action, U, V, X1, X2], treat_tuple_pair_other(Action, U, V, X1, X2, Arity, PT1, PT2)),
	treat_tuple_pair__action(Action, U, V, TreatTuple).

:- pred treat_tuple_pair__action/4 + prop(unfold). % (abstract)

:- predspec_atom1(treat_tuple_pair__level_first(unify, Arity, Pt1, Pt2), lowentry(det_with_failins, [intmach, mut(tagged), mut(tagged)], 'cunify_args') + prop(with_worker)).
treat_tuple_pair__level_first__(Action, Arity, Pt1, Pt2) :-
	X1 = ~newmut(tagged),
	X2 = ~newmut(tagged),
	Result = ~newmut(bool),
	'$catch_failins'((treat_tuple_pair__level_other(Action, Arity,Pt1,Pt2,X1,X2),
	                  treat_tuple_pair__action_loop(Action, @X1, @X2), Result <- true),
			 (Result <- false)),
	treat_tuple_pair__vt_undo(Action),
	( @Result = true -> true ; '$cont'(failinscont) ).
:- pred treat_tuple_pair__vt_undo/1 + prop(unfold). % (abstract)

:- predspec_atom1(treat_tuple_pair__level_other(unify, Arity, Pt1, Pt2, X1, X2), lowentry(det_with_failins, [intmach, mut(tagged), mut(tagged), mut(tagged), mut(tagged)], 'cunify_args_aux') + prop(with_worker)).
treat_tuple_pair__level_other__(Action, Arityb, Pt1b, Pt2b, X1, X2) :-
	Arity = ~'$forcedmut'(Arityb), % TODO: a kludge... I need threaded vars in recursions!
	Pt1 = ~'$forcedmut'(Pt1b), % TODO: a kludge... I need threaded vars in recursions!
	Pt2 = ~'$forcedmut'(Pt2b), % TODO: a kludge... I need threaded vars in recursions!
	T1 = ~initmut(tagged, ~'$ccons'(0, tagged)), % TODO: ~bitwise_not(0) or 2<<32-1...
	T2 = ~initmut(tagged, ~'$ccons'(0, tagged)), % TODO: ~bitwise_not(0) or 2<<32-1...
	% Terminating unification of complex structures: Forward args of pt2 to
        % args of pt1 using choice stack as value cell.  When done, reinstall
        % values.
	treat_tuple_pair__vt_test_overflow(Action),
	treat_tuple_pair__tuple(Action, Arity, Pt1, Pt2, T1, T2),
	X1 <- @T1,
	X2 <- @T2.
	% TODO: remove.. it seems to be unnecessary
        % valuetrail__test_overflow(~'$ccons'(choice_pad, intmach)), % TODO: it was equiv to CHOICEPAD/2... why????
:- pred treat_tuple_pair__vt_test_overflow/1 + prop(unfold). % (abstract)

:- predspec_atom1(treat_tuple_pair__tuple(unify, Arity, Pt1, Pt2, T1, T2), prop(subpr)).
treat_tuple_pair__tuple__(Action, Arity, Pt1, Pt2, T1, T2) :-
	( @Arity =< 0 ->
	    true
	; T1 <- @(@Pt1),
	  T2 <- @(@Pt2),
	  '$trust_type'(@T1, nonstacktagged),
	  '$trust_type'(@T2, nonstacktagged),
	  PrepareDoAction = ~'$predabs'([], [], [Action, Arity, T1, T2], (treat_tuple_pair__prepare_do_action(Action, Arity, T1, T2))),
	  treat_tuple_pair__prepare(Action, PrepareDoAction, Pt1, Pt2, T1, T2),
	  Pt1 <- ~'$mut_move'(@Pt1, 1),
	  Pt2 <- ~'$mut_move'(@Pt2, 1),
	  Arity <- @Arity - 1,
	  treat_tuple_pair__tuple(Action, Arity, Pt1, Pt2, T1, T2)
	).
:- pred treat_tuple_pair__prepare/6 + prop(unfold). % (abstract)

:- predspec_atom1(treat_tuple_pair__prepare_do_action(unify, Arity, T1, T2), prop(subpr)).
treat_tuple_pair__prepare_do_action__(Action, Arity, T1, T2) :-
	( @Arity > 1 ->
	    treat_tuple_pair__action_loop(Action, @T1, @T2)
	; true
	).

% ---------------------------------------------------------------------------
% Tagged word definition

% The tagged type is defined in several layers, using rules, in a
% fashion similar to object inheritance. The different rules are
% grouped together using indexing on tagged tag (that is similar to
% method dispatching in OOP).

% TODO: ideally, it should be possible to remove definitions for,
% e.g. LST, and rebuild everything without need to modify in hundred
% of code locations (or instructions).

% TODO: introduce guards different than !: it would be useful to have
% special guards that order clauses from the most specific to the less
% specific condition (and gives an error in case of ambiguity)
% TODO: decompose functor__* predicates?

% TODO: introduce annotations to test for a particular tag at some
% program points (may them be called 'selection points'?)
% (a list of type checks along with the specialized code for each branch)
% e.g.
%   :- u_constraint__2 + selpoint(hva;cva;sva;(num;atm;lst;str))
%   u_constraint__2(T1, T2, A) :- reftagged(@T1), !, u_constraint__3(T1, T2, A).
%   u_constraint__2(T1, T2, _) :- nonreftagged(@T1), !, bind_ref(@T2,@T1).

% TODO: indexing on tagged tag is similar to a compiled object dispatching!

% TODO: do not cast between intmach and tagged (size may differ)

% TODO: use an optional external tag stack (it does not seem easy)
% TODO: add options to disable tags (e.g. SVA and LST)

% TODO: define intval uintval uinttag tagged stagged in source code all built-ins (e.g. tagged, etc.)

:- lowtype(tagged).
:- abstract_class tagged {
    % A tagged word
    :- bitstr.
    % base storage for tagged
    :- if('$use_opt'(tagged64)).
        :- base(uint64).
    :- else.
        :- base(uint32).
    :- endif.
    % tag member
    :- attr tag :: /*compat*/ ref0('tagged_$key'). % TODO: ugly...
    :- if('$use_opt'(highbittags)). % tag scheme: TTTx...x
        :- attr tag :: '$prop'(bitpack(upper(3))).
    :- elif('$use_opt'(halfwordtags)). % tag scheme: 0....0TTT|x...x (for tagged64)
        :- attr tag :: '$prop'(bitpack(upper(32))).
    :- elif('$use_opt'(lowbittags)). % tag scheme: x...xTTT
        :- attr tag :: '$prop'(bitpack(lower(3))).
    :- elif('$use_opt'(splitbittags)). % tag scheme: Tx...xTT (easier pointer extraction?)
        :- attr tag :: '$prop'(bitpack(split(1,2))).
    :- else.
        % TODO: error? no tags? 
    :- endif.
    % gc marks member
    :- if(\+ '$use_opt'(extgc)).
        :- attr gc :: /*compat*/ ref0(gctype).
        :- mixin_extends(gctype_def).
        :- if('$use_opt'(lowbitgc)). % GC bits in the lower part of the tagged
            :- attr gc :: '$prop'(bitpack(lower)).
        :- elif('$use_opt'(highbitgc)). % GC bits in the upper part of the tagged
            :- attr gc :: '$prop'(bitpack(upper)).
        :- else.
            % TODO: error? no gc?
        :- endif.
        % TODO: this is a kludge... find a better way
        :- sub(gc) {
            :- mixin_extends(gctype_def__storage).
        }.
    :- endif.
    % TODO: do not forget pointer reversal in GC, can it be described in the type? it should be! (e.g. sometimes a pointer is stored in a value)
    :- if('$use_opt'(qtag)).
        :- attr qtag :: /*compat*/ ref0(bittype).
        :- attr qtag :: '$prop'(bitpack(upper)). % reserve qtag for all
    :- endif.
}.

% uinttag: integer that can hold the value of a tag
:- lowtype(uinttag).
:- type(uinttag/1) + equiv. % TODO: define automatically? or try to remove? (e.g. most efficient type that can store the tag, unsigned)
uinttag(T) :- T = ~uint32. % TODO: define automatically? or try to remove? (e.g. most efficient type that can store the tag, unsigned)

% functors
% TODO:[ts] can a functor and a tagged be different?
:- lowtype(functor).
:- type(functor/1) + equiv.
functor(T) :- T = ~tagged.

% TODO: autogenerate joincont code, for derefvar (e.g. defien something like '$unfoldbranchcases'(derefvar(U), [isvar, nva])...  ?)

% TODO: DO NOT CHANGE THE HIERARCHY ORDER (clause order matters because of indexing on tag... FIX IT) 

:- pred choice_pad/1 + foreigncons(intmach, 'CHOICEPAD').
:- pred call_pad/1 + foreigncons(intmach, 'CALLPAD').

% TODO: define tag as the type of tagged.tag? however, the tag/1 type may not be necessary... (at least explicitly)
:- type(tag/1) + enum.
tag(T) :- T = ~'$typekey'(tagged). % TODO: improve
% Unify two terms
% TODO:[throwfail] replace failure that is propagated to the absmach by exceptions (throw(fail)): we are not untrailing anything and our execution is deterministic (except for read-only tests)
% TODO: see TODO:[throwfail]
:- pred unify/2 + prop(unfold).
unify(U, V) :-
	T0 = ~clonemut(U),
	T1 = ~clonemut(V),
	( \+ @T0 == @T1 -> % fast check before call
	    treat_tuple_pair__begin(unify, @T0, @T1)
	; true
	).
treat_tuple_pair__prepare(Action, PrepareDoAction, Pt1, Pt2, T1, T2) :- Action = unify, !,
	( \+ @T1 == @T2 ->
	    derefvar(T1),
	    ( reftagged(@T1) -> % TODO: use a special index prop to know if it worths doing unfoldcond or not
	        '$unfold'(PrepareDoAction)
	    ; derefvar(T2),
	      ( reftagged(@T2) ->
		  '$unfold'(PrepareDoAction)
	      ; ( \+ @T1 == @T2 ->
		    ( structtagged(@T1), structtagged(@T2) -> true ; '$cont'(failinscont) ),
		    % replace smaller value by larger value, using choice stack as value trail
		    % TODO: check documentation: swap nodes so that the process works with cyclic terms
		    ( @T1 > @T2 ->
		        valuetrail__set(@Pt1, @T2)
		    ; valuetrail__set(@Pt2, @T1)
		    ),
		    '$unfold'(PrepareDoAction)
		; true
		)
	      )
	    )
	; true
	).
treat_tuple_pair__action(Action, U, V, TreatTuple) :- Action = unify, !,
	unify_aux(U, V, TreatTuple).
treat_tuple_pair__vt_undo(Action) :- Action = unify, !,
	valuetrail__undo.
treat_tuple_pair__vt_test_overflow(Action) :- Action = unify, !,
        valuetrail__test_overflow(~choice_pad).
% Value trail
:- pred valuetrail__undo/0 + foreignmacro([], [], 'VALUETRAIL__UNDO').
:- pred valuetrail__test_overflow/1 + foreignmacro([intmach], [c], 'VALUETRAIL__TEST_OVERFLOW').
:- pred valuetrail__set/2 + foreignmacro([mut(tagged), tagged], [c,c], 'VALUETRAIL__SET').

:- pred unify_aux/3 + prop(unfold).
unify_aux(U, V, TreatTuple) :- derefvar(U), unify_auxd(U, V, TreatTuple).
:- pred unify_auxd/3 + prop(unfold). % (abstract)
:- pred unify_with_vard/3 + prop(unfold).
unify_with_vard(V, U, TreatTuple) :- derefvar(V), unify_withd_vard(V, U, TreatTuple).
:- pred unify_withd_vard/3 + prop(unfold). % (abstract)
:- pred unify_with_nvad/3 + prop(unfold).
unify_with_nvad(V, U, TreatTuple) :- derefvar(V), unify_withd_nvad(V, U, TreatTuple).
:- pred unify_withd_nvad/3 + prop(unfold). % (abstract)

% tagged building and unification definitions (for the engine)
:- pred u_constraint__3/3 + prop(unfold). % (abstract)
:- pred unify_local_value__2/2 + prop(unfold) + prop(propargs). % (abstract)
:- pred u_cons0/2 + prop(unfold).
u_cons0(T1, Cons) :- derefvar(T1), u_cons0d(T1, Cons).
:- pred u_cons0d/2 + prop(unfold). % (abstract)
:- pred u_blob0/2 + prop(unfold).
u_blob0(T1, L) :- derefvar(T1), u_blob0d(T1, L).
:- pred u_blob0d/2 + prop(unfold). % (abstract)
:- pred u_str0/2 + prop(unfold).
u_str0(T1, F) :- derefvar(T1), u_str0d(T1, F).
:- pred u_str0d/2 + prop(unfold). % (abstract)

% ---------------------------------------------------------------------------
% References (to other tagged and references pointing to themselves (unbound variables))
:- abstract_class reftagged < tagged {
    :- bitstr.
    :- attr ptr :: /*compat*/ ref1(mut(tagged)).
    :- attr ptr :: '$prop'(bitpack(rest)).
    :- smallptrmodel(ptr, uintval).
}.
u_cons0d(T1, Cons) :- '$expandcond'(reftagged(@T1)), !, bind_ref(@T1,Cons).
u_blob0d(T1, L) :- '$expandcond'(reftagged(@T1)), !, blob__bind(T1, L).
u_str0d(T1, F) :- '$expandcond'(reftagged(@T1)), !,
	'$set_mode'(w),
	'$set_mode__nospecmode'(w),
	functor__bind(F, T1).
:- pred trail_cond/1 + prop(unfold) + prop(propargs). % (abstract)
unify_auxd(U, V, TreatTuple) :- '$expandcond'(reftagged(@U)), !,
	unify_with_vard(V, U, TreatTuple).
unify_withd_vard(V, U, _TreatTuple) :- '$expandcond'(reftagged(@V)), !,
	unify_ref_ref(@U,@V).
unify_withd_nvad(V, U, TreatTuple) :- '$expandcond'(reftagged(@V)), !,
	bind_ref(@V,@U).
:- pred unify_ref_ref/2 + prop(unfold) + prop(propargs).
% If the two variables have the same tag, the younger variable will
% point to the older one.  If the variables have different tag, they
% points according to the priority: sva < hva < cva
%
% Note: ref_prior_le is always computed at compile time; this code is
% specialized for each possible combination of tags
unify_ref_ref(U, V) :-
	( U.tag = V.tag -> % same tag
	    % TODO: imProlog compiler does not propagate that the tags are the same! (younger/2 needs it)
	    ( U == V -> true
	    ; younger(V,U) -> bind_ref(V,U)
	    ; bind_ref(U,V)
	    )
	; ( ref_prior_le(U, V) ->
	      bind_ref(U, V)
	  ; bind_ref(V, U)
	  )
	).
:- pred ref_prior_le/2 + prop(unfold) + prop(propargs).
ref_prior_le(U, V) :-
	ref_prior(U, Up),
	ref_prior(V, Vp),
	Up =< Vp.
:- pred ref_prior/2 + prop(unfold) + prop(propargs). % (abstract)
% TODO: for younger/2, the tag of V and U must be the same! check!
:- pred younger/2 + prop(unfold) + prop(propargs). % (abstract)
:- pred bind_ref/2 + prop(unfold) + prop(propargs).
bind_ref(A, B) :- bind_nowake(A, B), inc_wake_count(A).
:- pred bind_nowake/2 + prop(unfold) + prop(propargs).
bind_nowake(A, B) :-
	( trail_cond(A) ->
	    trail_push_check0(A)
	; true
	),
	A.ptr <- B.
:- pred inc_wake_count/1 + prop(unfold) + prop(propargs). % (abstract)
% ---------------------------------------------------------------------------
% Non-references
:- abstract_class nonreftagged < tagged {
    :- bitstr.
}.
u_cons0d(T1, Cons) :- ( @T1 == Cons -> true ; '$cont'(failinscont) ).
u_blob0d(T1, L) :- ( blob__check(@T1, L) -> true ; '$cont'(failinscont) ).
u_str0d(T1, F) :-
	( functor__check(F, T1) -> true ; '$cont'(failinscont) ),
	'$set_mode'(r),
	'$set_mode__nospecmode'(r),
	functor__s(F, T1).
unify_auxd(U, V, TreatTuple) :-
	unify_with_nvad(V, U, TreatTuple).
unify_withd_vard(V, U, TreatTuple) :-
	unify_withd_nvad(U, V, TreatTuple). % exchange arguments
unify_withd_nvad(V, U, TreatTuple) :-
	unify_nva_nva(U, V, TreatTuple).
:- pred unify_nva_nva/3 + prop(unfold).
unify_nva_nva(U, V, TreatTuple) :-
	( @U == @V -> % same tag and value
	    true
%	; same_tag(@U,@V), % must have the same tag
%	  % TODO: make impcomp infer that @U and @V have the same tag
%	  unify_nva_nva__same_tag_diff_value(U, V, TreatTuple)
%	).
	; ( same_tag(@U,@V) -> true ; '$cont'(failinscont) ),
	  % must have the same tag
	  % TODO: make impcomp infer that @U and @V have the same tag
	  unify_nva_nva__same_tag_diff_value(U, V, TreatTuple)
	).
:- pred unify_nva_nva__same_tag_diff_value/3 + prop(unfold). % (abstract)

% ---------------------------------------------------------------------------
% Constants
:- abstract_class constagged < nonreftagged {
    :- bitstr.
}.
%unify_nva_nva__same_tag_diff_value(U, _V, _TreatTuple) :- constagged(@U), !, fail. % the tagged cannot be single-cell (constag, NUM or ATM) (that means that the value is different, since we are here because the @U == @V check failed)
unify_nva_nva__same_tag_diff_value(U, _V, _TreatTuple) :- constagged(@U), !, '$cont'(failinscont). % the tagged cannot be single-cell (constag, NUM or ATM) (that means that the value is different, since we are here because the @U == @V check failed)

% ---------------------------------------------------------------------------
% Structures
:- abstract_class structtagged < nonreftagged {
    :- bitstr.
}.

% ---------------------------------------------------------------------------
% Heap variables
:- class hvatagged < reftagged {
    :- bitstr.
    :- key(tag, hva).
}.
trail_cond(A) :- hvatagged(A), !, cond_hva(A).
:- pred cond_hva/1 + foreign([tagged], semidet, 'CondHVA').
ref_prior(U, P) :- hvatagged(U), !, P = ~'$consprop'(1).
younger(V, U) :- hvatagged(V), !, younger_heap_var(V,U).
:- pred younger_heap_var/2 + foreign([tagged, tagged], semidet, 'YoungerHeapVar').
inc_wake_count(A) :- hvatagged(A), !.
% ---------------------------------------------------------------------------
% Constraint variables
:- class cvatagged < reftagged {
    :- bitstr.
    :- key(tag, cva).
}.
trail_cond(A) :- cvatagged(A), !, true. % TODO: why is it always trailed? b/c of wake?
:- pred cond_cva/1 + foreign([tagged], semidet, 'CondCVA').
ref_prior(U, P) :- cvatagged(U), !, P = ~'$consprop'(2).
younger(V, U) :- cvatagged(V), !, younger_heap_var(V,U).
inc_wake_count(A) :- cvatagged(A), !, inc_wake_count__2.
:- pred inc_wake_count__2/0 + foreignmacro([], [], 'IncWakeCount').
% ---------------------------------------------------------------------------
% Stack variables
:- class svatagged < reftagged {
    :- bitstr.
    :- key(tag, sva).
}.
trail_cond(A) :- svatagged(A), !, cond_sva(A).
:- pred cond_sva/1 + foreign([tagged], semidet, 'CondSVA').
ref_prior(U, P) :- svatagged(U), !, P = ~'$consprop'(0).
younger(V, U) :- svatagged(V), !, younger_stack_var(V,U).
:- pred younger_stack_var/2 + foreign([tagged, tagged], semidet, 'YoungerStackVar').
inc_wake_count(A) :- svatagged(A), !.

% ---------------------------------------------------------------------------
% Small integers
:- class numtagged < constagged {
    :- bitstr.
    :- key(tag, num).
    :- attr num :: /*compat*/ ref0(intval).
    :- attr num :: '$prop'(bitpack(rest)).
}.
% ---------------------------------------------------------------------------
% Atoms
:- class atmtagged < constagged {
    :- bitstr.
    :- key(tag, atm).
    :- if(\+ '$use_opt'(qtag)).
        :- attr qtag :: /*compat*/ ref0(bittype).
        :- attr qtag :: '$prop'(bitpack(upper)). % reserve qtag for only for atmtagged
    :- endif.
    :- attr qval :: /*compat*/ ref0(intval).
    :- attr qval :: '$prop'(bitpack(rest)).

    % special 'nested' atoms (for cterm)
    :- attr nestedmark :: /*compat*/ ref0(gctype).
    :- attr nestedval :: /*compat*/ ref0(intval).
    % storage for nestedmark and nestedval
    :- if('$use_opt'(nestedmarksgc)).
        :- sub(gc) {
            :- attr nestedmark :: '$prop'(bitpack(rest)).
        }.
        :- sub(qval) {
            :- attr nestedval :: '$prop'(bitpack(rest)).
        }.
    :- else.
        :- sub(qval) {
            :- attr nestedmark :: '$prop'(bitpack(upper)).
            :- attr nestedval :: '$prop'(bitpack(rest)).
        }.
    :- endif.
}.

% ---------------------------------------------------------------------------
% List constructor
:- class lsttagged < structtagged {
    :- bitstr.
    :- key(tag, lst).
    :- attr ptr :: /*compat*/ ref1(mut(tagged)).
    :- attr ptr :: '$prop'(bitpack(rest)).
    :- smallptrmodel(ptr, uintval).
}.
unify_nva_nva__same_tag_diff_value(U, V, TreatTuple) :- lsttagged(@U), !, % lst x lst
	'$trust_type'(@V, lsttagged), % TODO: infer from the same_tag/2 check?
	'$unfold'(TreatTuple(2, ~tagged_to_car(@U), ~tagged_to_car(@V))).

% ---------------------------------------------------------------------------
% Structures (and blobs) (not lists)
:- class strtagged < structtagged {
    :- bitstr.
    :- key(tag, str).
    :- attr ptr :: /*compat*/ ref1(mut(tagged)).
    :- attr ptr :: '$prop'(bitpack(rest)).
    :- smallptrmodel(ptr, uintval).
}.
unify_nva_nva__same_tag_diff_value(U, V, TreatTuple) :- strtagged(@U), !, % str x str
	'$trust_type'(@V, strtagged), % TODO: infer from the same_tag/2 check?
	T1 = ~tagged_to_head_functor(@V),
	( ~tagged_to_head_functor(@U) \== T1 -> '$cont'(failinscont) ; true ),
	( functor_is_blob(T1) -> % str(blob)
	    ( compare_blob((@U).ptr, (@V).ptr) -> true ; '$cont'(failinscont) )
	; % str(struct)
	  '$unfold'(TreatTuple(~arity(T1), ~tagged_to_arg(@U,1), ~tagged_to_arg(@V,1)))
	).

% ---------------------------------------------------------------------------
% TODO: Other methods
u_constraint__3(T1, T2, A) :- ((@T1).tag = hva ; (@T1).tag = sva), !,
	unify_ref_ref(@T2,@T1), A <- @T2.
u_constraint__3(T1, T2, _) :- bind_ref(@T2,@T1). % TODO: cannot use bind because it uses 'younger' (it does not know that T2 is always younger than T1)
% SVA
unify_local_value__2(T1, A) :- (@T1).tag = sva, !,
	B2 = ~clonemut(T1),
	bind_ref(@B2, ~tagged(hva, A)),
	T1 <- ~tagged(hva, A).
unify_local_value__2(_, _).

% ---------------------------------------------------------------------------
% Error catch
% TODO: define automatically... (some methods cannot fail)
trail_cond(A) :- '$error'(trail(A)).
younger(V, U) :- '$error'(younger(V,U)).
ref_prior(U, P) :- '$error'(ref_prior(V,U)).
inc_wake_count(A) :- '$error'(inc_wake_count(A)).
%
treat_tuple_pair__action(Action, _, _, _) :- '$error'(treat_tuple_pair__action(Action)).
treat_tuple_pair__vt_undo(Action) :- '$error'(treat_tuple_pair__vt_undo(Action)).
treat_tuple_pair__vt_test_overflow(Action) :- '$error'(treat_tuple_pair__vt_test_overflow(Action)).
treat_tuple_pair__prepare(Action, _, _, _, _, _) :- '$error'(treat_tuple_pair__prepare(Action)).

% other tagged classes
% TODO: use regtypes? use a constant attribute in the class?
% TODO: use negation! define as:
%  nonstackreftagged(T) :- reftagged(T), \+ svatagged(T).
:- bitstr_or(nonstackreftagged, tagged, [hvatagged, cvatagged]).
% TODO: use negation! define as:
%  nonstacktagged(T) :- tagged(T), \+ svatagged(T).
:- bitstr_or(nonstacktagged, tagged, [nonstackreftagged, nonreftagged]).

% TODO: define like unify/2...
:- pred v__bind0/2 + prop(unfold).
v__bind0(A2, B2) :-
%	derefvar(A2),
	( '$expandcond'(reftagged(@A2)) ->
	    ( '$expandcond'(reftagged(@B2)) ->
	        unify_ref_ref(@A2, @B2)
	    ; bind_ref(@A2, @B2)
	    )
	; 
%	    derefvar(B2),
	    ( '$expandcond'(reftagged(@B2)) ->
	        bind_ref(@B2, @A2)
	    ; '$error'(v__bind0(A, B))
	    )
	).

% tag encoding
% note: if tag encoding changes, tag tests must be updated too
:- enum_encode('tagged_$key'/1, [
	(hva, 2'000), % heap variable (value is a tagged pointer)
	(cva, 2'001), % constrained variable (value is a tagged pointer)
	(sva, 2'010), % stack variable (value is a tagged pointer)
%	(ubv, 2'011), % (unused)
	(num, 2'100), % number (value is a small integer)
	(atm, 2'101), % atom (value is an index in the atom table)
	(lst, 2'110), % list (value is a tagged pointer)
	(str, 2'111) % structure (value is a tagged pointer)
]).
:- pred tag_enc/2 + lowentrymacrofuncons([iany], intmach, 'TAG').
tag_enc(X) := ~'$keytable'(X, [
	case(hva, ~'$etype__encode'('tagged_$key', hva)),
	case(cva, ~'$etype__encode'('tagged_$key', cva)),
	case(sva, ~'$etype__encode'('tagged_$key', sva)),
%	case(ubv, ~'$etype__encode'('tagged_$key', ubv)), % (unused)
	case(num, ~'$etype__encode'('tagged_$key', num)),
	case(atm, ~'$etype__encode'('tagged_$key', atm)),
	case(lst, ~'$etype__encode'('tagged_$key', lst)),
	case(str, ~'$etype__encode'('tagged_$key', str))
]).

% boolean
:- lowtype(bool).
:- type(bool/1) + enum.
bool(T) :- T = ~'$base'(intmach), T = (false|true).
:- enum_encode(bool/1, [
	(false, 0),
	(true, 1)]).

% TODO: define as a array to char (with some properties)
% :- lowtype(cstring).
:- type(cstring/1) + equiv.
cstring(T) :- T = ~mut(char).

% TODO: this is a temporal type for C pointers, it should not be used anywhere!
% :- lowtype(cptr).
:- type(cptr/1) + equiv.
cptr(T) :- T = ~mut(char).

% GC bit definitions
:- type(gctype/1) + equiv.
gctype(T) :- T = ~bitfield(2).
:- mixin gctype_def {
  :- mut gc_marked :: bittype.
  :- mut gc_reversed :: bittype.
}.
% storage constraints
:- mixin gctype_def__storage {
  :- attr gc_marked :: '$prop'(bitpack(upper)).
  :- attr gc_reversed :: '$prop'(bitpack(upper)).
}.

:- if('$use_opt'(extgc)).
% GC bits are external (in a separate section)
:- pred extgc/1 + lowentrymacrocons(intmach, 'EXTGC').
extgc := 1.
:- lowtype(extgc).
:- class extgc {
    :- bitstr.
    :- base(uint8).
%    :- base(uint32).
    :- mut gc :: gctype.
    :- mixin_extends(gctype_def).
    % storage constraints
    :- attr gc :: '$prop'(bitpack(lower)).
    :- sub(gc) {
        :- mixin_extends(gctype_def__storage).
    }.
}.
:- else.
'$use_opt'(nestedmarksgc).
:- endif.

% TODO: define stagged/1 automatically from tagged/1 definition
% TODO: activate stagged only when necessary
%    stagged: signed integer that can hold the value of a tagged
:- lowtype(stagged).
:- type(stagged/1) + equiv.
:- if('$use_opt'(tagged64)).
stagged(T) :- T = ~int64.
:- else.
stagged(T) :- T = ~int32.
:- endif.

% TODO: ADD this order: Post from most specific to more general, Post from shorter to larger, Pre from most specific to more general
% TODO: review costs, specially for non H3 modes
% enctagtest(Pos, Post, Pre, Cost, Var, Def)

% Fast tests for 3 upper bits
:- tagtestdefcustom('TagH3_HasTag(X,T)', '(TagOf((X)) == (T))').
:- tagtestdefcustom('TAGH3_OFFSET', '(tagged__size-3)').
:- tagtestdefcustom('TagH3_t', 'tagged_t').
:- tagtestdefcustom('sTagH3_t', 'stagged_t').
:- enctagtest('H3', (0), (any), 2, 'X', '((X) < (TagH3_t)1<<TAGH3_OFFSET)').
:- enctagtest('H3', (1), (0;1;2), 2, 'X', '((X)&((TagH3_t)1<<TAGH3_OFFSET))').
:- enctagtest('H3', (1), (0;1;2;3), 2, 'X', '((sTagH3_t)(X<<1) >= (sTagH3_t)((TagH3_t)1<<1<<TAGH3_OFFSET))').
:- enctagtest('H3', (1), (any), 2, 'X', 'TagH3_HasTag(X,1)').
:- enctagtest('H3', (2), (0;1;2), 2, 'X', '((X)&((TagH3_t)2<<TAGH3_OFFSET))').
:- enctagtest('H3', (2), (0;1;2;4;5;6;7), 2, 'X', '((sTagH3_t)(X) >= (sTagH3_t)((TagH3_t)2<<TAGH3_OFFSET))').
:- enctagtest('H3', (4), (any), 2, 'X', '((sTagH3_t)(X) < (sTagH3_t)((TagH3_t)5<<TAGH3_OFFSET)) ').
:- enctagtest('H3', (5), (4;5;6;7), 2, 'X', '((sTagH3_t)(X<<1) >= (sTagH3_t)((TagH3_t)5<<1<<TAGH3_OFFSET))').
:- enctagtest('H3', (5), (any), 2, 'X', 'TagH3_HasTag(X,5)').
:- enctagtest('H3', (6), (any), 2, 'X', 'TagH3_HasTag(X,6)').
:- enctagtest('H3', (7), (any), 2, 'X', '((X) >= ((TagH3_t)7<<TAGH3_OFFSET))').
:- enctagtest('H3', (0;1), (any), 2, 'X', '((X) < ((TagH3_t)2<<TAGH3_OFFSET))').
:- enctagtest('H3', (0;2), (0;1;2), 1, 'X', '(!TagH3_Pre012_Is1((X)))'). % complementary
:- enctagtest('H3', (4;5), (4;5;6;7), 2, 'X', '(!TagH3_Pre4567_Is67((X)))'). % complementary
:- enctagtest('H3', (4;5), (any), 2, 'X', '((sTagH3_t)(X) < (sTagH3_t)((TagH3_t)6<<TAGH3_OFFSET))').
:- enctagtest('H3', (4;6), (4;5;6;7), 1, 'X', '(!TagH3_Pre4567_Is57((X)))'). % complementary
:- enctagtest('H3', (5;7), (4;5;6;7), 1, 'X', '((X)&((TagH3_t)1<<TAGH3_OFFSET))').
:- enctagtest('H3', (6;7), (4;5;6;7), 1, 'X', '((X)&((TagH3_t)2<<TAGH3_OFFSET))').
:- enctagtest('H3', (6;7), (any), 1, 'X', '((X) >= ((TagH3_t)6<<TAGH3_OFFSET))').
:- enctagtest('H3', (0;1;2;3), (any), 2, 'A', '((sTagH3_t)(A)>=0)').
:- enctagtest('H3', (0;1;6;7), (any), 2, 'A', '((sTagH3_t)((A)+((TagH3_t)2<<TAGH3_OFFSET))>=0)'). % TODO: test that the previous one works everywhere or use the following old version % :- enctagtest('H3', (0;1;6;7), (any), 'A', '((TagH3_t)(A)+((TagH3_t)2<<TAGH3_OFFSET) < ((TagH3_t)4<<TAGH3_OFFSET))').
:- enctagtest('H3', (4;5;6;7), (any), 2, 'X', '(!TagH3_Is0123((X)))'). % complementary
%
% Fast tests for 3 lower bits
% TODO: other option: rotate to the left the tag and use some TagH3 code
:- tagtestdefcustom('TagL3_t', 'tagged_t').
:- tagtestdefcustom('sTagL3_t', 'stagged_t').
:- enctagtest('L3', (0), (any), 2, 'X', 'TagOf_Is0((X))'). % relaxed precondition
:- enctagtest('L3', (1), (0;1;2), 2, 'X', 'TagL3_Is1((X))').
:- enctagtest('L3', (1), (0;1;2;3), 2, 'X', 'TagL3_Is1((X))').
:- enctagtest('L3', (1), (any), 2, 'X', '(((X)&7)==1)').
:- enctagtest('L3', (2), (0;1;2), 2, 'X', 'TagL3_Is2((X))').
:- enctagtest('L3', (2), (0;1;2;4;5;6;7), 2, 'X', '(((X)&7)==2)'). % relaxed precondition
:- enctagtest('L3', (4), (any), 2, 'X', 'TagOf_Is4((X))'). % relaxed precondition
:- enctagtest('L3', (5), (4;5;6;7), 2, 'X', 'TagOf_Is5((X))'). % relaxed precondition
:- enctagtest('L3', (5), (any), 2, 'X', 'TagOf_Is5((X))'). % relaxed precondition
:- enctagtest('L3', (6), (any), 2, 'X', 'TagOf_Is6((X))'). % relaxed precondition
:- enctagtest('L3', (7), (any), 2, 'X', 'TagOf_Is7((X))'). % relaxed precondition
:- enctagtest('L3', (0;1), (any), 2, 'X', '(((X)&7)<2)').
:- enctagtest('L3', (0;2), (0;1;2), 1, 'X', '(!TagL3_Pre012_Is1((X)))'). % complementary
:- enctagtest('L3', (4;5), (4;5;6;7), 2, 'X', '(!TagL3_Pre4567_Is67((X)))'). % complementary
:- enctagtest('L3', (4;5), (any), 2, 'X', '((((X)-4)&7)<2)').
:- enctagtest('L3', (4;6), (4;5;6;7), 1, 'X', '(!TagL3_Pre4567_Is57((X)))'). % complementary
:- enctagtest('L3', (5;7), (4;5;6;7), 1, 'X', '((X)&1)').
:- enctagtest('L3', (6;7), (4;5;6;7), 1, 'X', '((X)&2)').
:- enctagtest('L3', (6;7), (any), 1, 'X', '(((X)&7)>=6)').
:- enctagtest('L3', (0;1;2;3), (any), 2, 'A', '(!((A)&4))').
:- enctagtest('L3', (0;1;6;7), (any), 2, 'A', '((((A)+2)&7)<4)').
:- enctagtest('L3', (4;5;6;7), (any), 2, 'X', '(!TagL3_Is0123((X)))'). % complementary
%
% Fast tests for 1 upper bits + 2 lower bits
:- tagtestdefcustom('TAGS3_MASK1', '((tagged_t)1<<(tagged__size-1))').
:- tagtestdefcustom('TAGS3_MASK', '(TAGS3_MASK1|3)').
:- tagtestdefcustom('TagS3_t', 'tagged_t').
:- tagtestdefcustom('sTagS3_t', 'stagged_t').
:- enctagtest('S3', (0), (any), 2, 'X', 'TagOf_Is0((X))'). % relaxed precondition
:- enctagtest('S3', (1), (0;1;2), 2, 'X', '(((X)&3)==1)').
:- enctagtest('S3', (1), (0;1;2;3), 2, 'X', 'TagS3_Is1((X))').
:- enctagtest('S3', (1), (any), 2, 'X', '(((X)&TAGS3_MASK)==1)').
:- enctagtest('S3', (2), (0;1;2), 2, 'X', '(((X)&3)==2)').
:- enctagtest('S3', (2), (0;1;2;4;5;6;7), 2, 'X', '(((X)&TAGS3_MASK)==2)'). % relaxed precondition
:- enctagtest('S3', (4), (any), 2, 'X', 'TagOf_Is4((X))'). % relaxed precondition
:- enctagtest('S3', (5), (4;5;6;7), 2, 'X', 'TagOf_Is5((X))'). % relaxed precondition
:- enctagtest('S3', (5), (any), 2, 'X', 'TagOf_Is5((X))'). % relaxed precondition
:- enctagtest('S3', (6), (any), 2, 'X', 'TagOf_Is6((X))'). % relaxed precondition
:- enctagtest('S3', (7), (any), 2, 'X', 'TagOf_Is7((X))'). % relaxed precondition
:- enctagtest('S3', (0;1), (any), 2, 'X', '(((X)&TAGS3_MASK)<2)').
:- enctagtest('S3', (0;2), (0;1;2), 1, 'X', '(!TagS3_Pre012_Is1((X)))'). % complementary
:- enctagtest('S3', (4;5), (4;5;6;7), 2, 'X', '(!TagS3_Pre4567_Is67((X)))'). % complementary
:- enctagtest('S3', (4;5), (any), 2, 'A', '({ TagS3_t temp_a = (A); (TagS3_Is4567(temp_a) && (!TagS3_Is67(temp_a))); })'). % inline static bool_t TagS3_Is45(TagS3_t A) { return (TagS3_Is4567(A) && (!TagS3_Is67(A)); }
:- enctagtest('S3', (4;6), (4;5;6;7), 1, 'X', '(!TagS3_Pre4567_Is57((X)))'). % complementary
:- enctagtest('S3', (5;7), (4;5;6;7), 1, 'X', '((X)&1)').
:- enctagtest('S3', (6;7), (4;5;6;7), 1, 'X', '((X)&2)').
:- enctagtest('S3', (6;7), (any), 1, 'X', '(((X)&TAGS3_MASK)>=Tagt(6))').
:- enctagtest('S3', (0;1;2;3), (any), 2, 'A', '(!((A)&TAGS3_MASK1))').
:- enctagtest('S3', (0;1;6;7), (any), 2, 'A', '({ TagS3_t temp_a = (A); (TagS3_Is01(temp_a) || TagS3_Is67(temp_a)); })'). % inline static bool_t TagS3_Is0167(TagS3_t A) { return TagS3_Is01(A) || TagS3_Is67(A); }
:- enctagtest('S3', (4;5;6;7), (any), 2, 'X', '(!TagS3_Is0123((X)))'). % complementary
/* fast tests for double word taggeds (TagB3 tests applied to TagOf) */
:- tagtestdefcustom('TagD3_t', 'tagged_t').
:- tagtestdefcustom('sTagD3_t', 'stagged_t').
:- enctagtest('D3', (0), (any), 2, 'X', 'TagOf_Is0((X))'). % relaxed precondition
:- enctagtest('D3', (1), (0;1;2), 2, 'A', 'TagB3_Pre012_Is1(TagOf((A)))').
:- enctagtest('D3', (1), (0;1;2;3), 2, 'A', 'TagB3_Pre0123_Is1(TagOf((A)))').
:- enctagtest('D3', (1), (any), 2, 'A', 'TagB3_Is1(TagOf((A)))').
:- enctagtest('D3', (2), (0;1;2), 2, 'A', 'TagB3_Pre012_Is2(TagOf((A)))').
:- enctagtest('D3', (2), (0;1;2;4;5;6;7), 2, 'X', 'TagB3_Is2(TagOf((A)))'). % relaxed precondition
:- enctagtest('D3', (4), (any), 2, 'X', 'TagOf_Is4((X))'). % relaxed precondition
:- enctagtest('D3', (5), (4;5;6;7), 2, 'X', 'TagOf_Is5((X))'). % relaxed precondition
:- enctagtest('D3', (5), (any), 2, 'X', 'TagOf_Is5((X))'). % relaxed precondition
:- enctagtest('D3', (6), (any), 2, 'X', 'TagOf_Is6((X))'). % relaxed precondition
:- enctagtest('D3', (7), (any), 2, 'X', 'TagOf_Is7((X))'). % relaxed precondition
:- enctagtest('D3', (0;1), (any), 2, 'A', 'TagB3_Is01(TagOf((A)))').
:- enctagtest('D3', (0;2), (0;1;2), 1, 'X', '(!TagD3_Pre012_Is1((X)))'). % complementary
:- enctagtest('D3', (4;5), (4;5;6;7), 2, 'X', '(!TagD3_Pre4567_Is67((X)))'). % complementary
:- enctagtest('D3', (4;5), (any), 2, 'A', 'TagB3_Is45(TagOf((A)))').
:- enctagtest('D3', (4;6), (4;5;6;7), 1, 'X', '(!TagD3_Pre4567_Is57((X)))'). % complementary
:- enctagtest('D3', (5;7), (4;5;6;7), 1, 'A', 'TagB3_Pre4567_Is57(TagOf((A)))').
:- enctagtest('D3', (6;7), (4;5;6;7), 1, 'A', 'TagB3_Pre4567_Is67(TagOf((A)))').
:- enctagtest('D3', (6;7), (any), 1, 'A', 'TagB3_Is67(TagOf((A)))').
:- enctagtest('D3', (0;1;2;3), (any), 2, 'A', 'TagB3_Is0123(TagOf((A)))').
:- enctagtest('D3', (0;1;6;7), (any), 2, 'A', 'TagB3_Is0167(TagOf((A)))').
:- enctagtest('D3', (4;5;6;7), (any), 2, 'X', '(!TagD3_Is0123((X)))'). % complementary
%
% Default tests
:- tagtestdefcustom('TagOf_t', 'tagged_t').
:- tagtestdefcustom('sTagOf_t', 'stagged_t').
:- enctagtest('Of', (0), (any), 2, 'X', 'HasTag((X),0)').
:- enctagtest('Of', (1), (0;1;2), 2, 'X', 'TagOf_Is1((X))'). % relaxed precondition
:- enctagtest('Of', (1), (any), 2, 'X', 'HasTag((X),1)').
:- enctagtest('Of', (2), (0;1;2), 2, 'X', 'TagOf_Is2((X))'). % relaxed precondition
:- enctagtest('Of', (2), (0;1;2;3), 2, 'X', 'TagOf_Is2((X))'). % relaxed precondition
:- enctagtest('Of', (2), (0;1;2;4;5;6;7), 2, 'X', 'TagOf_Is2((X))'). % relaxed precondition
:- enctagtest('Of', (2), (any), 2, 'X', 'HasTag((X),2)').
:- enctagtest('Of', (4), (any), 2, 'X', 'HasTag((X),4)').
:- enctagtest('Of', (5), (4;5;6;7), 2, 'X', 'TagOf_Is5((X))'). % relaxed precondition
:- enctagtest('Of', (5), (any), 2, 'X', 'HasTag((X),5)').
:- enctagtest('Of', (6), (any), 2, 'X', 'HasTag((X),6)').
:- enctagtest('Of', (7), (any), 2, 'X', 'HasTag((X),7)').
:- if('$use_opt'(tagtestmask)).
:- enctagtest('Of', (0;1), (any), 2, 'A', '(((1<<TagOf(A)) & ((1<<0)|(1<<1))) != 0)').
:- else.
% inline static bool_t TagOf_Is01(TagOf_t a) { switch (TagOf(a)) { case 0: case 1: return TRUE; default: return FALSE; } }
:- enctagtest('Of', (0;1), (any), 2, 'A', '({ bool_t temp_res; switch (TagOf((A))) { case 0: case 1: temp_res = TRUE; break; default: temp_res = FALSE; } temp_res; })').
:- endif.
:- enctagtest('Of', (0;2), (0;1;2), 1, 'X', '(!TagOf_Pre012_Is1((X)))'). % complementary
:- enctagtest('Of', (4;5), (4;5;6;7), 2, 'X', '(!TagOf_Pre4567_Is67((X)))'). % complementary
:- if('$use_opt'(tagtestmask)).
:- enctagtest('Of', (4;5), (any), 2, 'A', '(((1<<TagOf(A)) & ((1<<4)|(1<<5))) != 0)').
:- else.
% inline static bool_t TagOf_Is45(TagOf_t a) { switch (TagOf(a)) { case 4: case 5: return TRUE; default: return FALSE; } }
:- enctagtest('Of', (4;5), (any), 2, 'A', '({ bool_t temp_res; switch (TagOf((A))) { case 4: case 5: temp_res = TRUE; break; default: temp_res = FALSE; } temp_res; })').
:- endif.
:- enctagtest('Of', (4;6), (4;5;6;7), 1, 'X', '(!TagOf_Pre4567_Is57((X)))'). % complementary
:- enctagtest('Of', (5;7), (4;5;6;7), 1, 'X', 'TagOf_Is57((X))'). % relaxed precondition
:- enctagtest('Of', (5;7), (any), 1, 'X', '(HasTag((X),5)||HasTag((X),7))').
:- enctagtest('Of', (6;7), (4;5;6;7), 1, 'X', 'TagOf_Is67((X))'). % relaxed precondition
:- enctagtest('Of', (6;7), (any), 1, 'X', '(HasTag((X),6)||HasTag((X),7))').
% TODO: do not use TagOf or HasTag
:- if('$use_opt'(tagtestmask)).
:- enctagtest('Of', (0;1;2;3), (any), 2, 'A', '(((1<<TagOf(A)) & ((1<<0)|(1<<1)|(1<<2)|(1<<3))) != 0)').
:- else.
% inline static bool_t TagOf_Is0123(TagOf_t a) { switch (TagOf(a)) { case 0: case 1: case 2: case 3: return TRUE; default: return FALSE; } }
:- enctagtest('Of', (0;1;2;3), (any), 2, 'A', '({ bool_t temp_res; switch (TagOf((A))) { case 0: case 1: case 2: case 3: temp_res = TRUE; break; default: temp_res = FALSE; } temp_res; })').
:- endif.
:- if('$use_opt'(tagtestmask)).
:- enctagtest('Of', (0;1;6;7), (any), 2, 'A', '(((1<<TagOf(A)) & ((1<<0)|(1<<1)|(1<<6)|(1<<7))) != 0)').
:- else.
% inline static bool_t TagOf_Is0167(TagOf_t a) { switch (TagOf(a)) { case 0: case 1: case 6: case 7: return TRUE; default: return FALSE; } }
:- enctagtest('Of', (0;1;6;7), (any), 2, 'A', '({ bool_t temp_res; switch (TagOf((A))) { case 0: case 1: case 6: case 7: temp_res = TRUE; break; default: temp_res = FALSE; } temp_res; })').
:- endif.
:- enctagtest('Of', (4;5;6;7), (any), 2, 'X', '(!TagOf_Is0123((X)))'). % complementary

% TODO: THOSE ARE REALLY USED!!! BUT HIDDEN IN INLINE_C DEFS OF THE PREVIOUS ENCTAGTEST... CHECK BEFORE REMOVING THEM!
% unused tag test def('H3', (2), (any), 'X', 'TagH3_HasTag(X,2)').
% unused tag test def('H3', (2;3), (any), 'X', '((sTagH3_t)(X) >= (sTagH3_t)((TagH3_t)2<<TAGH3_OFFSET))').
% unused tag test def('L3', (2), (any), 'X', '(((X)&7)==2)').
% unused tag test def('D3', (2), (any), 'A', 'TagB3_Is2(TagOf((A)))').
% unused tag test def('S3', (2), (any), 'X', '(((X)&TAGS3_MASK)==2)').

% Fast tests for 4 upper bits
% TODO: those tests are imcomplete and not ordered!
:- tagtestdefcustom('TAGH4_OFFSET', '(tagged__size-4)').
:- tagtestdefcustom('TAGH4_MASK', 'MakeMask(TagH4_t, 4, TAGH4_OFFSET)').
:- tagtestdefcustom('TagH4_HasTag(X,T)', '(((X) & TAGH4_MASK) == ((TagH4_t)(T)<<TAGH4_OFFSET))').
:- tagtestdefcustom('TagH4_t', 'tagged_t').
:- tagtestdefcustom('sTagH4_t', 'stagged_t').
:- enctagtest('H4', (8), (any), 2, 'X', '((sTagH4_t)(X) < (sTagH4_t)(9<<TAGH4_OFFSET))').
:- enctagtest('H4', (11), (any), 2, 'X', 'TagH4_HasTag((X), 11)').

% Fast tests for 3 lower bits when the rest of bits are set to 0
% TODO: those tests are not ordered!
:- tagtestdefcustom('TagB3_t', 'tagged_t').
:- tagtestdefcustom('sTagB3_t', 'stagged_t').
:- enctagtest('B3', (0;1;2;3), (any), 2, 'A', '(!((A)&4))').
:- enctagtest('B3', (0;1;6;7), (any), 2, 'A', '((((A)+2)&7)<4)').
:- enctagtest('B3', (0;1), (any), 2, 'X', '((X)<2)').
:- enctagtest('B3', (4;5), (any), 2, 'X', '((((X)-4)&7)<2)').
:- enctagtest('B3', (1), (any), 2, 'X', '((X)==1)').
:- enctagtest('B3', (2), (any), 2, 'X', '((X)==2)').
:- enctagtest('B3', (1), (0;1;2;3), 2, 'X', 'TagB3_Is1((X))').
:- enctagtest('B3', (6;7), (any), 1, 'X', '((X)>=6)').
:- enctagtest('B3', (5;7), (4;5;6;7), 1, 'X', '((X)&1)').
:- enctagtest('B3', (6;7), (4;5;6;7), 1, 'X', '((X)&2)').
:- enctagtest('B3', (1), (0;1;2), 2, 'X', 'TagB3_Is1((X))').
:- enctagtest('B3', (2), (0;1;2), 2, 'X', 'TagB3_Is2((X))').

% Tests for tag sets
:- if('$use_opt'(tagtestbit)).
:- if('$use_opt'(highbittags)).
:- swtest_mode('H3').
:- elif('$use_opt'(lowbittags)).
:- swtest_mode('L3').
:- elif('$use_opt'(splitbittags)).
:- swtest_mode('S3').
:- elif('$use_opt'(halfwordtags)).
:- swtest_mode('D3').
:- endif.
:- else.
:- swtest_mode('Of').
:- endif.

:- pred is_var/1 + foreign([tagged], semidet, 'IsVar').
:- swtest_macro('IsVar', (any), (hva;cva;sva)).

% TODO: port missing #define ConsTaggedIsNUM(F) (((F)&((tagged_t)1<<tagged__tag_offset)) == 0)
% TODO: port missing #define ConsTaggedIsATM(F) (((F)&((tagged_t)1<<tagged__tag_offset)) != 0)
:- swtest_macro('IsHeapPtr', (any), (hva;cva;lst;str)). % (the value contains a pointer to the heap)
:- swtest_macro('TaggedIsNUMorATM', (any), (num;atm)).
:- swtest_macro('TaggedIsCVA', (any), (cva)).
:- swtest_macro('TaggedIsSVA', (hva;cva;sva;num;atm;lst;str), (sva)).
:- swtest_macro('IsNonvarAtom', (num;atm;lst;str), (atm)).
:- swtest_macro('TaggedIsATM', (any), (atm)).
:- swtest_macro('TaggedIsHVA', (any), (hva)).
:- swtest_macro('TaggedIsNUM', (any), (num)).
:- swtest_macro('TaggedIsLST', (any), (lst)).
:- swtest_macro('TaggedIsSTR', (any), (str)).

% :- swrule_def(Cases, TagPrecondition, OptionalPrefixIns, Def)
% TODO: for the same post, order from most to less specific preconditions (like it is done for instructions!)
% TODO: define names automatically when not exported to C
% TODO: use encoded tags to define optimized rules
% TODO: take into account case priority... (deeper the leaf in the search tree, less the priority)
:- swrule_def([(num;atm),lst,str], none, macro('Sw_NUMorATM_LST_STR', [NUMorATM, LST, STR], sw(ctree, cswitch), [sw([case((str), STR), case((lst), LST)]), case((num;atm), NUMorATM)])).
%
:- swrule_def([(lst;str),(hva;cva;sva;num;atm)], none, macro('Sw_LSTorSTR_Other', [LSTorSTR, Other], sw(ctree, cswitch), [case((lst;str), LSTorSTR), case((hva;cva;sva;num;atm), Other)])).
%
:- swrule_def([(hva;cva;sva),(num;atm;lst;str)], none, macro('Sw_HVAorCVAorSVA_NVA', [HVAorCVAorSVA,NVA], ctree, [case((hva;cva;sva), HVAorCVAorSVA), case((num;atm;lst;str), NVA)])).
%
:- swrule_def([hva,cva,sva,(num;atm;lst;str)], none, macro('Sw_HVA_CVA_SVA_Other', [HVA,CVA,SVA,Other], sw(ctree, cswitch), [case((num;atm;lst;str), Other), case((sva), SVA), case((hva), HVA), case((cva), CVA)])).
%
:- swrule_def([hva,cva,sva], none, macro('SwHVAorCVAorSVA_HVA_CVA_SVA', [HVA,CVA,SVA], sw(ctree, cswitch), [case((sva), SVA), case((hva), HVA), case((cva), CVA)])).
%
:- swrule_def([hva,cva,(num;atm;lst;str)], none, macro('Sw_HVA_CVA_Other', [HVA,CVA,Other], sw(ctree, cswitch), [case((num;atm;lst;str), Other), case((hva), HVA), case((cva), CVA)])).
%
:- swrule_def([hva,cva], none, macro('Sw_HVA_CVA', [HVA, CVA], sw(ctree, cswitch), [case((cva), CVA), case((hva), HVA)])).
%
:- swrule_def([hva,(cva;sva;num;atm;lst;str)], none, swcondtt((hva;cva;sva;num;atm;lst;str), (hva))).
%
:- swrule_def([cva,(hva;sva;num;atm;lst;str)], none, swcondtt((hva;cva;sva;num;atm;lst;str), (cva))).
%
:- swrule_def([sva,(hva;cva;num;atm;lst;str)], none, swcondtt((hva;cva;sva;num;atm;lst;str), (sva))).
%
:- swrule_def([lst,(num;atm;str)], none, swcondtt((num;atm;lst;str), (lst))).
:- swrule_def([lst,(hva;cva;sva;num;atm;str)], none, swcondtt((hva;cva;sva;num;atm;lst;str), (lst))).
%
:- swrule_def([str,(num;atm;lst)], none, swcondtt((num;atm;lst;str), (str))).
:- swrule_def([str,(hva;cva;sva;num;atm;lst)], none, swcondtt((hva;cva;sva;num;atm;lst;str), (str))).
%
:- swrule_def([num,atm,lst,str], none, macro('Sw_NUM_ATM_LST_STR', [NUM, ATM, LST, STR], sw(ctree, cswitch), [sw([case((atm), ATM), case((num), NUM)]), case((lst), LST), case((str), STR)])).
%
:- swrule_def([cva,(hva;sva)], none, macro('Sw_CVA_HVAorSVA', [CVA, HVAorSVA], sw(ctree, cif), [case((cva), CVA), case((hva;sva), HVAorSVA)])).
%
:- swrule_def([(hva;cva),sva,str], none, macro('Sw_HVAorCVA_SVA_STR', [HVAorCVA, SVA, STR], sw(ctree, cswitch), [case((hva;cva), HVAorCVA), case((sva), SVA), case((str), STR)])).
%
:- swrule_def([(hva;cva;sva),str], none, macro('Sw_HVAorCVAorSVA_STR', [HVAorCVAorSVA, STR], sw(ctree, cswitch), [case((hva;cva;sva), HVAorCVAorSVA), case((str), STR)])).
%
:- swrule_def([cva,(hva;sva),(num;atm;lst;str)], none, macro('Sw_CVA_HVAorSVA_Other', [CVA, HVAorSVA, Other], sw(ctree, cswitch), [sw([case((cva), CVA), case((hva;sva), HVAorSVA)]), case((num;atm;lst;str), Other)])).
%
:- swrule_def([lst,str,(hva;cva;sva;num;atm)], none, macro('Sw_LST_STR_Other', [LST, STR, Other], sw(ctree, cswitch), [case((str), STR), case((lst), LST), case((hva;cva;sva;num;atm), Other)])).
%
:- swrule_def([lst,str], none, macro('Sw_LST_STR', [LST, STR], sw(ctree, cif), [case((str), STR), case((lst), LST)])).
%
:- swrule_def([str,lst,atm,(hva;cva;sva;num)], none, macro('Sw_STR_LST_ATM_Other', [STR, LST, ATM, Other], ctree, [case((str), STR), case((lst), LST), case((atm), ATM), case((hva;cva;sva;num), Other)])).
%
:- swrule_def([str,lst,atm], none, macro('Sw_STR_LST_ATM', [STR, LST, ATM], ctree, [case((str), STR), case((lst), LST), case((atm), ATM)])).
%
:- swrule_def([hva,sva,cva,num,atm,lst,str], none, macro('Sw_HVA_SVA_CVA_NUM_ATM_LST_STR', [HVA, SVA, CVA, NUM, ATM, LST, STR], cswitch, [case(hva, HVA), case(sva, SVA), case(cva, CVA), case(num, NUM), case(atm, ATM), case(lst, LST), case(str, STR)])).
%
:- swrule_def([(num;atm),str,(hva;cva;sva;lst)], none, macro('Sw_NUMorATM_STR_Other', [NUMorATM, STR, Other], sw(ctree, cswitch), [case((num;atm), NUMorATM), case((str), STR), case((hva;cva;sva;lst), Other)])).
%
:- swrule_def([num,str,(hva;cva;sva;atm;lst)], none, macro('Sw_NUM_STR_Other', [NUM, STR, Other], sw(ctree, cswitch), [case((str), STR), case((num), NUM), case((hva;cva;sva;atm;lst), Other)])).

% ---------------------------------------------------------------------------

:- swrule_def([(hva;cva),(num;atm;lst;str)], join(derefvar), 'HeapDerefSw_HVAorCVA_Other').
:- swrule_def([hva,cva,(num;atm;lst;str)], join(derefvar), 'HeapDerefSw_HVA_CVA_Other').
:- swrule_def([hva,cva,sva,(num;atm;lst;str)], join(derefvar), 'DerefSw_HVA_CVA_SVA_Other').
:- swrule_def([hva,cva,sva], join(derefvar), 'DerefSwHVAorCVAorSVA_HVA_CVA_SVA').
:- swrule_def([hva,cva], join(derefvar), 'DerefSwHVAorCVA_HVA_CVA').
:- swrule_def([sva,(hva;cva;num;atm;lst;str)], join(deref_up_to_sva), 'DerefUpToSVA_Sw_SVA_Other').

% ---------------------------------------------------------------------------

:- tagtestdefcustom('TaggedATMIsATMQ(F)', '((F) & QTAGMASK)').
:- if(('$use_opt'(tagtestbit), '$use_opt'(highbittags))).
% && (tagged__qtag_offset + 1 == tagged__tag_offset)
% /* 11 = 5(ATM) * 2 + 1 */
% #if TAG(atm) != 5
% #error "ATM is not 5"
% #endif
:- tagtestdefcustom('TaggedIsATMQ(F)', 'TagH4_Isb((F))').
:- else.
% TODO: use something like (((X) & (TAGMASK|QTAGMASK)) == Tagt((T))+QTAGMASK)
:- tagtestdefcustom('TaggedIsATMQ(F)', '(HasTag((F), ATM) && ((F) & QTAGMASK))').
:- endif.

:- if('$use_opt'(qtag)).
% Post: NUM (and not a functor for STR(blob(float)))
% Post: NUM && not QTAGMASK
:- if(('$use_opt'(tagtestbit), '$use_opt'(highbittags))).
% && (tagged__qtag_offset + 1 == tagged__tag_offset)
% #if TAG(num) != 4
% #error "NUM is not 4"
% #endif
:- tagtestdefcustom('TaggedIsSmall(X)', 'TagH4_Is8((X))').
:- else.
% TODO: indeed... no defined tagscheme uses it?
:- tagtestdefcustom('TaggedIsSmall(X)', '(TaggedIsNUM((X)) && !BlobHF((X)))').
:- endif.
:- else.
% Post: NUM
:- tagtestdefcustom('TaggedIsSmall(X)', 'TaggedIsNUM((X))').
:- endif.

% ---------------------------------------------------------------------------
% Other types used in the engine

:- lowtype(instance_clock).
:- type(instance_clock/1) + equiv.
instance_clock(T) :- T = ~uint16.
:- lowtype(intclick).
:- type(intclick/1) + equiv.
intclick(T) :- T = ~int64.
:- lowtype(intfreq).
:- type(intfreq/1) + equiv.
intfreq(T) :- T = ~int64.

% pointer to bytecode
:- lowtype(bcp).
:- type(bcp/1) + equiv. % TODO: not a mut, but a pointer (to elements of type ftype)
bcp(T) :- T = ~cptr. % TODO: not a mut, but a pointer (to elements of type ftype)

% storage units for blobs (for default copying and comparing operations)
% TODO:[ts] change granularity to make it optimal (is int32_t the best option in all architectures?) (but preserving the precondition)
% pre: sizeof(blob_unit_t) =< size of tagged_t, functor_t or bignum_t
:- lowtype(blob_unit).
:- type(blob_unit/1) + equiv.
blob_unit(T) :- T = ~int32.

% ---------------------------------------------------------------------------
% units and half units for bignums

:- lowtype(signed_bignum).
:- type(signed_bignum/1) + equiv.
signed_bignum(T) :- T = ~int32.
:- lowtype(bignum).
:- type(bignum/1) + equiv.
bignum(T) :- T = ~uint32.
:- lowtype(bignum_half).
:- type(bignum_half/1) + equiv.
bignum_half(T) :- T = ~uint16.

% ---------------------------------------------------------------------------
% foreign types

% TODO: refine
:- foreigntype(char, 'char'). % TODO: define int8 and uint8? or define byte? what about wchars for unicode?
:- foreigntype(uchar, 'unsigned char').
:- foreigntype(intclick, 'int64_t'). % TODO: define a typedef
:- foreigntype(intfreq, 'int64_t'). % TODO: define a typedef
:- foreigntype(blocking_type, 'BlockingType'). % TODO: redefine in ImProlog
:- foreigntype(void, 'void'). % TODO: this is strange...
:- foreigntype(liveinfo, 'liveinfo_t').
:- foreigntype('JMP_BUF', 'JMP_BUF').
:- foreigntype('FILE', 'FILE').
:- foreigntype(time, 'time_t').
:- foreigntype('THREAD_T', 'THREAD_T').
:- foreigntype('THREAD_ID', 'THREAD_ID').
:- foreigntype('LOCK', 'LOCK').
:- foreigntype('SLOCK', 'SLOCK').
:- foreigntype('CONDITION', 'CONDITION').
:- foreigntype(thread_state, 'thread_state_t').
% TODO: FIX: volatile is not a type!!! define volatile mutables
:- foreigntype('volatile+Behavior', 'volatile Behavior').
% TODO: FIX: volatile is not a type!!! define volatile mutables
:- foreigntype('volatile+int', 'volatile int').

% ---------------------------------------------------------------------------
% definitons for ftype runtime info

:- lowtype(ftype_typeid).
:- type(ftype_typeid/1) + equiv.
ftype_typeid(T) :- T = ~uint8.

% state for fmt macros
:- lowtype(fmt).
:- class fmt {
    :- struct.
    :- mut type :: intmach.
    :- mut i :: intmach.
    :- mut n :: intmach.
    % TODO: value should be a union of pointer to ftype_typeid or ftype_typeid
    :- mut value :: intmach.
}.
  
:- lowtype(ftype_base).
:- class ftype_base {
    :- struct.
    :- mut type :: intmach. % one of FTYPEDEF_*
}.

% TODO: define as an enum type and export as its encoding?
:- pred ftypedef/2 + lowentrymacrofuncons([iany], intmach, 'FTYPEDEF').
ftypedef(X) := ~'$keytable'(X, [
	case(basic, 0),
	case(str, 1),
	case(array, 2),
	case(blob, 3)
]).

:- lowtype(ftype_str).
:- class ftype_str {
    :- struct.
    :- mut type :: intmach.
    :- mut arity :: intmach.
    :- mut args :: mut(ftype_typeid). % TODO: this should be an array... not a mut(mut())
}.

:- lowtype(ftype_array).
:- class ftype_array {
    :- struct.
    :- mut type :: intmach.
    :- mut itype :: intmach.
    :- mut argtype :: intmach.
}.

:- lowtype(ftype_basic).
:- class ftype_basic {
    :- struct.
    :- mut type :: intmach.
    :- mut size :: intmach.
    :- mut smethod :: intmach.
    :- mut lmethod :: intmach.
}.

:- lowtype(ftype_blob).
:- class ftype_blob {
    :- struct.
    :- mut type :: intmach.
}.

% ---------------------------------------------------------------------------
% macros for some ftype definitions
% TODO: generate automatically from other defs
% TODO: should not be necessary if all code is in improlog

% save method for ftypes
% (to save a ftype to disk)
% note: saved value may require relocations
% TODO: document: the idea is that one can save using one absmach size different than the current one
% TODO: synchronize with table in compiler/absmach.pl
:- pred qs_enc/2 + lowentrymacrofuncons([iany], intmach, 'QS').
qs_enc(X) := ~'$keytable'(X, [
	case(integer, 2),
	case(poffset, 3),
	case(functor, 5),
	case(tagged, 6),
	case(emul_entry, 7),
	case(builtin_entry, 9),
	case(small, 8)
]).

% load methods for ftypes
% (to load a ftype from disk)
% note: final value in bytecode may not be available until relocations are performed
% TODO: document: load creates the memory representation valid for the current absmach
% TODO: synchronize with table in compiler/absmach.pl
:- pred ql_enc/2 + lowentrymacrofuncons([iany], intmach, 'QL').
ql_enc(X) := ~'$keytable'(X, [
	case(uint16, 8),
	case(uint32, 6),
	case(uint64, 9),
	case(baseptr, 3)
]).

%
:- pred ftype_id/2 + lowentrymacrofuncons([iany], intmach, 'FTYPE_id').
ftype_id(X) := ~'$keytable'(X, [
     case(f_o, ~'$ftype__id'(f_o)),
     case(f_e, ~'$ftype__id'(f_e)),
     case(f_f, ~'$ftype__id'(f_f)),
     case(f_i, ~'$ftype__id'(f_i)),
     case(f_l, ~'$ftype__id'(f_l)),
     case(f_g, ~'$ftype__id'(f_g)),
     case(f_p, ~'$ftype__id'(f_p)),
     case(f_t, ~'$ftype__id'(f_t)),
     case(f_x, ~'$ftype__id'(f_x)),
     case(f_y, ~'$ftype__id'(f_y)),
     case(f_z, ~'$ftype__id'(f_z)),
     case(f_C, ~'$ftype__id'(f_C)),
     case(f_E, ~'$ftype__id'(f_E)),
     case(f_Q, ~'$ftype__id'(f_Q)),
     case(f_Y, ~'$ftype__id'(f_Y)),
     case(f_Z, ~'$ftype__id'(f_Z)),
     case(f_b, ~'$ftype__id'(f_b))
]).

:- pred ftype_size/2 + lowentrymacrofuncons([iany], intmach, 'FTYPE_size').
ftype_size(X) := ~'$keytable'(X, [
     case(f_o, ~'$ftype__size'(f_o)),
     case(f_e, ~'$ftype__size'(f_e)),
     case(f_f, ~'$ftype__size'(f_f)),
     case(f_i, ~'$ftype__size'(f_i)),
     case(f_l, ~'$ftype__size'(f_l)),
     case(f_g, ~'$ftype__size'(f_g)),
     case(f_p, ~'$ftype__size'(f_p)),
     case(f_t, ~'$ftype__size'(f_t)),
     case(f_x, ~'$ftype__size'(f_x)),
     case(f_y, ~'$ftype__size'(f_y)),
     case(f_z, ~'$ftype__size'(f_z)),
     case(f_C, ~'$ftype__size'(f_C)),
     case(f_Cc, ~'$ftype__size'(f_Cc)),
     case(f_Ci, ~'$ftype__size'(f_Ci)),
     case(f_E, ~'$ftype__size'(f_E)),
     case(f_Q, ~'$ftype__size'(f_Q))
]).

:- pred ftype_ctype/2 + lowentrymacrofuncons([iany], intmach, 'FTYPE_ctype').
ftype_ctype(X) := ~'$keytable'(X, [
     case(f_o, ~'$ftype__enc_ctype'(f_o)),
     case(f_e, ~'$ftype__enc_ctype'(f_e)),
     case(f_f, ~'$ftype__enc_ctype'(f_f)),
     case(f_i, ~'$ftype__enc_ctype'(f_i)),
     case(f_l, ~'$ftype__enc_ctype'(f_l)),
     case(f_g, ~'$ftype__enc_ctype'(f_g)),
     case(f_p, ~'$ftype__enc_ctype'(f_p)),
     case(f_t, ~'$ftype__enc_ctype'(f_t)),
     case(f_x, ~'$ftype__enc_ctype'(f_x)),
     case(f_y, ~'$ftype__enc_ctype'(f_y)),
     case(f_z, ~'$ftype__enc_ctype'(f_z)),
     case(f_C, ~'$ftype__enc_ctype'(f_C)),
     case(f_Cc, ~'$ftype__enc_ctype'(f_Cc)),
     case(f_Ci, ~'$ftype__enc_ctype'(f_Ci)),
     case(f_E, ~'$ftype__enc_ctype'(f_E)),
     case(f_Q, ~'$ftype__enc_ctype'(f_Q))
]).

% ---------------------------------------------------------------------------

:- lowtype(absmachdef).
:- class absmachdef {
    :- struct.
    :- mut ftype_id_i :: ftype_typeid.
    :- mut ftype_id_o :: ftype_typeid.

    % TODO: this should be an array... not a mut(mut())
    :- mut ins_info :: mut(mut(ftype_base)).
    :- mut ins_n :: intmach.

    % TODO: this should be an array... not a mut(mut())
    :- mut ftype_info :: mut(mut(ftype_base)). 
    :- mut ftype_n :: intmach.

    :- mut q_pad1 :: intmach.
    :- mut q_pad2 :: intmach.
    :- mut tagged_size :: intmach.
    :- mut size_align :: intmach.
}.

:- globalvar(abscurrdef/1) + lowentry(absmachdef, 'abscurr').
% TODO: check w.r.t. type! use <- or = depending on member type
% TODO: add $ to initializers
abscurrdef__(T) :-
	T = ~absmachdef,
	%
	T.ftype_id_i <- ~'$ftype__id'(f_i),
	T.ftype_id_o <- ~'$ftype__id'(f_o),
	T.ins_info <- ~'$ins_info',
	T.ins_n <- ~'$ins_opcount',
	%
	'$ftype_info'(FTypeInfo),
	T.ftype_info <- ~'$array_elems'(FTypeInfo),
	T.ftype_n <- ~'$array_size'(FTypeInfo),
	%
	% TODO: this is CONTPAD, synchronize
	T.q_pad1 <- (128*(~'$type_size'(tagged))),
	% TODO: this is CALLPAD, synchronize
	T.q_pad2 <- (1152*(~'$type_size'(tagged))),
	%
        T.tagged_size <- ~'$type_size'(tagged),
	T.size_align <- ~'$type_size'(tagged).

% ---------------------------------------------------------------------------

% TODO: those are emitted to check sizes at runtime
:- pred x_offset_from_worker/1 + lowentrymacrocons(intmach, 'X_OFFSET_FROM_WORKER').
x_offset_from_worker := ~'$x_offset_from_worker'.
:- pred y_offset_from_frame/1 + lowentrymacrocons(intmach, 'Y_OFFSET_FROM_FRAME').
y_offset_from_frame := ~'$y_offset_from_frame'.

% ---------------------------------------------------------------------------
% Type of predicates

% TODO: predtyp member in definition/1 structure

:- pred predtype/2 + lowentrymacrofuncons([iany], intmach, 'PREDTYPE').
predtype(X) := ~'$keytable'(X, [
	case(compactcode, 0),
	case(compactcode_indexed, 1),
	case(profiledcode, 2),
	case(profiledcode_indexed, 3),
	case(undefined, 6),
	case(cbool, 7),
	case(cinsnp, 23),
	case(cvoid, 24),
	case(interpreted, 8)
]).

% ---------------------------------------------------------------------------
% Definition bits for defprops

:- pred defbits/2 + lowentrymacrofuncons([iany], intmach, 'DEFBITS').
defbits(X) := ~'$keytable'(X, [
	case(dynamic, 1),
	case(concurrent, 2),
	case(multifile, 4),
	case(hardrtexp, 8)
]).

% ---------------------------------------------------------------------------
% Error code classification (ISO PROLOG)
% (note: original C code by OGRAMA)

% TODO: xref errhandle.pl, internals.pl -> unify in a single file!

% TODO: this entangled code is indeed a box/unbox operation of the error data type!!!
% TODO: use the machinery to represent tagged words to do this automatically

% Errors identifiers cannot be zero (as 0 = -0)
:- pred instantiation_error/1 + lowentrymacrocons(intmach, 'INSTANTIATION_ERROR').
instantiation_error := 1.
:- pred type_error/2 + lowentrymacrofun([intmach], intmach, 'TYPE_ERROR').
type_error(D) := ~range_per_error * ~error_start(~'$ccons'(type, unknown)) + D.
:- pred domain_error/2 + lowentrymacrofun([intmach], intmach, 'DOMAIN_ERROR').
domain_error(D) := ~range_per_error * ~error_start(~'$ccons'(dom, unknown)) + D.
:- pred existence_error/2 + lowentrymacrofun([intmach], intmach, 'EXISTENCE_ERROR').
existence_error(D) := ~range_per_error * ~error_start(~'$ccons'(exist, unknown)) + D.
:- pred permission_error/3 + lowentrymacrofun([intmach, intmach], intmach, 'PERMISSION_ERROR').
permission_error(D, F) := ~range_per_error * ~error_start(~'$ccons'(perm, unknown)) + D*10 + F.
:- pred representation_error/2 + lowentrymacrofun([intmach], intmach, 'REPRESENTATION_ERROR').
representation_error(D) := ~range_per_error * ~error_start(~'$ccons'(repres, unknown)) + D.
:- pred evaluation_error/2 + lowentrymacrofun([intmach], intmach, 'EVALUATION_ERROR').
evaluation_error(D) := ~range_per_error * ~error_start(~'$ccons'(eval, unknown)) + D.
:- pred resource_error/1 + lowentrymacrocons(intmach, 'RESOURCE_ERROR').
resource_error := ~range_per_error * ~error_start(~'$ccons'(res, unknown)).
:- pred syntax_error/1 + lowentrymacrocons(intmach, 'SYNTAX_ERROR').
syntax_error := ~range_per_error * ~error_start(~'$ccons'(syntax, unknown)).
%:- pred system_error :: intmach + lowentrymacrocons('SYSTEM_ERROR').
:- pred system_error/1 + lowentrymacrocons(intmach, 'SYSTEM_ERROR').
system_error := ~range_per_error * ~error_start(~'$ccons'(system, unknown)).
:- pred user_exception/1 + lowentrymacrocons(intmach, 'USER_EXCEPTION').
user_exception := ~range_per_error * ~error_start(~'$ccons'(user, unknown)).

% Enough number of errors
:- pred range_per_error/1 + lowentrymacrocons(intmach, 'RANGE_PER_ERROR').
range_per_error := 100.

:- pred error_start/2 + lowentrymacrofuncons([iany], intmach, 'error_start').
error_start(X) := ~'$keytable'(X, [
	case(inst, 0),
	case(type, 1),
	case(dom, 2),
	case(exist, 3),
	case(perm, 4),
	case(repres, 5),
	case(eval, 6),
	case(res, 7),
	case(syntax, 8),
	case(system, 9),
	case(user, 10)
]).

:- pred type_errors/2 + lowentrymacrofuncons([iany], intmach, 'TYPE_ERRORS').
type_errors(X) := ~'$keytable'(X, [
	case(atom, 0),
	case(atomic, 1),
	case(byte, 2),
	case(character, 3),
	case(compound, 4),
	case(evaluable, 5),
	case(in_byte, 6),
	case(integer, 7),
	case(list, 8),
	case(number, 9),
	case(predicate_indicator, 10),
	case(variable, 11),
	case(callable, 12)
]).

:- pred domain_errors/2 + lowentrymacrofuncons([iany], intmach, 'DOMAIN_ERRORS').
domain_errors(X) := ~'$keytable'(X, [
	case(character_code_list, 0),
	case(source_sink, 1),
	case(stream, 2),
	case(io_mode, 3),
	case(not_empty_list, 4),
	case(not_less_than_zero, 5),
	case(operator_priority, 6),
	case(prolog_flag, 7),
	case(read_option, 8),
	case(flag_value, 9),
	case(close_option, 10),
	case(stream_option, 11),
	case(stream_or_alias, 12),
	case(stream_position, 13),
	case(stream_property, 14),
	case(write_option, 15),
	case(operator_specifier, 16)
]).

:- pred existence_errors/2 + lowentrymacrofuncons([iany], intmach, 'EXISTENCE_ERRORS').
existence_errors(X) := ~'$keytable'(X, [
	case(procedure, 0),
	case(source_sink, 1),
	case(stream, 2)
]).

% PERMISION_ERRORS: composed of type of action + object on which the action
% is defined

:- pred permission_types/2 + lowentrymacrofuncons([iany], intmach, 'PERMISSION_TYPES').
permission_types(X) := ~'$keytable'(X, [
	case(access, 0),
	case(create, 1),
	case(input, 2),
	case(modify, 3),
	case(open, 4),
	case(output, 5),
	case(reposition, 6)
]).

:- pred permission_objects/2 + lowentrymacrofuncons([iany], intmach, 'PERMISSION_OBJECTS').
permission_objects(X) := ~'$keytable'(X, [
	case(binary_stream, 0),
	case(source_sink, 1),
	case(stream, 2),
	case(text_stream, 3),
	case(flag, 4),
	case(operator, 5),
	case(past_end_of_stream, 6),
	case(private_procedure, 7),
	case(static_procedure, 8)
]).

:- pred representation_errors/2 + lowentrymacrofuncons([iany], intmach, 'REPRESENTATION_ERRORS').
representation_errors(X) := ~'$keytable'(X, [
	case(character_code_list, 0),
	case(in_character_code, 1),
	case(max_arity, 2),
	case(character, 3),
	case(max_integer, 4),
	case(min_integer, 5),
	case(character_code, 6)
]).

:- pred evaluation_errors/2 + lowentrymacrofuncons([iany], intmach, 'EVALUATION_ERRORS').
evaluation_errors(X) := ~'$keytable'(X, [
	case(float_overflow, 0),
	case(int_overflow, 1),
	case(e_undefined, 2),
	case(e_underflow, 3),
	case(zero_divisor, 4)
]).

% ---------------------------------------------------------------------------
% Indexing information for dynamic predicates? First simple, common cases,
% then hash table indexing.  Includes information on openness/closeness of
% concurrent facts. (MCL)

% numberof X regs used for control in choicepoints for dynamic code
%
%    X(0) - [head]
%    X(1) - [body]
%    X(2) - x2_next clause pointer / handle.
%    X(3) - [action] (see instance_cont)
%    X(4) - clock (used even in conc. predicates, although ignored).
%    X(5) - x5_next clause pointer / handle.
%    ------ The next ones, only meaningful for concurrent predicates.
%    X(6) - predicate root (needed in case there are no clause pointers - MCL).
%    X(7) - blocking/non-blocking and exited/non exited (MCL).
%    X(8) - pointer to previous dynamic concurrent choicepoint.

:- pred dynx/2 + lowentrymacrofuncons([iany], intmach, 'DYNX').
dynx(X) := ~'$keytable'(X, [
	case('X2_CHN', 2),
	case('ClockSlot', 4),
	case('X5_CHN', 5),
	case('RootArg', 6),
	case('InvocationAttr', 7),
	case('PrevDynChpt', 8),
	case('DynamicPreserved', 9)
]).

% ---------------------------------------------------------------------------
% Instruction opcodes, names and emit macros (for external code)

:- pred opcode/2 + lowentrymacrocons(intmach, 'OPCODE').
opcode(X) := ~'$ins_exported_opcodes'(X).
:- pred ins_opcount/1 + lowentrymacrocons(intmach, 'INS_OPCOUNT').
ins_opcount := ~'$ins_opcount'.

:- globalvar(ins_name/1) + lowentry(ref0(array(ref1(cstring))), 'ins_name').
ins_name__(T) :- T = ~'$ins_name_array'.

% Definitions for runtime bytecode generation (c_term)

% TODO: this should be a lowmacro, not a lowmacrocons!
:- pred emit_op/2 + lowentrymacrocons(intmach, 'EMIT_op').
emit_op(X) := ~'$ic_exported_emit_op'(X).

% ---------------------------------------------------------------------------

:- lowatom(var, 'atm_var').
:- lowatom(attv, 'atm_attv').
:- lowatom(float, 'atm_float').
:- lowatom(integer, 'atm_int').
:- lowatom(structure, 'atm_str').
:- lowatom(atom, 'atm_atm').
:- lowatom(list, 'atm_lst').

:- lowatom(success, 'atom_success'). % MARKERS
:- lowatom(failure, 'atom_failure'). % MARKERS
:- lowatom(share, 'atom_share').
:- lowatom(noshare, 'atom_noshare').
:- lowatom(user_input, 'atom_user_input').
:- lowatom(user_output, 'atom_user_output').
:- lowatom(user_error, 'atom_user_error').
:- lowatom(read, 'atom_read').
:- lowatom(write, 'atom_write').
:- lowatom(append, 'atom_append').
:- lowatom(socket, 'atom_socket').
:- lowatom(symlink, 'atom_symlink').
:- lowatom(regular, 'atom_regular').
:- lowatom(directory, 'atom_directory').
:- lowatom(fifo, 'atom_fifo').
:- lowatom(unknown, 'atom_unknown').
:- lowatom(user, 'atom_user').
:- lowatom(prolog, 'atom_prolog').
:- lowatom('<', 'atom_lessthan').
:- lowatom('>', 'atom_greaterthan').
:- lowatom('=', 'atom_equal').
:- lowatom('.', 'atom_lst').
:- lowatom([], 'atom_nil').
:- lowatom(on, 'atom_on').
:- lowatom(off, 'atom_off').
:- lowatom(error, 'atom_error').
:- lowatom(trace, 'atom_trace').
:- lowatom(debug, 'atom_debug').
:- lowatom(fail, 'atom_fail').
:- lowatom(all, 'atom_all').
:- lowatom(terse, 'atom_terse').
:- lowatom(verbose, 'atom_verbose').
:- lowatom(compiled, 'atom_compiled').
:- lowatom(interpreted, 'atom_interpreted').
:- lowatom(built_in, 'atom_builtin').
:- lowatom(true, 'atom_true').
:- lowatom('basiccontrol:true', 'atom_basiccontroltrue').
:- lowatom('$$retry_hook', 'atom_retry_hook').
:- lowatom(unprofiled, 'atom_unprofiled').
:- lowatom(profiled, 'atom_profiled').
:- lowatom(concurrent, 'atom_concurrent').
:- lowatom(wait, 'atom_wait').
:- lowatom(dynamic, 'atom_dynamic').
:- lowatom(multifile, 'atom_multifile').
:- lowatom(block, 'atom_block').
:- lowatom(no_block, 'atom_no_block').
:- lowatom(self, 'atom_self').
:- lowatom(create, 'atom_create').
:- lowatom('-', 'atom_dash').

% ---------------------------------------------------------------------------
% Iterators (for $for_each)
% TODO: define like types...

% I is the offset for the Y variables in the frame of size Size, in reverse order
:- iterator_def(yrange(Size), I,
	% TODO: too complex? and too expecific
	% TODO: find a more abstract iterator? (e.g. one that returns Y variables using muts)
	/*Init =*/ (I = ~initmut(intmach, Size - ~'$type_size'(tagged))),
	/*Cond =*/ (@I >= ~frame_size0(0)),
	/*Next =*/ (I <- @I - ~'$type_size'(tagged)),
	/*MaybeEmpty =*/ yes).
% I between Max..Min+1
:- iterator_def(revrange0(Max,Min), I,
	/*Init =*/ (I = ~initmut(intmach, Max)),
	/*Cond =*/ (@I > Min),
	/*Next =*/ (I <- @I - 1),
	/*MaybeEmpty =*/ yes).
:- iterator_def(revrange0_noinit(Min), I,
	/*Init =*/ (true),
	/*Cond =*/ (@I > Min),
	/*Next =*/ (I <- @I - 1),
	/*MaybeEmpty =*/ yes).
% I between Initial..1
:- iterator_def(revrange(Initial), I,
	/*Init =*/ (I = ~initmut(intmach, Initial)),
	/*Cond =*/ (@I \== 0),
	/*Next =*/ (I <- @I - 1),
	/*MaybeEmpty =*/ no).
% I between 0..N-1
:- iterator_def(intrange(N), I,
	/*Init =*/ (I = ~initmut(intmach, 0)),
	/*Cond =*/ (@I < N),
	/*Next =*/ (I <- @I + 1),
	/*MaybeEmpty =*/ yes).

% ---------------------------------------------------------------------------
% Native types definitions (incomplete)

% TODO: change emit_emulator so that types and typedefs are emitted in the correct order

:- lowtype(stream_node).
:- class stream_node {
    :- struct.
% streams: a doubly linked circular list
    :- mut label :: tagged.
    :- mut backward :: ref1(stream_node).
    :- mut forward :: ref1(stream_node).
    :- mut streamname :: tagged.
    :- mut streammode :: char.
    :- mut pending_char :: intmach. % from peek'ing
    :- mut isatty :: bitfield(1).
    :- mut socket_eof :: bitfield(1).
    :- mut last_nl_pos :: intmach.
    :- mut nl_count :: intmach.
    :- mut char_count :: intmach.
    :- mut streamfile :: ref1('FILE'). % not used for sockets
}.

:- lowtype(io_streams).
:- class io_streams {
    :- struct.
    :- mut input_stream_ptr :: ref1(stream_node).
    :- mut output_stream_ptr :: ref1(stream_node).
    :- mut error_stream_ptr :: ref1(stream_node).
}.

% TODO: split? modular statistics?
% TODO: use USE_GCSTATS to disable not used parts
:- lowtype(statistics).
:- class statistics {
    :- struct.
    :- mut ss_click :: intclick. % time spent stack_shifting
    :- mut ss_global :: intmach. % # global shifts
    :- mut ss_local :: intmach. % # local shifts 
    :- mut ss_control :: intmach. % # control/trail shifts
    :- mut gc_click :: intclick. % Total GC clicks (sec)
    :- mut gc_count :: intmach. % # garbage collections
    :- mut gc_acc :: intmach. % Total reclaimed heap space
    :- mut gc_longest_click :: intclick. % Longest individual GC clicks (sec)
    :- mut startclick :: intclick.
    :- mut lastclick :: intclick.
    :- mut startwallclick :: intclick.
    :- mut lastwallclick :: intclick.
    :- mut startuserclick :: intclick.
    :- mut lastuserclick :: intclick.
    :- mut startsystemclick :: intclick.
    :- mut lastsystemclick :: intclick.
    :- mut wallclockfreq :: intfreq.
    :- mut userclockfreq :: intfreq.
    :- mut systemclockfreq :: intfreq.
}.

:- lowtype(nestedstack_entry).
:- class nestedstack_entry {
    :- struct.
    :- mut p_off :: intmach.
    % TODO: this should be an array... not a mut(mut())
    :- mut s :: mut(tagged).
}.

:- lowtype(misc_info).
:- class misc_info {
    :- struct.
    % from heapgc: need to have per-worker garbage collection
    :- mut gc_total_grey :: intmach. % accumulated upper bound of garbage left 
    :- mut gcgrey :: intmach. % upper bound[?] of garbage left in old segments 
    :- mut total_found :: intmach. % the number of non-garbage words in the heap 
    :- mut cvas_found :: tagged. % the last CVA found while shunting 
    :- mut gc_aux_choice :: ref1(choice). % aux. choicepoint for the WAM registers 
    :- mut gc_choice_start :: ref1(choice).
    % TODO: not a mut(mut()), an array? subarray? stack?
    :- mut gc_heap_start :: mut(tagged).
    :- mut gc_stack_start :: ref1(frame).
    % For error handling through exceptions
    % TODO: rf1 or ref1(mut)?
    :- mut errhandler :: ref1('JMP_BUF').
    :- mut errcode :: intmach.
    :- mut errfuncname :: cstring.
    :- mut errfuncarity :: intmach.
    :- mut errargno :: intmach.
    :- mut culprit :: tagged.
    % Temporary allocation for c_term bytecode
    % TODO: an array, not a mut(mut())
    :- mut pbuff :: mut(char).
    :- mut pbuff_length :: intmach.
    % Nested stack for c_term
    % TODO: an array, not a mut(mut())
    :- mut nestedstack :: mut(nestedstack_entry).
    :- mut nestedstack_length :: intmach.
    :- mut nestedstack_count :: intmach.
    % Access the goal descriptor for this thread 
    :- mut goal_desc_ptr :: ref1(goal_descriptor).
    % Available workers are enqueued in a list. 
    :- mut next_worker :: ref1(worker).
    % Exit code 
    :- mut exit_code :: intmach.
    % This goal should stop right now! 
    :- mut stop_this_goal :: bool.
}.

% Possible actions requested from the toplevel.
:- pred gdaction/2 + lowentrymacrofuncons([iany], intmach, 'GDACTION').
gdaction(X) := ~'$keytable'(X, [
	case(no_action, 0),
	case(shares_structure, 1),
	case(has_continuation, 2),
	case(keep_stacks, 4),
	case(backtracking, 8),
	case(create_thread, 16),
	case(create_wam, 32),
	case(needs_freeing, 64)
]).

:- lowtype(goal_descriptor).
:- class goal_descriptor {
    :- struct.
    % Pointer to the WAM registers.    If NULL, no WAM has been associated to the goal
    :- mut goal_number :: intmach.
    :- mut worker_registers :: ref1(worker).
    % This defines the state of the WAM (if any) associated to the goal
    :- mut state :: thread_state.
    % If any thread is working on the WAM, these are the points to interact with it
    :- mut thread_id :: 'THREAD_ID'.
    :- mut thread_handle :: 'THREAD_T'. % Different from thread_id in Win32
    :- mut action :: intmach. % Defines the behavior of the goal % TODO: of gdaction type!
    :- mut goal :: tagged. % The pointer to the goal to execute
    :- mut goal_lock_l :: 'SLOCK'.
    :- mut forward :: ref1(goal_descriptor).
    :- mut backward :: ref1(goal_descriptor).
}.

:- lowtype(instance).
:- class instance {
    :- struct.
    :- mut forward :: ref1(instance).
    :- mut backward :: ref1(instance).
    :- mut root :: ref1(int_info).
    :- mut next_forward :: ref1(instance).
    :- mut next_backward :: ref1(instance).
    :- mut key :: tagged.
    :- mut rank :: tagged.
    % Dynamic clause lifespan
    :- mut birth :: instance_clock.
    :- mut death :: instance_clock.
    :- if('$use_opt'(regmod)).
        :- mut mark :: tagged.
    :- endif.
    :- mut pending_x2 :: ref1(instance_handle). % Seen by invocations looking at here
    :- mut pending_x5 :: ref1(instance_handle). % Seen by invocations looking at here
    :- mut objsize :: intmach. % total number of bytes
    % TODO: add automatically
    :- if((\+ '$use_opt'(pointer64), '$use_opt'(tagged64))).
        :- mut pad0 :: intmach.
    :- endif.
    :- attr emulcode :: array(ref0(mut(char)), 0).
    % methods
    :- attr 'TAIL' :: '$macrocons'(~'$type'(char)).
    :- attr 'SIZE_TAILED' :: '$macrofun'([Ptr], [ref1(instance)], ~tailed_bytes_to_size(~'$type'(instance), @Ptr.objsize)).
    :- attr 'SET_TAILED_SIZE' :: '$macropred'([Ptr, N], det, [ref1(instance), intmach], [c, c], (Ptr.objsize <- ~tailed_size_to_bytes(~'$type'(instance), N))).
}.

:- lowtype(hashtab_val).
:- union hashtab_val {
    :- mut try_chain :: ref1(try_node). % try-retry-trust as linked list
    :- mut instp :: ref1(instance). % int. clauses or recorded terms
    :- mut def :: ref1(definition). % predicate definition
    :- mut atomp :: ref1(atom). % Info on atoms and main functors 
    :- mut raw :: intmach.
    % TODO: a 64 bit integer with tagged64
    :- mut tagged :: tagged.
    :- mut tab :: ref1(hashtab).
    :- mut proc :: mut(void). % TODO: strange type... and do not use mut(mut())
    :- mut module :: ref1(module).
    % label index for compilation to C
    :- mut idx :: intmach.
}.

:- lowtype(hashtab_node).
:- class hashtab_node {
    :- struct.
    :- mut key :: tagged.
    :- attr value :: /*compat*/ ref0(hashtab_val).
}.

:- pred hashtab_size/2 + foreignfun([ref1(hashtab)], intmach, 'HASHTAB_SIZE').
:- pred hashtab_size2mask/2 + foreignfun([intmach], uintmach, 'HASHTAB_SIZE2MASK').
:- lowtype(hashtab).
:- class hashtab {
    :- struct.
    :- mut mask :: uintmach. % bitmask
    :- mut count :: intmach.
    :- if('$use_opt'(atomgc)).
        :- mut next_index :: intmach.
    :- endif.
    :- attr node :: array(ref0(mut(hashtab_node)), 0).
    % methods
    :- attr 'TAIL' :: '$macrocons'(~'$type'(hashtab_node)).
    :- attr 'SIZE_TAILED' :: '$macrofun'([N], [ref1(hashtab)], ~hashtab_size(N)).
    :- attr 'SET_TAILED_SIZE' :: '$macropred'([Ptr, N], det, [ref1(hashtab), intmach], [c, c], (Ptr.mask <- ~hashtab_size2mask(N))).
}.

:- lowtype(instance_handle).
:- class instance_handle {
    :- struct.
    :- mut inst_ptr :: ref1(instance). % Pointer to the instance
    :- mut head :: tagged. % Goal called, allows indexing
    :- mut next_handle :: ref1(instance_handle).
    :- mut previous_handle :: ref1(instance_handle).
}.

:- lowtype(int_info).
:- class int_info {
    :- struct.
    :- mut behavior_on_failure :: 'volatile+Behavior'. % behavior if no clauses match.
    :- mut clause_insertion_cond :: 'CONDITION'.
  %  JF: was:
  % #if defined(CONDITIONAL_VARS)
  %   CONDITION clause_insertion_cond;
  % #else
  %  %* SLOCK clause_insertion_cond;
  %   CONDITION clause_insertion_cond;
  % #endif
    :- mut x2_pending_on_instance :: ref1(instance_handle). % Used when pred. is empty
    :- mut x5_pending_on_instance :: ref1(instance_handle).
    %
    :- mut first :: ref1(instance).
    :- mut varcase :: ref1(instance).
    :- if('$use_opt'(optlst)).
        :- mut lstcase :: ref1(instance).
    :- endif.
    :- mut indexer :: ref1(hashtab).
}.

:- lowtype(atom).
:- class atom {
    :- struct.
    :- mut has_squote :: bitfield(1).
    :- mut has_dquote :: bitfield(1).
    :- mut has_special :: bitfield(1).
    % TODO: change in 64 bit mode
    :- mut index :: bitfield(29).
    :- if('$use_opt'(atom_locks)).
        % support for locking on atom names (MCL)
        :- mut atom_lock_l :: 'LOCK'. % May be held for a long time
        :- if('$use_opt'(general_locks)).
            :- mut atom_lock_counter :: 'volatile+int'. % For general semaphores
            :- mut counter_lock :: 'SLOCK'. % Held for a short time
        :- endif.
    :- endif.
    :- if('$use_opt'(atom_len)).
        :- mut atom_len :: uintmach.
    :- endif.
    :- if('$use_opt'(functor_table)).
        :- mut functor :: ref1(definition).
    :- endif.
    :- attr name :: array(ref0(mut(char)), 0).
    % methods
    :- attr 'TAIL' :: '$macrocons'(~'$type'(char)).
}.

:- lowtype(module).
:- class module {
    :- struct.
    % module definition
    :- mut name :: tagged. % module name
    % Module code
    :- mut defs_count :: intmach. % number of inserted definitions (temporal)
    % TODO: necessary to reuse abolish when an error is found...
    :- mut defs_size :: intmach. % number of definitions
    % TODO: should be an array, not a mut(mut())
    :- mut defs :: mut(ref1(definition)). % array to definition pointers
    :- if('$use_opt'(dynlink_mod)).
        % TODO: not a mut(mut()), strange type
        :- mut so_handle :: mut(void). 
        :- mut end_func :: cvoid0.
    :- endif.
    % Module properties
    :- mut is_static :: bitfield(1).
    :- mut is_initialized :: bitfield(1).
    :- mut enable_goaltrans :: bitfield(1).
    :- mut timestamp :: time. % when the module was loaded
    % Imported modules
    :- mut uses_size :: intmach.
    :- mut uses :: mut(tagged). % TODO: use 'module_t **uses;'? % TODO: should be an array
    % OO extensions
    :- if('$use_opt'(oo_extensions)).
        :- mut instvars_size :: intmach.
        :- mut instvars :: mut(tagged).
        :- mut instdatas_size :: intmach.
        :- mut instdatas :: mut(tagged). % TODO: should be an array
        %
        :- mut instvars_table :: ref1(hashtab).
        :- mut instdatas_table :: ref1(hashtab).
        %
        :- mut objfunctor :: tagged.
    :- endif.
    % Tables for runtime expansions
    % (required if any module uses runtime expansions)
    :- mut exports :: ref1(hashtab).
    :- mut defines :: ref1(hashtab). % (includes reexports) 
    % (required if the module uses runtime expansions)
    :- mut imports :: ref1(hashtab).
    :- if('$use_opt'(cache_local_resolve)).
        % local expansion cache
        :- mut local_cache :: ref1(hashtab).
    :- endif.
    % Init and end names
    :- mut init_name :: tagged.
    :- mut end_name :: tagged.
}.

% TODO: type definitions should be read as type construction code (not as type checking)
% TODO: change syntax to define as? :- prop choice/1 + foreigntype.
:- lowtype(choice).
:- class choice {
    :- struct.
    :- mut next_alt :: ref1(try_node).
    :- mut flags :: intmach.
    % TODO: not mut(mut()), an array? subarray? stack?
    :- mut trail_top :: mut(tagged).
    % TODO: not mut(mut()), an array? subarray? stack?
    :- mut heap_top :: mut(tagged).
    :- mut frame :: ref1(frame).
    :- mut next_insn :: bcp.
    :- mut local_top :: ref1(frame).
    % TODO: add automatically
    :- if((\+ '$use_opt'(pointer64), '$use_opt'(tagged64))).
        :- mut pad0 :: intmach.
    :- endif.
    :- attr x :: array(ref0(mut(tagged)), 0).
    % methods
    :- attr 'TAIL' :: '$macrocons'(~'$type'(tagged)).
}.

% TODO: DECLARE AUTOMATICALLY BEFORE DEFINITION!!
% TODO: declare definfo in engine__definitions.h
% TODO: this is a C union...
:- lowtype(definfo).
:- union definfo {
    :- mut raw :: mut(char). % TODO: strange type, not mut(mut())
    :- mut intinfo :: ref1(int_info).
    :- mut incoreinfo :: ref1(incore_info).
    :- mut proc :: mut(void). % TODO: strange type, not mut(mut())
}.

:- lowtype(defprops).
:- class defprops {
    :- struct.
    :- mut hardrtexp :: bitfield(1). % TODO: document
    :- mut spy :: bitfield(1). % TODO: document
    % concurrent obeys declaration and implies dynamic
    :- mut concurrent :: bitfield(1). % 1 if wait on it, 0 if closed (MCL)
    :- mut dynamic :: bitfield(1). % obeys declaration
    :- mut multifile :: bitfield(1). % obeys declaration
    :- mut nonvar :: bitfield(1). % seen P(X,...) :- var(X), !, ...
    :- mut var :: bitfield(1). % seen P(X,...) :- nonvar(X), !, ...
}.

:- lowtype(definition).
:- class definition {
    :- struct.
    :- mut enterop :: ftype_o.
    % TODO: add automatically alignments
    :- if('$use_opt'(halfword_o)).
        :- mut dummy0 :: ftype_q.
    :- endif.
    :- if('$use_opt'(functor_table)).
        :- mut atom :: intmach.
        :- mut arity :: intmach.
        :- mut self0 :: ref1(definition).
        :- mut string :: cstring.
    :- else.
        :- mut functor :: tagged.
    :- endif.
    :- if('$use_opt'(profile_calls)).
        :- mut number_of_calls :: intmach.
        :- mut time_spent :: uintmach.
    :- endif.
    % this cannot be mut (changing the value of 'properties' is not allowed, but you can change the arguments)
    :- attr properties :: /*compat*/ ref0(defprops).
    :- mut predtyp :: intmach.
    % this cannot be mut (changing the value of 'code' is not allowed, but you can change the arguments)
    :- attr code :: /*compat*/ ref0(definfo).
}.

:- lowtype(incore_info).
:- class incore_info {
    :- struct.
    :- mut clauses :: ref1(emul_info). % first clause
    :- mut clauses_tail :: mut(ref1(emul_info)). % "next" field of last clause
    :- mut varcase :: ref1(try_node).
    :- if('$use_opt'(optlst)).
        :- mut lstcase :: ref1(try_node).
    :- endif.
    :- mut othercase :: ref1(hashtab).
}.

% WARNING: the size of this structure cannot change !!! (The same for worker)
% a.k.a. environment
:- lowtype(frame).
:- class frame {
    :- struct.
    :- mut frame :: ref1(frame). % continuation frame pointer
    :- mut next_insn :: bcp. % continuation program pointer
    :- attr x :: array(ref0(mut(tagged)), 0). % permanent variables
    % methods
    :- attr 'TAIL' :: '$macrocons'(~'$type'(tagged)).
}.

:- lowtype(try_node).
:- class try_node {
    :- struct.
    :- mut altop :: ftype_o. % Opcode
    % TODO: add automatically! (when necessary, for alignment, based on size of ftype_o)
    :- if('$use_opt'(halfword_o)).
        :- mut padding :: ftype_q.
    :- endif.
    :- mut next :: ref1(try_node). % Next alternative or NULL
    :- mut code :: cptr. % bcp if opcode is restore_all_next_alt or restore_all_no_alt, cinsnp if opcode is exec_cinsnp_alt
    :- mut arity :: intmach.
    :- mut clause :: ref1(emul_info).
    :- if('$use_opt'(incoreopt)).
        :- mut previous :: ref1(try_node).
    :- endif.
}.

:- lowtype(emul_info).
:- class emul_info {
    :- struct.
    :- mut next :: ref1(emul_info). % next clause
    :- if('$use_opt'(regmod)).
        :- mut mark :: tagged.
    :- endif.
    :- mut objsize :: intmach. % total number of bytes
    % TODO: add automatically
    :- if((\+ '$use_opt'(pointer64), '$use_opt'(tagged64))).
        :- mut pad0 :: intmach.
    :- endif.
    :- attr emulcode :: array(ref0(mut(char)), 0).
    % methods
    :- attr 'TAIL' :: '$macrocons'(~'$type'(char)).
    :- attr 'SIZE_TAILED' :: '$macrofun'([Ptr], [ref1(emul_info)], ~tailed_bytes_to_size(~'$type'(emul_info), @Ptr.objsize)).
    :- attr 'SET_TAILED_SIZE' :: '$macropred'([Ptr, N], det, [ref1(emul_info), intmach], [c, c], (Ptr.objsize <- ~tailed_size_to_bytes(~'$type'(emul_info), N))).
}.

:- lowtype(worker).
:- class worker {
    :- struct.
    % Space for miscelaneous stuff (or temporary hacks)
    :- mut misc :: ref1(misc_info).
    % Input and output streams
    :- mut streams :: ref1(io_streams).
    % Enable/disable debugging
    :- mut debugger_mode :: tagged.
    :- mut liveinfo :: bcp.
    % Temporary allocation for various operations regarding atoms and strings.
    :- mut atom_buffer :: mut(char).
    :- mut atom_buffer_length :: intmach.
    % Root of global logical variables
    :- if((\+ '$use_opt'(pointer64), '$use_opt'(tagged64))).
        :- mut pad0 :: intmach.
    :- endif.
    :- mut global_vars_root :: tagged.
    :- mut reg_bank_size :: intmach.
    % TODO: strange type, not mut(mut())
    :- mut dummy03 :: mut(void).
    :- mut top_conc_chpt :: ref1(choice). % Topmost choicepoint for concurrent facts
    % Boundaries of different areas
    :- mut heap_start :: mut(tagged). % TODO: strange type, not mut(mut()), a subarray? array? stack?
    :- mut heap_warn_soft :: mut(tagged). % TODO: strange type, not mut(mut()), a subarray? array? stack?
    :- mut heap_end :: mut(tagged). % TODO: strange type, not mut(mut()), a subarray? array? stack?
    :- mut int_heap_warn :: mut(tagged).	% Heap_Start if control-C was hit, else Heap_Warn % TODO: strange type, not mut(mut()), a subarray? array? stack?
    % 
    :- mut stack_start :: mut(tagged). % TODO: strange type, not mut(mut()), a subarray? array? stack?
    :- mut stack_end :: mut(tagged). % TODO: strange type, not mut(mut()), a subarray? array? stack?
    :- mut stack_warn :: mut(tagged). % TODO: strange type, not mut(mut()), a subarray? array? stack?
    % 
    :- mut choice_end :: mut(tagged). % TODO: strange type, not mut(mut()), a subarray? array? stack?
    :- mut choice_start :: mut(tagged). % TODO: strange type, not mut(mut()), a subarray? array? stack?
    %
    :- mut trail_start :: mut(tagged). % TODO: strange type, not mut(mut()), a subarray? array? stack?
    :- mut trail_end :: mut(tagged). % TODO: strange type, not mut(mut()), a subarray? array? stack?
    %
    :- mut choice :: ref1(choice). % choice pointer
    :- mut previous_choice :: ref1(choice). % choice at predicate entry
    :- mut segment_choice :: ref1(choice). % gc's segment choice point
    :- mut insn :: bcp. % program counter
    :- mut structure :: mut(tagged). % use?
    % TODO: add automatically
    :- if((\+ '$use_opt'(pointer64), '$use_opt'(tagged64))).
        :- mut pad1 :: intmach.
    :- endif.
    :- mut global_uncond :: tagged. % first uncond. global variable
    :- mut local_uncond :: tagged. % first uncond. local variable no.
    % size of value_trail extension of w->choice (see VALUETRAIL)
    :- mut value_trail :: intmach. 
    % TODO: remove?... not yet
    :- mut ins :: ref1(instance).
    % Current computation's choice
    :- mut g :: choice.
    % methods
    % TODO: get from choice's tail (using something like $typemember(choice, 'TAIL') ??
    :- attr 'TAIL' :: '$macrocons'(~'$type'(tagged)).
    :- attr 'SET_TAILED_SIZE' :: '$macropred'([Ptr, N], det, [ref1(worker), intmach], [c, c], (Ptr.reg_bank_size <- N)).
}.

:- lowtype(cinsnp0).
:- type(cinsnp0/1) + predabs.
cinsnp0(T) :- T = ~predabs(nondet, [], []).
:- lowtype(cvoid0).
:- type(cvoid0/1) + predabs.
cvoid0(T) :- T = ~predabs(det, [], []).
:- lowtype(cbool0).
:- type(cbool0/1) + predabs.
cbool0(T) :- T = ~predabs(semidet, [], []).
:- lowtype(cbool1).
:- type(cbool1/1) + predabs.
cbool1(T) :- T = ~predabs(semidet, [tagged], []).
:- lowtype(cbool2).
:- type(cbool2/1) + predabs.
cbool2(T) :- T = ~predabs(semidet, [tagged, tagged], []).
:- lowtype(cbool3).
:- type(cbool3/1) + predabs.
cbool3(T) :- T = ~predabs(semidet, [tagged, tagged, tagged], []).
:- lowtype(ctagged1).
:- type(ctagged1/1) + predabs.
ctagged1(T) :- T = ~predabs(semidet_re, [tagged], [tagged]).
:- lowtype(ctagged2).
:- type(ctagged2/1) + predabs.
ctagged2(T) :- T = ~predabs(semidet_re, [tagged, tagged], [tagged]).
% TODO: do not use those... use cvoid0, cbool0 or cinsnp0... but check
%       before if those are used for multiarity or not
:- lowtype(cvoid).
:- type(cvoid/1) + predabs.
cvoid(T) :- T = ~predabs(det, [], []).
:- lowtype(cbool).
:- type(cbool/1) + predabs.
cbool(T) :- T = ~predabs(semidet, [], []).
:- lowtype(cinsnp).
:- type(cinsnp/1) + predabs.
cinsnp(T) :- T = ~predabs(nondet, [], []).

% ---------------------------------------------------------------------------
% Definition of special operand types for bytecode instructions
% (called format-type or ftype)

% f_Y ::= f_i{f_y}
:- ftype(f_Y) {
    id(3). % TODO: strange case
    def(array(f_i,f_y)).
}.
% f_Z ::= f_i{f_z}
:- ftype(f_Z) {
    id(4). % TODO: strange case
    def(array(f_i,f_z)).
}.
% f_C C code pointer
:- ftype(f_C) {
    id(5).
    enctype(mut(char)).
    smethod(builtin_entry).
    :- if('$use_opt'(pointer64)).
        lmethod(uint64).
    :- else.
        lmethod(uint32).
    :- endif.
    % TODO: add a parameter in f_C to encode what kind of C function it
    %       is, so that it is not necessary to use trust_typed in
    %       instructions with f_C operands
    zero(f_C(_,_)).
    glb(f_C(T,X), f_C(T), f_C(T,X)).
    dectype(any). % TODO: wrong?
    decfun0(f_C(_T,N/A), ~'$custommem_smbpred'(N/A), f_C).
    dec_pat0(f_C(_T,X), X).
}.
%
:- ftype(f_Cc) {
    enctype(cbool).
    dectype(any). % TODO: wrong type
}.
%
:- ftype(f_Ci) {
    enctype(cinsnp).
    dectype(any). % TODO: wrong type
}.
% f_E predicate pointer
:- ftype(f_E) {
    id(6).
    dectype(ref1(definition)).
    enctype(ref1(definition)).
    smethod(emul_entry).
    :- if('$use_opt'(pointer64)).
        lmethod(uint64).
    :- else.
        lmethod(uint32).
    :- endif.
}.
% f_b blob (e.g. float or bigint)
:- ftype(f_b) {
    id(7).
    def(blob).
    % TODO: WRONG! .. add new definition for complex types, take into
    % account alignments
    enctype(tagged).
    dectype(mut(tagged)). % TODO: wrong type
    decfun(lrg).
}.
% f_e frame_size
:- ftype(f_e) {
    id(8).
    :- if('$use_opt'(op32)).
        dectype(int32).
        enctype(uint32).
        smethod(small).
        lmethod(uint32).
    :- else.
        dectype(int16).
        enctype(uint16).
        smethod(small).
        lmethod(uint16).
    :- endif.
    encfun(sizeop).
}.
% f_f functor
:- ftype(f_f) {
    id(9).
    enctype(tagged).
    :- if('$use_opt'(tagged64)).
        smethod(functor).
        lmethod(uint64).
    :- else.
        smethod(functor).
        lmethod(uint32).
    :- endif.
    zero(f_f(_)).
    glb(f_f(X), f_f, f_f(X)).
    dectype(tagged).
    decfun0(f_f('.'/2), ~'$custommem_smbf'('.'/2), f_f).
    dec_pat0(f_f(X), X).
}.
% f_i count
:- ftype(f_i) {
    id(10).
    :- if('$use_opt'(op32)).
        enctype(uint32).
        smethod(small).
        lmethod(uint32).
    :- else.
        enctype(uint16).
        smethod(small).
        lmethod(uint16).
    :- endif.
    dectype(intmach).
    decfun0(f_i(X), X, f_i). % TODO: fix dec_args
    zero(f_i(_)).
    glb(f_i(X), f_i, f_i(X)).
    dec_pat0(f_i(X), X).
}.
% f_l long
:- ftype(f_l) {
    id(11).
    :- if('$use_opt'(pointer64)).
        dectype(int64). % TODO: right?
        enctype(uint64).
        smethod(integer).
        lmethod(uint64).
    :- else.
        dectype(int32). % TODO: right?
        enctype(uint32).
        smethod(integer).
        lmethod(uint32).
    :- endif.
}.
% f_g liveinfo
:- ftype(f_g) {
    id(12).
    enctype(liveinfo). % TODO: WRONG! .. add new definition for complex types, take into account alignments
    def(str([f_l,f_i])).
    dectype(bcp). % TODO: wrong type
    decfun(linf).
}.
% f_p bytecode pointer
:- ftype(f_p) {
    id(13).
    dectype(bcp).
    enctype(bcp).
    smethod(poffset).
    lmethod(baseptr).
}.
% f_t term
:- ftype(f_t) {
    id(14).
    enctype(tagged).
    :- if('$use_opt'(tagged64)).
        smethod(tagged).
        lmethod(uint64).
    :- else.
        smethod(tagged).
        lmethod(uint32).
    :- endif.
    zero(f_t(_)).
    glb(f_t(X), f_t, f_t(X)).
    dectype(nonreftagged).
    decfun0(f_t([]), ~'$custommem_smbt'([]), f_t).
    dec_pat0(f_t(X), X).
}.
% f_o opcode
:- if('$use_opt'(op32)).
    :- type(ftype_o/1) + equiv. % TODO: connect with ftype__enc
    ftype_o(T) :- T = ~uint32. % TODO: connect with ftype__enc
:- else.
    :- type(ftype_o/1) + equiv. % TODO: connect with ftype__enc
    ftype_o(T) :- T = ~uint16. % TODO: connect with ftype__enc
    % TODO: find an automatic way to say that 
    %       2*sizeof(ftype_o) == sizeof(intmach)
    '$use_opt'(halfword_o).
:- endif.
:- ftype(f_o) {
    id(15).
    :- if('$use_opt'(op32)).
        dectype(uint32).
        enctype(uint32).
        smethod(small).
        lmethod(uint32).
    :- else.
        dectype(uint16).
        enctype(uint16).
        smethod(small).
        lmethod(uint16).
    :- endif.
}.
% f_x x operand
:- ftype(f_x) {
    id(16).
    :- if('$use_opt'(op32)).
        enctype(uint32).
        smethod(small).
        lmethod(uint32).
    :- else.
        enctype(uint16).
        smethod(small).
        lmethod(uint16).
    :- endif.
    glb(f_x, m, f_x).
    zero(f_x(_)).
    glb(f_x(X), m, f_x(X)).
    % TODO: do not emit bytecode with x(-1), use a different operand
    dectype(mut(tagged)).
    decfun0(f_x(-1), ~'$custommem_default_choice', dc).
    decfun0(f_x(0), ~'$custommem_x'(0), f_x).
    dec_pat0(f_x(-1), x(-1)).
    dec_pat0(f_x(0), x(0)).
    dec_pat(A, x(A)).
    encfun(xop).
    decfun(xb).
}.
% f_y y operand
:- ftype(f_y) {
    id(17).
    :- if('$use_opt'(op32)).
        enctype(uint32).
        smethod(small).
        lmethod(uint32).
    :- else.
        enctype(uint16).
        smethod(small).
        lmethod(uint16).
    :- endif.
    glb(f_y, m, f_y).
    dec_pat(A, y(A)).
    encfun(yop).
    dectype(mut(tagged)).
    decfun(yb).
}.
% f_z y operand, low bit on means unsafe
:- ftype(f_z) {
    id(18).
    :- if('$use_opt'(op32)).
        dectype(uint32).
        enctype(uint32).
        smethod(small).
        lmethod(uint32).
    :- else.
        dectype(uint16).
        enctype(uint16).
        smethod(small).
        lmethod(uint16).
    :- endif.
    encfun(zop).
}.
% f_Q pad byte
:- type(ftype_q/1) + equiv. % TODO: connect with ftype__enc
ftype_q(T) :- T = ~uint16. % TODO: connect with ftype__enc
:- ftype(f_Q) {
    id(19).
    dectype(int16).
    enctype(uint16).
    smethod(small).
    lmethod(uint16).
}.

% ---------------------------------------------------------------------------

% TODO: incomplete and hardwired... use information from split__itf about exported predicates!!
:- blt_dec(term_typing:'var'/1, bu1_var).
:- blt_dec(term_typing:'nonvar'/1, bu1_nonvar).
:- blt_dec(arithmetic:'>'/2, bu2_numgt).
:- blt_dec(arithmetic:'<'/2, bu2_numlt).
:- blt_dec(arithmetic:'=\\='/2, bu2_numne).
:- blt_dec(arithmetic:'$+'/2, fu1_plus).
:- blt_dec(arithmetic:'$-'/2, fu1_minus).
:- blt_dec(arithmetic:'$++'/2, fu1_add1).
:- blt_dec(arithmetic:'$--'/2, fu1_sub1).
:- blt_dec(arithmetic:'$+'/3, fu2_plus).
:- blt_dec(arithmetic:'$-'/3, fu2_minus).
:- blt_dec(term_basic:'arg'/3, fu2_arg).
:- blt_dec(term_basic:'functor'/3, bu3_functor).

% ---------------------------------------------------------------------------
% Foreign definitions implemented in C

:- pred valid_local_top/0 + foreign([], semidet, 'ValidLocalTop').
:- pred heap_char_offset/3 + foreignfun([mut(tagged), intmach], mut(tagged), 'HeapCharOffset').
:- pred stack_char_offset/3 + foreignfun([ref1(frame), intmach], ref1(frame), 'StackCharOffset').
:- pred choice_next0/3 + foreignfun([ref1(choice), intmach], ref1(choice), 'ChoiceNext0').
:- pred choice_cont0/3 + foreignfun([ref1(choice), intmach], ref1(choice), 'ChoiceCont0').
:- pred choice_to_tagged/2 + foreignfun([ref1(choice)], tagged, 'ChoiceToTagged').
:- pred choice_from_tagged/2 + foreignfun([tagged], ref1(choice), 'ChoiceFromTagged').
:- pred same_tag/2 + foreign([tagged, tagged], semidet, 'TaggedSameTag').
:- pred tagged_to_pointer/2 + foreignfun([tagged], mut(tagged), 'TaggedToPointer').
:- pred tagged_to_root/2 + foreignfun([tagged], ref1(int_info), 'TaggedToRoot').
:- pred tagged_to_arg/3 + foreignfun([tagged, intmach], mut(tagged), 'TaggedToArg').
:- pred tagged_to_head_functor/2 + foreignfun([tagged], tagged, 'TaggedToHeadfunctor').
:- pred compare_blob/2 + foreign([mut(tagged), mut(tagged)], semidet, 'compare_blob').
:- pred functor_is_blob/1 + foreign([tagged], semidet, 'FunctorIsBlob').

:- pred tagged_to_car/2 + foreignfun([tagged], mut(tagged), 'TaggedToCar').
:- pred trail_younger/2 + foreign([mut(tagged), mut(tagged)], semidet, 'TrailYounger').
:- pred stack_younger/2 + foreign([ref1(frame), ref1(frame)], semidet, 'StackYounger').
:- pred off_stacktop/2 + foreign([ref1(frame), ref1(frame)], semidet, 'OffStacktop').
:- pred arity/2 + foreignfun([tagged], intmach, 'Arity').
:- pred choice_arity/2 + foreignfun([ref1(choice)], intmach, 'ChoiceArity').
:- pred unsafe_var/2 + foreign([ref1(frame), tagged], semidet, 'UnsafeVar').

:- pred frame_size/2 + foreignfun([bcp], intmach, 'FrameSize').
:- pred frame_size0/2 + foreignfun([intmach], intmach, 'FrameSize0').
:- pred make_blob/2 + foreignfun([mut(tagged)], nonreftagged, 'MakeBlob').
:- pred make_small/2 + foreignfun([intval], numtagged, 'MakeSmall').
:- pred get_small/2 + foreignfun([tagged], intval, 'GetSmall').

:- pred is_deep/0 + foreign([], semidet, 'IsDeep').
:- pred is_shallowtry/0 + foreign([], semidet, 'IsShallowTry').
:- pred set_shallow_try/0 + foreignmacro([], [], 'SetShallowTry').
:- pred set_shallow_retry/0 + foreignmacro([], [], 'SetShallowRetry').
:- pred set_deep/0 + foreignmacro([], [], 'SetDeep').


:- pred validate_local_top/1 + prop(unfold) + prop(propargs).
validate_local_top(E) :-
	( '$use_opt'(lazy_localtop) ->
	    (~g).local_top <- E
	; true
	).

:- lowmacro(get_frame_top/3, det, [mut(ref1(frame)), ref1(choice), ref1(frame)], 'GetFrameTop', [m,c,c]).
:- pred get_frame_top/3 + prop(unfold) + prop(propargs).
get_frame_top(A,B,E) :-
	( '$use_opt'(lazy_localtop) ->
	    ( valid_local_top ->
	        A <- @ (~g).local_top
	    ; A <- @B.local_top,
	      ( \+ stack_younger(@A,E) ->
		  A <- ~stack_char_offset(E, ~frame_size(@ (~g).next_insn))
	      ; true
	      )
	    )
	; A <- @B.local_top,
	  ( \+ stack_younger(@A,E) ->
	      A <- ~stack_char_offset(E, ~frame_size(@ (~g).next_insn))
	  ; true
	  )
	).

:- pred invalidate_local_top/0 + prop(unfold).
invalidate_local_top :-
	( '$use_opt'(lazy_localtop) ->
	    (~g).local_top <- ~'$null_ref1'(frame)
	; true
	).

:- lowmacro(set_local_top/1, det, [ref1(frame)], 'SetLocalTop', [c]).
set_local_top(E) :-
	(~g).local_top <- E.
% used in engine__gc
:- lowmacro(update_local_top/2, det, [ref1(choice), ref1(frame)], 'UpdateLocalTop', [c,c]).
:- pred update_local_top/2 + prop(unfold) + prop(propargs).
update_local_top(B, E) :-
	get_frame_top((~g).local_top, B, E).

% warning: the frame pointed by E will be incomplete!
:- lowmacro(alloc0/1, det, [mut(ref1(frame))], 'CODE_ALLOC', [m]).
:- pred alloc0/1 + prop(unfold) + prop(propargs).
alloc0(E) :-
	get_frame_top(E, @ (~w).choice, @ (~g).frame).

:- pred set_event/0 + foreignmacro([], [], 'SetEvent').

:- pred emul_to_term/2 + foreignmacro([ref1(definition), mut(tagged)], [c,m], 'EMUL_TO_TERM').
:- pred code_kontinue/1 + foreignmacro([mut(ref1(definition))], [m], 'CODE_KONTINUE').
:- pred incore_index_alt/2 + foreignmacro([tagged, mut(ref1(try_node))], [c,m], 'INCORE_INDEX_ALT').
:- pred pred_hook/2 + foreignmacro([cstring, ref1(definition)],[c,c], 'PRED_HOOK').
:- pred fail_hook/0 + foreignmacro([], [], 'FAIL_HOOK').
:- pred test_heap_overflow/3 + foreignmacro([mut(tagged), intmach, intmach], [c,c,c], 'TEST_HEAP_OVERFLOW').
% TODO: does not exist, remove
% :- pred test_stack_overflow/2 + foreignmacro([], [c,c], 'TEST_STACK_OVERFLOW').
:- pred test_choice_overflow/2 + foreignmacro([ref1(choice), intmach], [c,c], 'TEST_CHOICE_OVERFLOW').
:- pred deref/2 + foreignmacro([mut(tagged), tagged], [m,c], 'DEREF').

:- pred 'INS_HOOK_R'/1 + foreignmacro([int32], [c], 'INS_HOOK_R').
:- pred 'INS_HOOK_W'/1 + foreignmacro([int32], [c], 'INS_HOOK_W').
:- pred 'INS_HOOK_U'/1 + foreignmacro([int32], [c], 'INS_HOOK_U').

:- pred tailed_bytes_to_size/3 + foreignfun([iany, intmach], intmach, 'TAILED_BYTES_TO_SIZE').
:- pred tailed_size_to_bytes/3 + foreignfun([iany, intmach], intmach, 'TAILED_SIZE_TO_BYTES').

% ---------------------------------------------------------------------------

:- lowmacro(code_choice_new/2, det, [mut(ref1(choice)), ref1(try_node)], 'CODE_CHOICE_NEW', [m,c]).
:- pred code_choice_new/2 + prop(unfold) + prop(propargs).
code_choice_new(B, Alt) :-
	update_local_top(@ (~w).choice, @ (~g).frame),
	code_choice_new0(B, @ (~w).choice, Alt),
	test_choice_overflow(@B, ~choice_pad).

% Pre: UpdateLocalTop(B0, G->frame) has been executed;
% Input: B0=current choice point (top of the choice stack), HEAP_TOP, ALT;
% Output: B=next choice point;
:- lowmacro(code_choice_new0/3, det, [mut(ref1(choice)), ref1(choice), ref1(try_node)], 'CODE_CHOICE_NEW0', [m,c,c]).
:- pred code_choice_new0/3 + prop(unfold) + prop(propargs).
code_choice_new0(B, B0, Alt) :-
	(~g).next_alt <- Alt,
	B <- ~choice_next0(B0, @Alt.arity),
	B.flags <- 0,
	B.trail_top <- @ (~g).trail_top,
	B.local_top <- @ (~g).local_top,
	B.heap_top <- @ (~g).heap_top,
	set_shallow_try,
	set_choice0(@B).

:- lowmacro(choice_patch/2, det, [ref1(choice), ref1(try_node)], 'CODE_CHOICE_PATCH', [c,c]).
:- pred choice_patch/2 + prop(unfold) + prop(propargs).
choice_patch(B, Alt) :-
	% TODO: study if it is possible to patch only one next_alt
	B.next_alt <- Alt,
	(~g).next_alt <- Alt.

:- pred set_choice0/1 + prop(unfold) + prop(propargs).
set_choice0(Chpt) :-
	(~w).global_uncond <- ~tagged(hva, @Chpt.heap_top),
	(~w).local_uncond <- ~tagged(sva, @Chpt.local_top),
	(~w).choice <- Chpt.

:- lowmacro(set_choice/1, det, [ref1(choice)], 'SetChoice', [c]).
:- pred set_choice/1 + prop(unfold).
set_choice(Chpt) :-
	(~g).next_alt <- @Chpt.next_alt,
	set_choice0(Chpt).

:- lowmacro(heap_push/2, det, [mut(mut(tagged)),tagged], 'HeapPush', [m,c]).
:- pred heap_push/2 + prop(unfold) + prop(propargs).
heap_push(H,X) :- % note: X often contains H
	@H <- X,
	H <- ~'$mut_move'(@H, 1).

:- lowmacro(constrhva/1, det, [mut(mut(tagged))], 'ConstrHVA', [m]).
:- pred constrhva/1 + prop(unfold).
constrhva(H) :-
	heap_push(H,~tagged(hva, @H)).

:- lowmacro(loadhva/2, det, [mut(tagged),mut(mut(tagged))], 'LoadHVA', [m,m]).
:- pred loadhva/2 + prop(unfold).
loadhva(To,H) :-
	Lhva_to = ~tagged(hva, @H),
	To <- Lhva_to,
	heap_push(H,Lhva_to).

:- pred loadcva/2 + prop(unfold).
loadcva(To,H) :-
	Lcva_to = ~tagged(cva, @H),
	To = ~initmut(tagged, Lcva_to),
	heap_push(H,Lcva_to).
% TODO: fix, no different definitions
:- lowmacro(loadcva_m/2, det, [mut(tagged),mut(mut(tagged))], 'LoadCVA', [m,m]).
:- pred loadcva_m/2 + prop(unfold).
loadcva_m(To,H) :-
	Lcva_to = ~tagged(cva, @H),
	To <- Lcva_to,
	heap_push(H,Lcva_to).

:- pred ref_heap_next/1 + prop(unfold).
ref_heap_next(To) :-
	From = ~'$evalmem'(~r_s),
	To <- @(@From),
	'$trust_type'(@To, nonstacktagged), % read from heap, cannot have stack pointers
	From <- ~'$mut_move'(@From, 1).

% TODO: replace read/write mode by S value... null|ptr(_)? (then we can
%   generate an emulator that does not use the read/write mode)
:- pred un_void/0 + prop(unfold).
un_void :-
	( '$readmode' ->
	    S = ~'$evalmem'(~r_s),
	    S <- ~heap_char_offset(@S, 1*(~'$type_size'(tagged)))
	; '$cache_h_load'(H),
	  constrhva(H),
	  '$cache_h_store'(H)
	).

:- pred un_voidr(f_i).
:- iter(un_voidr, un_void, zero).
:- pred un_voidr/1 + prop(unfold) + prop(propargs).
un_voidr(I) :-
	( '$readmode' ->
	    S = ~'$evalmem'(~r_s),
	    S <- ~heap_char_offset(@S, I*(~'$type_size'(tagged)))
	; '$for_each'(Idx, ~revrange(~'$trust_typed'(I, intmach)),
             un_void)
	).

:- pred un_var/1 + prop(unfold).
un_var(X) :-
	( '$readmode' ->
	    ref_heap_next(X)
	; '$cache_h_load'(H),
	  loadhva(X,H),
	  '$cache_h_store'(H)
	).
% TODO: see TODO:[throwfail]
:- pred un_val/1 + prop(unfold).
un_val(X) :-
	( '$readmode' ->
	    T1 = ~newmut(tagged),
	    ref_heap_next(T1),
	    u_val(X, T1)
	; '$cache_h_load'(H),
	  heap_push(H,@X),
	  '$cache_h_store'(H)
	).

un_lval(X) :-
	( '$readmode' ->
	    un_val(X)
	; T1 = ~clonemut(X),
	  '$cache_h_load'(H),
	  unify_local_value(T1, H),
	  '$cache_h_store'(H)
	).

:- lowmacro(unify_local_value/2, det, [mut(tagged),mut(mut(tagged))], 'Unify_local_value', [m,m]).
:- pred unify_local_value/2 + prop(unfold).
unify_local_value(T1, H) :-
	deref_up_to_sva(T1),
	unify_local_value__2(T1, @H),
	heap_push(H, @T1).

un_fval(X) :-
	T0 = ~newmut(tagged), % TODO: necessary to avoid mem conflicts when specmode is off (FIX it by capturing output vars of if-then-else branches!)
	un_var(T0),
	u_fval(T0,X).

:- pred un_lval_un_voidr(m,f_i).
un_lval_un_voidr(A, B) :-
	T1 = ~newmut(tagged),
	ref_heap_next(T1),
	un_voidr(B),
	u_val(A, T1).

un_lval_un_var(A, B) :-
	% TODO: cannot move later since operands may refer to the same element
	T0 = ~clonemut(A),
	T1 = ~newmut(tagged),
	ref_heap_next(T1),
	un_var(B),
	u_val(T0, T1).

:- defmode(inits, w).
:- pred inits/1 + prop(unfold).
inits(A) :-
	A <- ~tagged(sva, A).

:- defmode(inith, w).
:- pred inith/1 + prop(unfold).
inith(A) :-
	'$cache_h_load'(H),
	loadhva(A, H),
	'$cache_h_store'(H).

:- defmode(init2h, w).
init2h(A, B) :-
	'$cache_h_load'(H),
	T = ~tagged(hva, @H),
	B <- T,
	A <- T,
	heap_push(H,T),
	'$cache_h_store'(H).

:- defmode(init2s, w).
init2s(A, B) :-
	T = ~tagged(sva, B),
	B <- T,
	A <- T.

% TODO: see TODO:[throwfail]
:- pred un_cons(f_t).
un_cons(Cons) :- un_cons0(Cons).
:- pred un_cons0/1 + prop(unfold).
un_cons0(Cons) :-
	( '$readmode' ->
	    T1 = ~newmut(tagged),
	    ref_heap_next(T1),
	    u_cons0(T1, Cons)
	; '$cache_h_load'(H),
	  heap_push(H, Cons),
	  '$cache_h_store'(H)
	).

% TODO: see TODO:[throwfail]
% TODO: add continuation in bin preds as a object!! with slots like continue, contaddress, isBytecode, etc.
:- pred u_cons(m,f_t).
:- defmode(u_cons, r).
u_cons(A, Cons) :-
	T1 = ~clonemut(A),
	u_cons0(T1, Cons).

:- lowmacro(cbool_u_cons/2, det_with_failins, [nonreftagged, tagged], 'CBOOL__UnifyCons', [c,c]).
% TODO: pre: Cons is atomic
cbool_u_cons(Cons, V) :-
	U = Cons,
	T1 = ~initmut(tagged, V),
	u_cons0(T1, U).

:- defmode(u_constraint, w).
u_constraint(A) :-
	T1 = ~clonemut(A),
	'$cache_h_load'(H),
	loadcva(T2, H),
	'$cache_h_store'(H),
	derefvar(T1),
	u_constraint__2(T1, T2, A).
:- pred u_constraint__2/3 + prop(unfold).
u_constraint__2(T1, T2, A) :- '$expandcond'(reftagged(@T1)), !, u_constraint__3(T1, T2, A).
u_constraint__2(T1, T2, _) :- bind_ref(@T2,@T1).

% TODO: see TODO:[throwfail]
:- pred un_str(f_f).
un_str(F) :- un_str0(F).
:- pred un_str0/1 + prop(unfold).
un_str0(F) :-
	( '$readmode' ->
	    T1 = ~newmut(tagged),
	    ref_heap_next(T1),
	    u_str0(T1, F)
	; functor__push(F)
	).

% TODO: see TODO:[throwfail]
:- pred u_str(m,f_f).
:- defmode(u_str, r).
u_str(A, F) :-
	'$set_mode'(r), % TODO: prefmode
	T1 = ~clonemut(A),
	u_str0(T1, F).

% ---------------------------------------------------------------------------
% TODO: abstract as a functor object with different methods
% TODO: do not use $ct=_evalmem, but a type check... define a special type for '.'/2 when optlst is enabled
% TODO: define special guards that order clauses by most to less specific cases.

% bind T1 with a new structure/list in the heap
:- pred functor__bind/2 + prop(unfold).
functor__bind(F, T1) :- '$use_opt'(optlst), '$ct=_evalmem'(F, ~'$custommem_smbf'('.'/2)), !,
	'$cache_h_load'(H),
	bind_ref(@T1,~tagged(lst,@H)).
functor__bind(F, T1) :-
	'$cache_h_load'(H),
	bind_ref(@T1,~tagged(str,@H)),
	heap_push(H, F),
	'$cache_h_store'(H).
% load in A a new structure/list in the heap
:- pred functor__load/2 + prop(unfold).
functor__load(F, A) :- '$use_opt'(optlst), '$ct=_evalmem'(F, ~'$custommem_smbf'('.'/2)), !,
	'$cache_h_load'(H),
	A <- ~tagged(lst,@H).
functor__load(F, A) :-
	'$cache_h_load'(H),
	A <- ~tagged(str,@H),
	heap_push(H, F),
	'$cache_h_store'(H).
% push in the heap a tagged word binded to a new structure/list in the heap
:- pred functor__push/1 + prop(unfold).
functor__push(F) :- '$use_opt'(optlst), '$ct=_evalmem'(F, ~'$custommem_smbf'('.'/2)), !,
	'$cache_h_load'(H),
	% TODO: define a '$substack' predicate a-la '$subarray'... define the type of H as 'stackiter'? or 'substack? abstract all heap_char_offset operations, use the same for frame stack, etc.
	heap_push(H, ~tagged(lst,~heap_char_offset(@H,1*(~'$type_size'(tagged))))),
	'$cache_h_store'(H).
functor__push(F) :-
	'$cache_h_load'(H),
	heap_push(H, ~tagged(str,~heap_char_offset(@H,1*(~'$type_size'(tagged))))),
	heap_push(H, F),
	'$cache_h_store'(H).
% check that T1 is a structure with the given functor or a list
:- pred functor__check/2 + prop(unfold).
functor__check(F, T1) :- '$use_opt'(optlst), '$ct=_evalmem'(F, ~'$custommem_smbf'('.'/2)), !,
	(@T1).tag = lst.
functor__check(F, T1) :-
	(@T1).tag = str,
	% TODO: tag to head functor and tag to arg may share	
	~tagged_to_head_functor(@T1) == F.
% set S register to the start of the structure/list
:- pred functor__s/2 + prop(unfold).
functor__s(F, T1) :- '$use_opt'(optlst), '$ct=_evalmem'(F, ~'$custommem_smbf'('.'/2)), !,
	~r_s <- (@T1).ptr.
functor__s(F, T1) :-
	~r_s <- ~tagged_to_arg(@T1,1).

% ---------------------------------------------------------------------------

% TODO: see TODO:[throwfail]
:- pred un_blob(f_b).
un_blob(L) :- un_blob0(L).
:- pred un_blob0(f_b) + prop(unfold).
un_blob0(L) :-
	( '$readmode' ->
	    T1 = ~newmut(tagged),
	    ref_heap_next(T1),
	    u_blob0(T1, L)
	; '$cache_h_load'(H),
	  OldH = @H,
	  H <- ~heap_char_offset(@H,1*(~'$type_size'(tagged))),
	  '$cache_h_store'(H),
	  '$set_mode'(r),
	  OldH <- ~make_blob(L)
	).

% TODO: see TODO:[throwfail]
:- pred u_blob(m,f_b).
:- defmode(u_blob, r).
u_blob(A, L) :-
	T1 = ~clonemut(A),
	u_blob0(T1, L).

:- pred blob__bind/2 + prop(unfold).
blob__bind(T1, L) :-
	bind_ref(@T1,~make_blob(L)).
:- pred blob__check/2 + prop(unfold) + prop(propargs).
blob__check(T1, L) :-
	T1.tag = str,
	compare_blob(L, T1.ptr).

:- defmode(alloc, none).
:- pred alloc/0 + prop(unfold).
alloc :-
	F = ~newmut(ref1(frame)),
	alloc0(F),
	~frame <- @F.

:- defmode(globunsafe, w).
globunsafe(From, To) :-
	T0 = ~clonemut(From),
	globunsafe_common(T0),
	To <- @T0.

:- defmode(globunsafe2, w).
globunsafe2(From, To) :-
	T0 = ~clonemut(From),
	globunsafe_common(T0),
	To <- @T0,
	From <- @T0.

:- pred globunsafe_common/1 + prop(unfold).
globunsafe_common(T0) :-
	E = ~'$evalmem'(~e),
	deref_up_to_sva(T0),
	globunsafe_common__2(E, T0).
:- pred globunsafe_common__2/2 + prop(unfold) + prop(propargs).
globunsafe_common__2(E, T0) :- (@T0).tag = sva, !,
	( unsafe_var(@E, @T0) ->
	    '$cache_h_load'(H),
	    T3 = ~clonemut(T0), % TODO: hmmm svatagged not right?
	    loadhva(T0, H),
	    '$cache_h_store'(H),
	    bind_ref(@T3, @T0)
	; true
	).
globunsafe_common__2(_, _).

:- defmode(dealloc, none).
:- pred dealloc/0 + prop(unfold).
dealloc :-
	E = ~'$evalmem'(~e),
	deallocate(@E).

% E must be a frame
:- lowmacro(deallocate/1, det, [ref1(frame)], 'DEALLOCATE', [c]).
:- pred deallocate/1 + prop(unfold) + prop(propargs).
deallocate(E) :-
	(~g).next_insn <- @E.next_insn,
	(~g).frame <- @E.frame.

% used by some built-ins
:- lowmacro(make_lst/3, det, [mut(tagged),tagged,tagged], 'MakeLST', [m,c,c]).
make_lst(To,Car,Cdr) :-
	Car0 = Car,
	'$cache_h_load'(H),
	heap_push(H, Car0),
	heap_push(H, Cdr),
	To <- ~tagged(lst,~heap_char_offset(@H,-2*(~'$type_size'(tagged)))),
	'$cache_h_store'(H).

% used by some built-ins
:- lowmacro(make_str/2, det, [mut(tagged),tagged], 'MakeSTR', [m,c]).
make_str(To,Functor) :-
	'$cache_h_load'(H),
	heap_push(H, Functor),
	% TODO: use a special 'stack' type for H...
	% To <- ~tagged(str,~stack_prev(@H, tagged)), (or even, a heap_push_getstart(H, Functor, To))
	% H <- ~stack_next(@H,array(ref0(mut(tagged)), ~arity(Functor))), (or even, heap_push_uninit)
	To <- ~tagged(str,~heap_char_offset(@H,-1*(~'$type_size'(tagged)))),
	H <- ~heap_char_offset(@H,~arity(Functor)*(~'$type_size'(tagged))),
	'$cache_h_store'(H).

:- defmode(move, none).
move(A, B) :- B <- @A.

:- pred ld_cons(m,f_t).
ld_cons(A, T) :- A <- T.

:- pred ld_blob(m,f_b).
:- defmode(ld_blob, w).
ld_blob(A, L) :-
	'$set_mode'(r),
	A <- ~make_blob(L),
	'$set_mode'(w).

:- pred ld_str(m,f_f).
:- defmode(ld_str, w).
ld_str(A, F) :-
	'$set_mode__nospecmode'(w),
	functor__load(F, A).

:- pred zputn(f_Z).
:- iter(zputn, zput, yes).
% move or globalize Yb(t1) (which may be unsafe)
% TODO: strange...
:- defmode(zput, w).
:- pred zput/2 + prop(unfold) + prop(propargs).
zput(T, Xn) :-
	T1 = T,
	( T1 /\ 1 \== 0 -> 
	    T0 = ~initmut(tagged, @(~ybcomp(T1 + 1))),
	    globunsafe_common(T0),
	    ~xcomp(Xn) <- @T0
	; ~xcomp(Xn) <- @(~ybcomp(T1))
	).

getchoice(X) :-
	X <- ~choice_to_tagged(@ (~w).previous_choice).

:- pred heapmargin_call(f_l,f_i).
heapmargin_call(A, B) :-
	% TODO: without it, it does not compile with specmode off, why?
	( '$use_opt'(specmode) ->
	    ( '$readmode' ->
	        '$cached_h'(H),
		test_heap_overflow(@H, ~'$trust_typed'(A, intmach), B)
	    ; '$cached_h'(H),
	      test_heap_overflow(@H, ~'$trust_typed'(A, intmach), B)
	    )
	; '$cached_h'(H),
	  test_heap_overflow(@H, ~'$trust_typed'(A, intmach), B)
	).

% TODO: see TODO:[throwfail]
:- defmode(u_val, r).
:- pred u_val/2 + prop(unfold).
u_val(A, B) :-
	unify(A,B).

% TODO: try to do automatically: like bind sva but with inversed arguments and optimized for the case where U=~tagged(sva, U)
:- defmode(u_fval, r).
:- pred u_fval/2 + prop(unfold).
u_fval(V, U) :-
	% TODO: precond: U must be a Yb(I) expression that is a fresh variable
	'$trust_type'(@U, svatagged),
	trail_if_conditional_sva(U),
	U <- @V.

:- defmode(proceed, r).
:- pred proceed/0 + prop(contpass_true).
proceed :-
	invalidate_local_top,
	% update the cached frame register (it may be used just after a call)
	~frame <- @ (~g).frame,
	% continue with the next_insn
	call(@ (~g).next_insn).

% after wakeup (read mode)
% TODO: do anything with LocalTop?
:- pred kontinue/0 + prop(contpass_true).
kontinue :-
	'$set_mode'(r),
	Func = ~newmut(ref1(definition)),
	code_kontinue(Func),
	dealloc,
	call(@Func).

:- defmode(exit_toplevel, r).
:- pred exit_toplevel/0 + prop(contpass_true).
exit_toplevel :- true. % doing nothing in a prop(contpass_true) instruction finish the emulation loop

% TODO: see TODO:[throwfail]
:- pred retry_cbool(f_C).
:- defmode(retry_cbool, r).
retry_cbool(A) :-
	( \+ is_deep -> set_deep ; true ),
	'$trust_type'(A, ho_det_with_failins(0)),
	call(A).

:- pred exec_cinsnp(f_C).
:- defmode(exec_cinsnp, r).
:- pred exec_cinsnp/1 + prop(contpass_true).
:- pred exec_cinsnp/1 + prop(unfold) + prop(propargs).
exec_cinsnp(A) :-
	% TODO: cannot do a trust_type because A is passed as a expression, fix (introduce a f_C0 ftype)
	call(~'$trust_typed'(A, ho_nondet(0))).

:- pred failins/0 + prop(contpass_true).
failins :-
	call(~failcont).

% Complete the choice point
neck :-
	( \+ is_deep ->
	    maybe_neck,
	    ~frame <- @ (~g).local_top, % OK even before allocate
	    set_deep
	; true
	).

:- defmode(neck0, none).
% Complete the choice point + proceed
neck0 :- % TODO: why not neck?
	( \+ is_deep ->
	    maybe_neck,
	    set_deep
	; true
	).

:- lowmacro(maybe_neck/0, det, [], 'CODE_MAYBE_NECK_TRY', []).
:- pred maybe_neck/0 + prop(unfold).
maybe_neck :-
	maybe_neck0(no_clock, xargs).

% Pre: \+ is_deep
:- pred maybe_neck0/2 + prop(unfold) + prop(propargs).
maybe_neck0(Mode, Args) :-
	( is_shallowtry -> % try
	    ( Mode = update_clock ->
	        % TODO: can this clock update be done in other place?
	        ~def_clock <- @(~use_clock) + 1,
	        ( @(~def_clock) == ~'$ccons'(0xffff, instance_clock) ->
	            clock_overflow
		; true
		)
	    ; Mode = no_clock ->
	        true
	    ; '$error'(bad_neck_mode(Mode))
	    ),
	    Choice = @ (~w).choice,
	    code_neck_try0(Choice, Args)
	; true
	).

% Pre: \+ is_deep
:- lowmacro(code_neck_try/1, det, [ref1(choice)], 'CODE_NECK_TRY', [c]).
:- pred code_neck_try/1 + prop(unfold).
code_neck_try(B) :-
	code_neck_try0(B, xargs).

:- pred code_neck_try0/2 + prop(unfold) + prop(propargs).
code_neck_try0(B, Args) :-
	B.frame <- @ (~g).frame,
	B.next_insn <- @ (~g).next_insn,
	B.next_alt <- @ (~g).next_alt,
	Arity = ~newmut(intmach),
	( '$vcccteq'(Args, xargs) ->
	    Arity <- ~choice_arity(B)
	; Arity <- ~'$ctlength'(Args)
	),
	( '$vcccteq'(Args, xargs) ->
	    '$for_each'(I, ~intrange(@Arity), (
	       B.x[@I] <- @ (~g).x[@I]
            ))
	; '$copy_to_choice'(B, ~g, Args)
	).

:- pred debug_deep_backtracking/0 + foreignmacro([], [], 'DEBUG_DEEP_BACKTRACKING').
:- pred code_restore_args/1 + prop(unfold) + prop(propargs).
code_restore_args(B) :-
	code_restore_args0(B, xargs).

:- pred code_restore_args0/2 + prop(unfold) + prop(propargs).
code_restore_args0(B, Args) :-
	( is_deep ->
	    % deep backtracking
	    debug_deep_backtracking,
	    Arity = ~newmut(intmach),
	    ( '$vcccteq'(Args, xargs) ->
	        Arity <- ~choice_arity(B)
	    ; Arity <- ~'$ctlength'(Args)
	    ),
	    (~w).previous_choice <- ~choice_cont0(B, @Arity),
	    set_shallow_retry,
	    (~g).frame <- @B.frame,
	    (~g).next_insn <- @B.next_insn,
	    (~g).next_alt <- @B.next_alt,
	    (~g).local_top <- @B.local_top,
	    ( '$vcccteq'(Args, xargs) ->
	        '$for_each'(I, ~intrange(@Arity), (
		  (~g).x[@I] <- @B.x[@I]
                ))
	    ; '$copy_from_choice'(B, ~g, Args)
	    )
	; true
	).

% note: last argument is used via ~'$get_pnext'
:- pred cframe(f_e).
:- defmode(cframe, w).
:- pred cframe/1 + prop(unfold).
:- pred cframe/1 + prop(contpass_true).
cframe(_E) :-
	E = ~'$evalmem'(~e),
	code_cframe(E, ~'$get_pnext'),
	call(~'$get_pnext').

:- lowmacro(code_cframe/2, det, [mut(ref1(frame)),bcp], 'CODE_CFRAME', [m,c]).
:- pred code_cframe/2 + prop(unfold) + prop(propargs).
code_cframe(E, NextInsn) :-
	E.next_insn <- @ (~g).next_insn,
	E.frame <- @ (~g).frame,
	(~g).frame <- @E,
	(~g).next_insn <- NextInsn,
	(~g).local_top <- ~stack_char_offset(@E, ~frame_size(@ (~g).next_insn)),
	( off_stacktop(@E, ~'$ccons'('Stack_Warn', ref1(frame))) ->
	    % (the stack overflow will be performed in handle_event)
	    set_event
	; true
	).

% note: last argument is used via ~'$get_pnext'
% TODO: try to define as cframe + call ?
:- pred fcall(f_E,f_e).
:- pred fcall/2 + prop(contpass_true).
:- pred fcall/2 + prop(unfold).
fcall(A, _E) :-
	'$set_mode'(r),
	E = ~'$evalmem'(~e),
	code_cframe(E, ~'$get_pnext'),
	call(A).

% note: last argument is used via ~'$get_pnext'
% TODO: try to define as set success pointer plus jumpdef
:- pred kall(f_E,f_e).
:- pred kall/2 + prop(contpass_true).
kall(A, _E) :-
	'$set_mode'(r),
	(~g).next_insn <- ~'$get_pnext',
	call(A).

% TODO: collapse cannot be used here b/c initall operand is cframe operand
:- pred alloc_init_cframe(f_e).
:- defmode(alloc_init_cframe, w).
:- pred alloc_init_cframe/1 + prop(contpass_true).
alloc_init_cframe(E) :-
	alloc,
	initfr(E),
	cframe(E).

% TODO: collapse cannot be used here b/c initall operand is fcall operand
:- pred alloc_init_fcall(f_E,f_e).
:- defmode(alloc_init_fcall, w).
:- pred alloc_init_fcall/2 + prop(contpass_true).
alloc_init_fcall(A, E) :-
	alloc,
	initfr(E),
	fcall(A, E).

:- pred initfr/1 + prop(unfold).
initfr(E) :-
	'$for_each'(T0, ~yrange(~'$trust_typed'(E, intmach)),
          (~ybcomp(@T0) <- ~tagged(sva, ~ybcomp(@T0)))).

:- pred lastcall(f_E).
%:- defmode(lastcall, w).
:- pred lastcall/1 + prop(contpass_true).
lastcall(A) :-
	'$set_mode'(r),
	call(A).

:- pred jump(f_p).
:- pred jump/1 + prop(contpass_true).
jump(NewP) :-
	call(NewP).

% TODO: see TODO:[throwfail]
:- pred blt1(m,f_C).
:- defmode(blt1, r).
blt1(A, B) :-
	'$trust_type'(B, ho_det_with_failins(1)),
	B(@A).

% TODO: see TODO:[throwfail]
:- pred blt2(m,m,f_C).
:- defmode(blt2, r).
blt2(A, B, C) :-
	'$trust_type'(C, ho_det_with_failins(2)),
	C(@A,@B).

% TODO: see TODO:[throwfail]
:- pred blt3(m,m,m,f_C).
:- defmode(blt3, r).
blt3(A, B, C, D) :-
	'$trust_type'(D, ho_det_with_failins(3)),
	D(@A,@B,@C).

:- pred fun1(m,m,f_C,f_g).
:- defmode(fun1, r).
:- pred fun1/4 + prop(unfold).
fun1(V, A, Func, LiveInfo) :-
	(~w).liveinfo <- LiveInfo,
	'$trust_type'(Func, ho_fun(1)),
	V <- ~'$funcall'(Func, [@A]).

:- pred fun2(m,m,m,f_C,f_g).
:- defmode(fun2, r).
:- pred fun2/5 + prop(unfold).
fun2(V, A, B, Func, LiveInfo) :-
	(~w).liveinfo <- LiveInfo,
	'$trust_type'(Func, ho_fun(2)),
	V <- ~'$funcall'(Func, [@A,@B]).

:- pred noerror/1 + prop(unfold).
noerror(A) :- ( ~'$ccons'('ERRORTAG', tagged) == @A -> '$cont'(failinscont) ; true ).

% TODO: see TODO:[throwfail]
:- pred funre1(m,m,f_C,f_g).
:- defmode(funre1, r).
funre1(A, B, C, D) :-
	fun1(A, B, C, D),
	noerror(A).
% TODO: see TODO:[throwfail]
:- pred funre2(m,m,m,f_C,f_g).
:- defmode(funre2, r).
funre2(A, B, C, D, E) :-
	fun2(A, B, C, D, E),
	noerror(A).

% cut (without any environment frame)
:- defmode(cutb, r).
cutb(I) :-
	% may get hole at top of local stack
	invalidate_local_top,
	cutf(I).
% cut (when the environment frame is created, before the first call)
:- defmode(cute, r).
cute(I) :-
	% (~g).local_top may be invalidated here
	E = ~'$evalmem'(~e),
	validate_local_top(@E),
	cutf(I).
:- defmode(cutf, r).
:- pred cutf/1 + prop(unfold).
cutf(A) :-
	set_default_choice(A),
	code_cut(@ (~w).previous_choice).

% Concurrency: if we cut (therefore discarding intermediate
% choicepoints), make sure we also get rid of the linked chains which
% point to the pending calls to concurrent predicates. (MCL)

:- pred debug_cut/1 + foreignmacro([ref1(choice)], [c], 'DEBUG_CUT').
:- pred conc_chpt_cleanup/2 + foreignmacro([ref1(choice), ref1(choice)], [c, c], 'ConcChptCleanUp').
% Topmost choicepoint for concurrent facts
:- pred top_conc_chpt/1 + foreignfun([], ref1(choice), 'TopConcChptFun').

:- lowmacro(code_cut/1, det, [ref1(choice)], 'CODE_CUT', [c]).
:- pred code_cut/1 + prop(unfold) + prop(propargs).
code_cut(B) :-
	B2 = B,
	set_choice(B2),
	debug_cut(B2),
	conc_chpt_cleanup(~top_conc_chpt, B2),
	set_deep.

:- pred set_default_choice/1 + prop(unfold).
set_default_choice(X) :-
	( '$ct=_evalmem'(X, ~'$custommem_default_choice') ->
	    true
	; (~w).previous_choice <- ~choice_from_tagged(@X) % TODO: this should be: unbox_pointer_choice_t; and should be general enough so that an unboxed pointer to choice ptr (like default_choice) is ok...
	).

:- pred clock_overflow/0 + foreign([], det, 'clock_overflow') + prop(with_worker).
:- pred prolog_unlock_predicate/1 + foreign([ref1(int_info)], det, 'prolog_unlock_predicate') + prop(with_worker).
:- pred prolog_erase_ptr/1 + foreign([ref1(instance)], det, 'prolog_erase_ptr') + prop(with_worker).
:- pred trail_push_check/1 + foreign([tagged], det, 'trail_push_check') + prop(with_worker).

% TODO: move where instance code is defined...
% TODO: see TODO:[throwfail]
:- defmode(dynamic_neck__proceed, r).
:- pred dynamic_neck__proceed/0 + prop(contpass_true).
dynamic_neck__proceed :-
	( \+ is_deep ->
	    maybe_neck0(update_clock, xargs),
	    set_deep
	; true
	),
	X3 = ~'$evalmem'(~'$custommem_x'(3)),
	XRootArg = ~'$evalmem'(~'$custommem_x'('RootArg')),
	( @X3 == ~make_small(~'$trust_typed'(2, intval)) ->
            % nothing, unlock
            Root = ~tagged_to_root(@XRootArg),
	    prolog_unlock_predicate(Root),
	    proceed_subpr
	; @X3 == ~make_small(~'$trust_typed'(0, intval)) ->
            % erase the instance
	    Ptr = @ (~w).ins,
	    Root = ~tagged_to_root(@XRootArg),
	    prolog_erase_ptr(Ptr),
	    prolog_unlock_predicate(Root),
	    proceed_subpr
	; @X3 == ~make_small(~'$trust_typed'(3, intval)) ->
	    % nothing, no unlock
	    proceed_subpr
%	    bincont
	; @X3 == ~make_small(~'$trust_typed'(4, intval)) ->
	    T0 = ~newmut(tagged),
            % unlock and execute body
	    Root = ~tagged_to_root(@XRootArg),
	    prolog_unlock_predicate(Root),
	    X1 = ~'$evalmem'(~'$custommem_x'(1)),
	    deref(T0,@X1),
	    ( \+ @T0 == ~atom(true) ->
	        % TODO: not tested...
	        X0 = ~'$evalmem'(~'$custommem_x'(0)),
	        X2 = ~'$evalmem'(~'$custommem_x'(2)),
	        X0 <- @T0,
		X1 <- ~choice_to_tagged(@ (~w).previous_choice),
		X2 <- ~atom(dynamic),
		call(~metacall)
	    ; proceed_subpr
	    )
	; is_var(@X3) ->
	    % get ref
	    Ptr = @ (~w).ins,
	    Root = ~tagged_to_root(@XRootArg),
	    ( \+ instance_to_ref(Ptr, @X3) ->
	        '$cont'(failinscont) %'GOTOFAIL'
	    ; true
	    ),
	    prolog_unlock_predicate(Root),
	    proceed_subpr
	; '$cont'(failinscont) %'GOTOFAIL'
	).

:- pred proceed_subpr/0 + prop(subpr).
proceed_subpr :-
	% TODO: do not use $unfold
	'$unfold'(proceed).

:- pred proceed_ins/0 + prop(unfold).
proceed_ins :-
	% TODO: strange way to share definition with instruction proceed
	'$sub2call'('$insc_s'(proceed)).

:- pred instance_to_ref/2 + foreign([ref1(instance), tagged], semidet, 'instance_to_ref') + prop(with_worker).

:- pred init(f_Y).
:- iter(init, inits(f_y), no).

% Instruction to enter a predicate
:- defmode(enter, r).
:- pred enter/1 + prop(contpass_true).
:- pred enter/1 + prop(unfold).
enter(Mode) :-
	% test events
	'$cached_h'(H),
	( test_event_or_heapwarn_overflow(@H) ->
	    ( stop_this_goal(~w) ->
	        true % doing nothing in a prop(contpass_true) instruction finish the emulation loop
	    ; NewFunc = ~handle_event(~pdef),
	      % TODO: why this condition? why not a simple null?
	      ( \+ NewFunc == ~pdef ->
	          call(NewFunc)
	      ; enter__2(Mode)
	      )
	    )
	; enter__2(Mode)
	).

:- pred handle_event/2 + prop(detfun(ref1(definition))) + prop(with_worker).
:- pred v__handle_event/1 + foreign([intmach], det, 'v__handle_event') + prop(with_worker).

:- pred enter__2/1 + prop(subpr).
:- pred enter__2/1 + prop(contpass_true).
enter__2(Mode) :-
	PDef = ~'$evalmem'(~pdef),
	( Mode = undefined ->
	    pred_hook("U",PDef),
	    T3 = ~newmut(tagged),
	    emul_to_term(PDef, T3),
	    X0 = ~'$evalmem'(~'$custommem_x'(0)),
	    X0 <- @T3,
	    Undef = ~'$evalmem'(~undefined_goal),
	    call(Undef)
	; Mode = interpreted ->
	    pred_hook("I",PDef),
	    T3 = ~newmut(tagged),
	    emul_to_term(PDef, T3),
	    X0 = ~'$evalmem'(~'$custommem_x'(0)),
	    X1 = ~'$evalmem'(~'$custommem_x'(1)),
	    X3 = ~'$evalmem'(~'$custommem_x'(3)),
	    X0 <- @T3,
	    '$cache_h_load'(H),
	    loadhva(X1, H),
	    '$cache_h_store'(H),
	    X3 <- ~make_small(~'$trust_typed'(4, intval)), % 4:call body 
	    current_instance(@PDef.code.intinfo, ~'$ccons'('NO_BLOCK', blocking_type))
	; Mode = cbool ->
% TODO: see TODO:[throwfail]
	    pred_hook("C",PDef),
	    call(~'$trust_typed'(@PDef.code.proc, ho_det_with_failins(0))),
	    proceed_ins
	; Mode = cinsnp ->
	    pred_hook("C",PDef),
	    exec_cinsnp(~'$trust_typed'(@PDef.code.proc, ho_nondet(0)))
	; Mode = cvoid ->
	    pred_hook("C",PDef),
	    call(~'$trust_typed'(@PDef.code.proc, ho_det(0))),
	    proceed_ins
	; Mode = indexed ->
	    pred_hook("E",PDef),
	    X0 = ~'$evalmem'(~'$custommem_x'(0)),
	    Alt = ~newmut(ref1(try_node)),
	    incore_index_alt(@X0, Alt),
	    try_alt(Alt)
	; Mode = nonindexed ->
	    pred_hook("E",PDef),
	    Alt = ~clonemut(PDef.code.incoreinfo.varcase),
	    try_alt(Alt)
	; '$error'(enter(Mode))
	).

:- pred current_instance/2 + foreign([ref1(int_info), blocking_type], nondet, 'current_instance') + prop(with_worker).

:- pred test_event_or_heapwarn_overflow/1 + foreign([mut(tagged)], semidet, 'TestEventOrHeapWarnOverflow').
:- pred stop_this_goal/1 + foreign([ref1(worker)], semidet, 'Stop_This_Goal').

:- pred try_alt/1 + prop(contpass_true).
:- pred try_alt/1 + prop(unfold).
try_alt(Alt) :-
	(~w).previous_choice <- @ (~w).choice,
	( \+ @Alt.next == ~'$null_ref1'(try_node) ->
	    Ch = ~newmut(ref1(choice)),
	    code_choice_new(Ch, @Alt.next),
	    call(~'$trust_typed'(@Alt.code, bcp))
	; set_deep,
	  call(~'$trust_typed'(@Alt.code, bcp))
	).

% Restore state and jump to next alternative instructions
:- defmode(altcont, r).
:- pred altcont/1 + prop(contpass_true).
:- pred altcont/1 + prop(unfold).
altcont(AltKind) :-
	fail_hook,
	Choice = @ (~w).choice,
	altcont__2(Choice, undo_goal_save_frame, AltKind).

:- pred trail_dec/1 + foreignmacro([mut(mut(tagged))], [m], 'TrailDec').

:- pred reset_wake_count/0 + foreignmacro([], [], 'ResetWakeCount').

:- pred code_restore_mem_ng/1 + prop(unfold).
code_restore_mem_ng(B) :- '$subctx'(altcont__2(B, ignore_undo_goal, only_mem)).

% TODO: emit a nondet predicate instead of a nondet macro
:- lowmacro(code_next_instance/0, nondet, [], 'CODE_NEXT_INSTANCE', []).
code_next_instance :-
	altcont__2(@ (~w).choice, undo_goal_do_not_save_frame, next_instance).
:- pred next_instance_conc/0 + foreign([], nondet, 'next_instance_conc') + prop(with_worker).
:- pred next_instance_noconc/0 + foreign([], nondet, 'next_instance_noconc') + prop(with_worker).

% Untrail to restore the heap state saved in B, restore the arguments saved in B, jump to the fail cont.
% Execute UNDO when a undo goal is found (interrupts the process at untrailing) .
:- pred altcont__2/3 + prop(unfold) + prop(propargs).
altcont__2(B, Mode, AltKind) :-
	TR = ~newmut(mut(tagged)), % TODO: strange type, not mut(mut()), a subarray? array? stack?
	T1 = ~newmut(mut(tagged)), % TODO: strange type, not mut(mut()), a subarray? array? stack?
	reset_wake_count,
	TR <- @ (~g).trail_top,
	T1 <- @B.trail_top,
	untrail(Mode, B, TR, T1, AltKind).

% TODO: (continues at altcont__3)
:- pred untrail/5 + prop(subpr) + prop(propargs).
untrail(Mode, B, TR, T1, AltKind) :-
	( trail_younger(@TR, @T1) ->
	    untrail__loop(Mode, B, TR, T1, AltKind)
	; altcont__3(B, AltKind)
	).

:- pred untrail__loop/5 + prop(subpr) + prop(propargs).
untrail__loop(Mode, B, TR, T1, AltKind) :-
	Ref = ~newmut(tagged),
	trail_dec(TR),
	Ref <- @(@TR),
% TODO: allow nonreftagged; check why output of compilation to C differs!
%	( \+ reftagged(@Ref) ->
	( \+ is_var(@Ref) ->
	    % Found undo goal
	    untrail__end(undo_goal_cont, Mode, B, TR, Ref, AltKind)
	; (@Ref).ptr <- @Ref,
	  ( trail_younger(@TR, @T1) ->
	      untrail__loop(Mode, B, TR, T1, AltKind)
	  ; untrail__end(fail_cont, Mode, B, TR, Ref, AltKind)
	  )
	).
:- pred untrail__end/6 + prop(unfold) + prop(propargs).
untrail__end(Undo, Mode, B, TR, Ref, AltKind) :-
	(~g).trail_top <- @TR,
	( Undo = undo_goal_cont ->
	    ( Mode = ignore_undo_goal ->
	        % TODO: this one is incorrect! add support to call nondet goals from det contexts
	        true
	    ; undo_goal(Ref, B, Mode)
	    )
	; Undo = fail_cont ->
	    altcont__3(B, AltKind)
	; '$error'(untrail__end(Undo))
	).

:- pred altcont__3/2 + prop(subpr) + prop(propargs).
altcont__3(B, AltKind) :-
	(~g).heap_top <- @B.heap_top,
	( AltKind = only_mem ->
	    true
	; code_restore_args(B),
	  jump_fail_cont(AltKind)
	).

:- pred jump_fail_cont/1 + prop(unfold).
jump_fail_cont(AltKind) :-
	Alt = ~clonemut((~g).next_alt),
	( AltKind = next_alt ->
	    % jump to the next alternative in the current choice
	    % Pre: \+ is_deep
	    choice_patch((~w).choice, @Alt.next),
	    jump_fail_cont__2(Alt)
	; AltKind = next_instance ->
	    % jump to the next alternative (for instances)
	    choice_patch((~w).choice, @Alt.next),
	    % Take into account 'open' predicates.  (MCL)
	    XRootArg = ~'$evalmem'(~'$custommem_x'('RootArg')),
	    ( @ (~tagged_to_root(@XRootArg)).behavior_on_failure == ~'$ccons'('DYNAMIC', 'volatile+Behavior') ->
	         next_instance_noconc
	    ; next_instance_conc
	    )
	; AltKind = no_alt ->
	    % jump to the next alternative in the previous choice
            % Pre: \+ is_deep
	    set_deep,
	    Choice = @ (~w).previous_choice,
	    set_choice(Choice),
	    jump_fail_cont__2(Alt)
	; AltKind = none ->
	    jump_fail_cont__2(Alt)
	; '$error'(altcont_alt_kind(AltKind))
	).

:- pred jump_fail_cont__2/1 + prop(subpr).
jump_fail_cont__2(Alt) :-
	call(~'$trust_typed'(@Alt.code, bcp)).

% execute an undo goal
:- pred undo_goal/3 + prop(contpass_true).
:- pred undo_goal/3 + prop(unfold) + prop(propargs).
undo_goal(Goal, B, Mode) :-
	X0 = ~'$evalmem'(~'$custommem_x'(0)),
	X0 <- @Goal,
	(~g).frame <- @B.frame,
	(~g).next_insn <- @B.next_insn,
	(~g).next_alt <- @B.next_alt,
	(~w).choice <- B,
	PTF = ~newmut(ref1(frame)),
	alloc0(PTF),
	FailCode = ~'$evalmem'(~failcode),
	code_cframe(PTF, FailCode),
	% TODO: put something in the abstract state that indicates that frame is being cached?
	( Mode = undo_goal_save_frame ->
	    ~frame <- @PTF
	; true
	),
	Call1 = ~'$evalmem'(~code_call1),
	call(Call1).

:- lowmacro(bind_hva/2, det, [hvatagged,tagged], 'BindHVA', [c,c]).
bind_hva(U,V) :- bind_ref(U,V).
% TODO: why sometimes wake is not incremented?
:- lowmacro(bind_cva_nowake/2, det, [cvatagged,tagged], 'BindCVANoWake', [c,c]).
bind_cva_nowake(U,V) :- bind_nowake(U,V).
:- lowmacro(bind_cva/2, det, [cvatagged,tagged], 'BindCVA', [c,c]).
bind_cva(U,V) :- bind_ref(U,V).
:- lowmacro(bind_sva/2, det, [svatagged,tagged], 'BindSVA', [c,c]).
bind_sva(U,V) :- bind_ref(U,V).

:- pred trail_if_conditional_sva/1 + prop(unfold).
trail_if_conditional_sva(A) :-
	( trail_cond(@A) ->
	    trail_push_check0(~tagged(sva, A))
	; true
	).

:- lowmacro(trail_push_check0/1, det, [tagged], 'TrailPushCheck', [c]).
:- pred trail_push_check0/1 + prop(unfold) + prop(propargs).
trail_push_check0(X) :-
	trail_push_check(X).

% definitions required for padding
:- pred pad6(f_Q, f_Q, f_Q).
:- defmode(pad6, none).
pad6(_,_,_).
:- pred pad4(f_Q, f_Q).
:- defmode(pad4, none).
pad4(_,_).
:- pred pad2(f_Q).
:- defmode(pad2, none).
pad2(_).

%% One definition for deref
%% TODO: make it more automatic!
%% TODO: this may be suboptimal... AND THIS IS SIMPLIFIED TO SUPPORT ONLY HVA!!
% :- lowpred(myderef/1, semidet, [mut(tagged)], 'myderef').
% myderef(A) :-
% 	deref_(A).
% :- pred deref_/1 + prop(subpr).
% deref_(Reg) :-
% 	( (@Reg).tag = hva ->
% 	    T <- @ (@Reg).ptr,
% 	    ( @Reg == @T ->
% 	        true
% 	    ; Reg <- @T,
% 	      deref_(Reg)
% 	    )
% 	; true
% 	).

% ---------------------------------------------------------------------------
% Instruction set

% TODO: those should be aliases!
:- defmode(func__enter_undefined, r).
:- pred func__enter_undefined/0 + prop(contpass_true).
func__enter_undefined :- enter(undefined).
:- defmode(func__enter_interpreted, r).
:- pred func__enter_interpreted/0 + prop(contpass_true).
func__enter_interpreted :- enter(interpreted).
:- defmode(func__enter_cbool, r).
:- pred func__enter_cbool/0 + prop(contpass_true).
func__enter_cbool :- enter(cbool).
:- defmode(func__enter_cinsnp, r).
:- pred func__enter_cinsnp/0 + prop(contpass_true).
func__enter_cinsnp :- enter(cinsnp).
:- defmode(func__enter_cvoid, r).
:- pred func__enter_cvoid/0 + prop(contpass_true).
func__enter_cvoid :- enter(cvoid).
:- defmode(func__enter_indexed, r).
:- pred func__enter_indexed/0 + prop(contpass_true).
func__enter_indexed :- enter(indexed).
:- defmode(func__enter_nonindexed, r).
:- pred func__enter_nonindexed/0 + prop(contpass_true).
func__enter_nonindexed :- enter(nonindexed).

% TODO: those should be aliases!
:- defmode(restore_all_next_alt, r).
:- pred restore_all_next_alt/0 + prop(contpass_true).
restore_all_next_alt :- altcont(next_alt).
:- defmode(restore_all_no_alt, r).
:- pred restore_all_no_alt/0 + prop(contpass_true).
restore_all_no_alt :- altcont(no_alt).
:- defmode(exec_cinsnp_alt, r).
:- pred exec_cinsnp_alt/0 + prop(contpass_true).
exec_cinsnp_alt :-
	Alt = ~clonemut((~g).next_alt),
	call(~'$trust_typed'(@Alt.code, ho_nondet(0))).

% TODO: Try to compile without inserting operands in the bytecodes, AND a pass to put the operands there

% TODO: add spec info in the format...
% TODO: use the inverse of alias to process bytecode instructions... but check efficiency... and expand it to specialize!!! (target: define specialized call to var, etc.)
% TODO: this as a abstract machine fold/unfold rules
% TODO: the collapsing rules defines abstract domain for sequence of instructions, that is we should write [move(x,y), move(y,x)]...  or unify(nva,x) etc. 
% TODO: generate names automatically??
% TODO: order matters since getalias patterns are not disjoint... (make them disjoint?)
:- iset(instruction_set/0).
% note: alias/2, entry/3 asserts information in the iset context 
instruction_set :-
	( \+ '$use_opt'(varops) -> entry(inits(f_y), all) ; true ),
	( '$use_opt'(omerg), \+ '$use_opt'(varops) -> entry(inits(f_y)+inits(f_y), all) ; true ),
	( \+ '$use_opt'(varops) -> entry(cframe(a), all) ; true ),
% TODO: define with a parameter... use T.entry(_,_)
	entry(alloc_init_cframe(a), all),
	entry(init(a)+cframe(a), l(loopall)),
	entry(alloc_init_fcall(a,a), all),
	( '$use_opt'(varops) ->
	    entry(init(a)+fcall(a,a), l(until(8))),
	    entry(init(s(8))+fcall(a,a), l(justone)),
	    entry(init(s(7))+fcall(a,a), l(justone)),
	    entry(init(s(6))+fcall(a,a), l(justone)),
	    entry(init(s(5))+fcall(a,a), l(justone)),
	    entry(init(s(4))+fcall(a,a), l(justone)),
	    entry(init(s(3))+fcall(a,a), l(justone)),
	    entry(init(s(2))+fcall(a,a), l(justone)),
	    entry(init(s(1))+fcall(a,a), l(justone))
	; true
	),
	entry(fcall(a,a), all),
	( '$use_opt'(varops) ->
	    entry(zputn(a)+kall(a,a), l(until(8))),
	    entry(zputn(s(8))+kall(a,a), l(justone)),
	    entry(zputn(s(7))+kall(a,a), l(justone)),
	    entry(zputn(s(6))+kall(a,a), l(justone)),
	    entry(zputn(s(5))+kall(a,a), l(justone)),
	    entry(zputn(s(4))+kall(a,a), l(justone)),
	    entry(zputn(s(3))+kall(a,a), l(justone)),
	    entry(zputn(s(2))+kall(a,a), l(justone)),
	    entry(zputn(s(1))+kall(a,a), l(justone))
	; true
	),
	entry(kall(a,a), all),
	( '$use_opt'(varops) ->
	    entry(zputn(a)+dealloc+lastcall(a), l(until(8))),
	    entry(zputn(s(8))+dealloc+lastcall(a), l(justone)),
	    entry(zputn(s(7))+dealloc+lastcall(a), l(justone)),
	    entry(zputn(s(6))+dealloc+lastcall(a), l(justone)),
	    entry(zputn(s(5))+dealloc+lastcall(a), l(justone)),
	    entry(zputn(s(4))+dealloc+lastcall(a), l(justone)),
	    entry(zputn(s(3))+dealloc+lastcall(a), l(justone)),
	    entry(zputn(s(2))+dealloc+lastcall(a), l(justone)),
	    entry(zputn(s(1))+dealloc+lastcall(a), l(justone))
	; true
	),
	( ( '$use_opt'(omerg) ; '$use_opt'(varops) ) ->
	    entry(dealloc+lastcall(a), 1)
	; true
	),
	entry(lastcall(a), all, align(all)),
	entry(inith(f_x), all),
	entry(init2h(f_x,f_x), all),
	entry(globunsafe2(f_x,f_x), all),
	( '$use_opt'(omerg) -> entry(alloc+init2s(f_x,f_y), 1) ; true ),
	( \+ ( '$use_opt'(omerg) ; '$use_opt'(varops2) )-> entry(alloc, 1) ; true ),
	entry(init2s(f_x,f_y), all),
	( '$use_opt'(omerg) -> entry(alloc+init2s(f_x,f_y)+init2s(f_x,f_y), 1) ; true ),
	( '$use_opt'(omerg) -> entry(init2s(f_x,f_y)+init2s(f_x,f_y), all) ; true ),
	( '$use_opt'(omerg) -> entry(move(f_y,f_x)+move(f_y,f_x), all) ; true ),
	entry(move(f_y,f_x), all),
	entry(globunsafe(f_y,f_x), all),
	entry(ld_cons(f_x,a), all, align(all)),
	entry(ld_cons(f_x,f_t([])), all),
	entry(ld_blob(f_x,a), all, align(all)),
	entry(ld_str(f_x,a), all, align(all)),
	( '$use_opt'(optlst) -> entry(ld_str(f_x,f_f('.'/2)), all) ; true ),
	( '$use_opt'(omerg) -> entry(move(f_y,f_x)+globunsafe(f_y,f_x), all) ; true ),
	( '$use_opt'(omerg) -> entry(globunsafe(f_y,f_x)+move(f_y,f_x), all) ; true ),
	( '$use_opt'(omerg) -> entry(globunsafe(f_y,f_x)+globunsafe(f_y,f_x), all) ; true ),
	entry(u_val(f_x,f_x), all),
	entry(u_fval(f_x,f_y), all),
	entry(u_val(f_x,f_y), all),
	entry(u_cons(f_x,a), all),
	entry(u_blob(f_x,a), all),
	entry(u_str(f_x,a), all),
	entry(u_cons(f_x,f_t([])), all),
	( '$use_opt'(optlst) -> entry(u_str(f_x,f_f('.'/2)), all) ; true ),
	%%entry(u_str(f_x(0),f_f('.'/2)), all),
	( '$use_opt'(omerg) -> entry(u_cons(f_x,a)+neck+proceed, 1) ; true ),
	( '$use_opt'(omerg) -> entry(u_cons(f_x,f_t([]))+neck+proceed, 1) ; true ),
	entry(cutb(f_x), all),
	entry(cutb(f_x(-1)), all),
	( '$use_opt'(omerg) -> entry(cutb(f_x(-1))+proceed, 1) ; true ),
	entry(cute(f_x), all),
	entry(cute(f_x(-1)), all),
	entry(cutf(f_x), all),
	entry(cutf(f_x(-1)), all),
	entry(cutf(f_y), all),
	entry(getchoice(f_x), all),
	( '$use_opt'(omerg) -> entry(alloc+getchoice(f_y), 1) ; true ),
	entry(getchoice(f_y), all),
	entry(kontinue, all),
	entry(exit_toplevel, all),
	entry(retry_cbool(a)+proceed, 1, align0),
	entry(exec_cinsnp(a), all, align0),
	( '$use_opt'(omerg) -> entry(move(f_x,f_x)+move(f_x,f_x), all) ; true ),
	entry(move(f_x,f_x), all),
	( '$use_opt'(omerg) -> entry(alloc+move(f_x,f_y), 1) ; true ),
	entry(move(f_x,f_y), all),
	( '$use_opt'(omerg) -> entry(alloc+move(f_x,f_y)+move(f_x,f_y), 1) ; true ),
	( '$use_opt'(omerg) -> entry(move(f_x,f_y)+move(f_x,f_y), all) ; true ),
	entry(jump(a), all, align(all)),
	( '$use_opt'(iblt) ->
	    entry(blt1(f_x,f_C(d,'term_typing:var'/1)), all),
	    entry(blt1(f_x,f_C(d,'term_typing:nonvar'/1)), all),
	    entry(blt2(f_x,f_x,f_C(d,'arithmetic:>'/2)), all),
	    entry(blt2(f_x,f_x,f_C(d,'arithmetic:<'/2)), all),
	    entry(blt2(f_x,f_x,f_C(d,'arithmetic:=\\='/2)), all),
	    entry(fun1(f_x,f_x,f_C(d,'arithmetic:$+'/2),a), all),
	    entry(fun1(f_x,f_x,f_C(d,'arithmetic:$-'/2),a), all),
	    entry(fun1(f_x,f_x,f_C(d,'arithmetic:$++'/2),a), all),
	    entry(fun1(f_x,f_x,f_C(d,'arithmetic:$--'/2),a), all),
	    entry(fun2(f_x,f_x,f_x,f_C(d,'arithmetic:$+'/3),a), all),
	    entry(fun2(f_x,f_x,f_x,f_C(d,'arithmetic:$-'/3),a), all),
	    entry(funre2(f_x,f_x,f_x,f_C(d,'term_basic:arg'/3),a), all),
	    entry(blt3(f_x,f_x,f_x,f_C(d,'term_basic:functor'/3)), all)
	; true
	),
	entry(fun1(f_x,f_x,a,a), all, align(all)),
	entry(fun2(f_x,f_x,f_x,a,a), all, align(all)),
	entry(funre1(f_x,f_x,a,a), all, align(all)),
	entry(funre2(f_x,f_x,f_x,a,a), all, align(all)),
	entry(blt1(f_x,a), all, align(all)),
	entry(blt2(f_x,f_x,a), all, align(all)),
	entry(blt3(f_x,f_x,f_x,a), all, align(all)),
	% begin complexmerge
	% TODO: this one makes suite slower! 
	% TODO: check that the underlying implementation is correct,,, rcont(I,_) increments P and jumps to other code that supposes that the first part of P is the opcode, but this is not the case,,, 
	%entry(u_str(f_x,f_f('.'/2))+un_var(f_x)+un_var(f_x), 1),
	% end complexmerge
	entry(u_constraint(f_x), all),
	( '$use_opt'(varops) ->
	    entry(un_voidr(a), rw(all,l(until(4)))),
	    entry(un_voidr(c(4)), rw(loopm,l(justone))),
	    entry(un_voidr(c(3)), rw(loopm,l(justone))),
	    entry(un_voidr(c(2)), rw(loopm,l(justone))),
	    entry(un_voidr(c(1)), rw(loopm,l(justone)))
	; true
	),
	( '$use_opt'(omerg), \+ '$use_opt'(varops) -> entry(un_void+un_void, rw(all,1)) ; true ),
	( \+ '$use_opt'(varops) -> entry(un_void, all) ; true ),
	entry(un_var(f_x), all),
	entry(un_val(f_x), rw(e(un_lval(f_x), 0), all)), % TODO: automatize?
	entry(un_lval(f_x), all),
	( '$use_opt'(omerg) -> entry(alloc+un_var(f_y), 1) ; true ),
	entry(un_var(f_y), all),
	entry(un_fval(f_y), all),
	entry(un_val(f_y), rw(e(un_lval(f_y), 0), all)),
	entry(un_lval(f_y), all),
	entry(un_cons(a), all, align(rw(0, all))),
	entry(un_blob(a), all),
	entry(un_str(a), all, align(rw(0, all))),
	entry(un_cons(f_t([])), all),
	( '$use_opt'(optlst) -> entry(un_str(f_f('.'/2)), all) ; true ),
	( '$use_opt'(omerg) ->
	    entry(un_cons(a)+neck+proceed, 1, align(rw(0, 1))),
	    entry(un_cons(f_t([]))+neck+proceed, 1),
 	    ( '$use_opt'(varops2) ->
 	        entry(un_voidr(a)+un_var(f_x), all),
 		entry(alloc+un_voidr(a)+un_var(f_y), 1),
 		entry(un_voidr(a)+un_var(f_y), all),
 		entry(un_voidr(a)+un_val(f_x), rw(e(un_voidr(a)+un_lval(f_x), 0), all)),
 		entry(un_voidr(a)+un_lval(f_x), all),
 		entry(un_voidr(a)+un_fval(f_y), all),
 		entry(un_voidr(a)+un_val(f_y), rw(e(un_voidr(a)+un_lval(f_y), 0), all)),
 		entry(un_voidr(a)+un_lval(f_y), all),
 		entry(un_var(f_x)+un_voidr(a), all)
 	    ; true
 	    ),
	    entry(un_var(f_x)+un_var(f_x), all),
	    entry(alloc+un_var(f_x)+un_var(f_y), 1),
	    entry(un_var(f_x)+un_var(f_y), all),
	    entry(un_var(f_x)+un_val(f_x), rw(e(un_var(f_x)+un_lval(f_x), 0), all)),
	    entry(un_var(f_x)+un_lval(f_x), all),
	    entry(un_var(f_x)+un_fval(f_y), all),
	    entry(un_var(f_x)+un_val(f_y), rw(e(un_var(f_x)+un_lval(f_y), 0), all)),
	    entry(un_var(f_x)+un_lval(f_y), all),
	    ( '$use_opt'(varops2) ->
	        entry(alloc+un_var(f_y)+un_voidr(a), 1),
		entry(un_var(f_y)+un_voidr(a), all)
	    ; true
	    ),
	    entry(alloc+un_var(f_y)+un_var(f_x), 1),
	    entry(un_var(f_y)+un_var(f_x), all),
	    entry(alloc+un_var(f_y)+un_var(f_y), 1),
	    entry(un_var(f_y)+un_var(f_y), all),
	    entry(alloc+un_var(f_y)+un_val(f_x), rw(e(alloc+un_var(f_y)+un_lval(f_x), 0),1)),
	    entry(alloc+un_var(f_y)+un_lval(f_x), 1),
	    entry(un_var(f_y)+un_val(f_x), rw(e(un_var(f_y)+un_lval(f_x), 0), all)),
	    entry(un_var(f_y)+un_lval(f_x), all),
	    entry(alloc+un_var(f_y)+un_val(f_y), rw(e(alloc+un_var(f_y)+un_lval(f_y), 0),1)),
	    entry(alloc+un_var(f_y)+un_lval(f_y), 1),
	    entry(un_var(f_y)+un_val(f_y), rw(e(un_var(f_y)+un_lval(f_y), 0), all)),
	    entry(un_var(f_y)+un_lval(f_y), all),
	    ( '$use_opt'(varops2) -> entry(un_fval(f_y)+un_voidr(a), all) ; true ),
	    entry(un_fval(f_y)+un_var(f_x), all),
	    entry(un_fval(f_y)+un_fval(f_y), all),
	    entry(un_fval(f_y)+un_val(f_x), rw(e(un_fval(f_y)+un_lval(f_x), 0), all)),
	    entry(un_fval(f_y)+un_lval(f_x), all),
	    entry(un_fval(f_y)+un_val(f_y), rw(e(un_fval(f_y)+un_lval(f_y), 0), all)),
	    entry(un_fval(f_y)+un_lval(f_y), all),
	    ( '$use_opt'(varops2) -> entry(un_val(f_x)+un_voidr(a), rw(e(un_lval(f_x)+un_voidr(a), 0), all)) ; true ),
	    ( '$use_opt'(varops2) -> entry(un_lval(f_x)+un_voidr(a), rw(e(un_lval_un_voidr(f_x,a), all), all)) ; true ),
	    entry(un_val(f_x)+un_var(f_x), rw(e(un_lval(f_x)+un_var(f_x), 0), all)),
	    entry(un_lval(f_x)+un_var(f_x), rw(e(un_lval_un_var(f_x,f_x), all), all)),
	    entry(alloc+un_val(f_x)+un_var(f_y), rw(e(alloc+un_lval(f_x)+un_var(f_y), 0),1)),
	    entry(alloc+un_lval(f_x)+un_var(f_y), 1),
	    entry(un_val(f_x)+un_var(f_y), rw(e(un_lval(f_x)+un_var(f_y), 0), all)),
	    entry(un_lval(f_x)+un_var(f_y), all),
	    entry(un_val(f_x)+un_val(f_x), rw(e(un_val(f_x)+un_lval(f_x), 0), all)),
	    entry(un_val(f_x)+un_lval(f_x), rw(e(un_lval(f_x)+un_val(f_x), 0), all)),
	    entry(un_lval(f_x)+un_val(f_x), rw(e(un_lval(f_x)+un_lval(f_x), 0), all)),
	    entry(un_lval(f_x)+un_lval(f_x), all),
	    entry(un_val(f_x)+un_fval(f_y), rw(e(un_lval(f_x)+un_fval(f_y), 0), all)),
	    entry(un_lval(f_x)+un_fval(f_y), all),
	    entry(un_val(f_x)+un_val(f_y), rw(e(un_val(f_x)+un_lval(f_y), 0), all)),
	    entry(un_val(f_x)+un_lval(f_y), rw(e(un_lval(f_x)+un_val(f_y), 0), all)),
	    entry(un_lval(f_x)+un_val(f_y), rw(e(un_lval(f_x)+un_lval(f_y), 0), all)),
	    entry(un_lval(f_x)+un_lval(f_y), all),
	    ( '$use_opt'(varops2) -> entry(un_val(f_y)+un_voidr(a), rw(e(un_lval(f_y)+un_voidr(a), 0), all)) ; true ),
	    ( '$use_opt'(varops2) -> entry(un_lval(f_y)+un_voidr(a), rw(e(un_lval_un_voidr(f_y,a), all), all)) ; true ),
	    entry(un_val(f_y)+un_var(f_x), rw(e(un_lval(f_y)+un_var(f_x), 0), all)),
	    entry(un_lval(f_y)+un_var(f_x), rw(e(un_lval_un_var(f_y,f_x), all), all)),
	    entry(un_val(f_y)+un_var(f_y), rw(e(un_lval(f_y)+un_var(f_y), 0), all)),
	    entry(un_lval(f_y)+un_var(f_y), all),
	    entry(un_val(f_y)+un_fval(f_y), rw(e(un_lval(f_y)+un_fval(f_y), 0), all)),
	    entry(un_lval(f_y)+un_fval(f_y), all),
	    entry(un_val(f_y)+un_val(f_x), rw(e(un_val(f_y)+un_lval(f_x), 0), all)),
	    entry(un_val(f_y)+un_lval(f_x), rw(e(un_lval(f_y)+un_val(f_x), 0), all)),
	    entry(un_lval(f_y)+un_val(f_x), rw(e(un_lval(f_y)+un_lval(f_x), 0), all)),
	    entry(un_lval(f_y)+un_lval(f_x), all),
	    entry(un_val(f_y)+un_val(f_y), rw(e(un_val(f_y)+un_lval(f_y), 0), all)),
	    entry(un_val(f_y)+un_lval(f_y), rw(e(un_lval(f_y)+un_val(f_y), 0), all)),
	    entry(un_lval(f_y)+un_val(f_y), rw(e(un_lval(f_y)+un_lval(f_y), 0), all)),
	    entry(un_lval(f_y)+un_lval(f_y), all)
	; true
	),

	entry(failins, all),
	entry(heapmargin_call(a,a), all),
	entry(neck, all),
	( '$use_opt'(omerg) -> entry(neck+proceed, e(neck0+proceed, all)) ; true ),
	( '$use_opt'(omerg) -> entry(dealloc+proceed, all) ; true ),
	( \+ '$use_opt'(omerg) -> entry(dealloc, all) ; true ),
	entry(proceed, all),
	entry(restore_all_next_alt, all),
	entry(restore_all_no_alt, all),
	entry(exec_cinsnp_alt, all),
	entry(func__enter_undefined, all),
	entry(func__enter_interpreted, all),
	entry(func__enter_cbool, all),
	entry(func__enter_cinsnp, all),
	entry(func__enter_cvoid, all),
	entry(func__enter_indexed, all),
	entry(func__enter_nonindexed, all),
	entry(dynamic_neck__proceed, all),

	illop(illop),

        % exported instructions
	exported_ins(dynamic_neck__proceed, dynamic_neck__proceed),
	exported_ins(restore_all_next_alt, restore_all_next_alt),
	exported_ins(restore_all_no_alt, restore_all_no_alt),
	exported_ins(exec_cinsnp_alt, exec_cinsnp_alt),
	exported_ins(func__enter_undefined, func__enter_undefined),
	exported_ins(func__enter_interpreted, func__enter_interpreted),
	exported_ins(func__enter_cbool, func__enter_cbool),
	exported_ins(func__enter_cinsnp, func__enter_cinsnp),
	exported_ins(func__enter_cvoid, func__enter_cvoid),
	exported_ins(func__enter_indexed, func__enter_indexed),
	exported_ins(func__enter_nonindexed, func__enter_nonindexed),
	exported_ins(kontinue, kontinue),
	exported_ins(exit_toplevel, exit_toplevel),
	exported_ins(retry_cbool(a)+proceed, retry_cbool__proceed, a0),
	exported_ins(proceed, proceed),
	exported_ins(exec_cinsnp(a), exec_cinsnp, a0),
        % needed for cterm
	exported_ins(u_blob(f_x,a), ux_blob),
	exported_ins(u_val(f_x,f_x), ux_xval),
	exported_ins(u_cons(f_x,a), ux_cons),
	exported_ins(u_str(f_x,a), ux_str),
	exported_ins(u_cons(f_x,f_t([])), ux_nil),
	exported_ins(u_str(f_x,f_f('.'/2)), ux_lst),
	exported_ins(move(f_x,f_x), movexx),
	( '$use_opt'(varops) ->
	    exported_ins(un_voidr(c(1)), un_voidr1)
	; exported_ins(un_void, un_void)
	),
	exported_ins(un_var(f_x), un_xvar),
	exported_ins(un_val(f_x), un_xval),
	exported_ins(un_cons(a), un_cons),
	exported_ins(un_blob(a), un_blob),
	exported_ins(un_str(a), un_str),
	exported_ins(un_cons(f_t([])), un_nil),
	exported_ins(un_str(f_f('.'/2)), un_lst),
        % compile_term_aux
	exported_ins(heapmargin_call(a,a), heapmargin_call),
	exported_ins(u_constraint(f_x), ux_constraint),
        % ciao_initcode and init_some_bytecode
	exported_ins(fcall(a,a), fcall),
	exported_ins(kall(a,a), call, a0),
	exported_ins(failins, failins),
	exported_ins(lastcall(a), lastcall).

% domain definition for mode
:- type(mode/1) + enum.
mode(T) :- T = (r|w).
:- enum_encode(mode/1, [
	(w, 0),
	(r, 1)]).
:- type(mode_r/1) + subenum(mode).
mode_r(T) :- T = r.
:- type(mode_w/1) + subenum(mode).
mode_w(T) :- T = w.

% TODO: define as attributes of an object
% TODO: do not use gvar
% TODO: obtain var declarations in $emu from here
:- class '$emu_globals' {
    :- struct.
    % TODO: document, 'bcp_jump' is a implicit variable where the emulator jump-to-bytecode is stored
    :- attr bcp_jump :: '$gvar'('bcp_jump', mut(char)).
    :- attr bcp_jump :: '$prop'(do_not_materialize).
    %
    :- attr cached_r_h :: '$gvar'('H', mut(mut(tagged))). % TODO: strange type, not mut(mut()), a subarray? array? stack?
    :- attr r_s :: '$gvar'('S', mut(mut(tagged))). % TODO: strange type, not mut(mut()), a subarray? array? stack?
    :- attr e :: '$gvar'('E', mut(ref1(frame))).
    :- attr g :: '$gvar'('G', ref1(choice)).
    :- attr w :: '$gvar'(w, ref1(worker)).
    :- if('$use_opt'(specmode)).
        :- attr mode :: '$gvar'(rmode, mut(mode)).
        :- attr mode :: '$prop'(do_not_materialize). % TODO: incomplete!
    :- else.
        % TODO: experimental... shared with S
        :- attr mode :: '$gvar'(rmode, mut(mode)).
    :- endif.
    :- if(('$use_opt'(computed_goto) ; '$use_opt'(threaded_bytecode_rel16) ; '$use_opt'(threaded_bytecode))).
    :- else.
        :- attr opcode :: '$gvar'(opcode, mut(ftype_o)).
    :- endif.
    :- attr pdef :: '$gvar'('PDEF', ref1(definition)).
    :- attr p :: '$gvar'('P', mut(bcp)).
    :- attr def_clock :: '$gvar'(def_clock, mut(instance_clock)).
    :- attr use_clock :: '$gvar'(use_clock, mut(instance_clock)).
    :- attr undefined_goal :: '$gvar'(address_undefined_goal, ref1(definition)).
    :- attr metacall :: '$gvar'(address_metacall, ref1(definition)).
    :- attr failcont :: '$gvar'('FAIL_INSNP', p0emu).
    :- attr frame :: '$gvar'(frame, mut(ref1(frame))).
    %
    :- attr code_call1 :: '$gvar'('code_call1', ho_nondet(0)).
    :- attr failcode :: '$gvar'('failcode', bcp).
}.

:- pred '$emu'/1 + lowentry(det, [bcp], 'wam__2') + prop(with_worker).
'$emu'(StartP) :-
	% program counter
        ( '$use_opt'(p_in_reg) ->
	    true
	; P = ~newmut(bcp),
	  '$mutname'(P, 'p')
	),
        ~p <- ~'$ccons'('NULL', bcp),
        Frame = ~newmut(ref1(frame)),
	'$mutname'(Frame, 'frame'),
        Frame <- ~'$null_ref1'(frame),
	% H in write mode, S in read mode
        Pt2 = ~newmut(mut(tagged)), % TODO: strange type, not mut(mut()), a subarray? array? stack?
	'$mutname'(Pt2, 'pt2'),
        Pt2 <- ~'$ccons'('NULL', mut(tagged)),
	% TODO: move to a structure and do a local structure 'unfolding' in the predicate
	% TODO: put here definition of ~mode
	( '$use_opt'(specmode) ->
	    true
	; RMode = ~newmut(bool),
	  '$mutname'(RMode, 'rmode'),
	  RMode <- true
	),
	% TODO: move to a structure and do a local structure 'unfolding' in the predicate
	( ( '$use_opt'(computed_goto) ; '$use_opt'(threaded_bytecode_rel16) ; '$use_opt'(threaded_bytecode) ) ->
	    true
	; Opcode = ~newmut(ftype_o),
	  '$mutname'(Opcode, 'opcode')
	),
	% TODO: find a way to remove '$emuctx'
	'$subctx4'((def_bcp_jump(BcpJump), ~bcp_jump <- BcpJump,
                   '$emuctx'('$catch_failins'(call(StartP), '$sub2call'('$insc'(failins)))))),
	end_emu(~p).

% internally called from ptoc__impcomp:reduce_to_bcp_jump(AR, AType, Xs2, Tbl, JumpGoal)
:- pred bcp_cont/2 + prop(unfold) + prop(propargs).
bcp_cont(BcpMode, Y) :-
	BcpJump = @(~bcp_jump),
	'$unfold'(BcpJump(BcpMode, Y)).

% definition for jump to bcp
:- pred def_bcp_jump/1 + prop(unfold).
def_bcp_jump(BcpJump) :-
	def_bcp_jump__2(BcpJump0),
	BcpJump = ~'$predabs'([propargs], [BcpMode, Y], [BcpJump0], def_bcp_jump__3(BcpMode, Y, BcpJump0)).

:- pred def_bcp_jump__3/3 + prop(unfold) + prop(propargs).
def_bcp_jump__3(BcpMode, Y, BcpJump0) :- BcpMode = no_set_p, !,
	% for code that do not read the value of P (e.g. fail instruction, because it will overwrite the value of P)
	'$unfold'(BcpJump0(Y)).
def_bcp_jump__3(BcpMode, Y, BcpJump0) :- BcpMode = set_p, !,
	% set P and jump
	~p <- Y,
	'$unfold'(BcpJump0(@(~p))).
def_bcp_jump__3(BcpMode, Y, BcpJump0) :- BcpMode = recover_frame__set_p, !,
	% set P
	~p <- Y,
	% recover frame (cached registers)
	~frame <- @ (~g).frame,
	% jump to P
	'$unfold'(BcpJump0(@(~p))).
def_bcp_jump__3(BcpMode, _Y, _BcpJump0) :-
	'$error'(bcp_jump_bad_mode(BcpMode)).

% basic instruction switch (using computed goto or a switch)
:- pred def_bcp_jump__1/1 + prop(unfold).
% TODO: optimize compilation by partial evaluation of ground propositions (e.g. '$use_opt'(X) with ground(X))
% TODO: but the previous point requires a declaration that says that '$use_opt' is not dynamic and its definition cannot change at runtime...
def_bcp_jump__1(BcpJump) :-
	( '$use_opt'(computed_goto) ->
	    '$decl_ins_pa_array'(static, IL),
	    Sw1 = ~'$predabs'([propargs], [Op], [IL], '$call_pa_array'(IL, Op))
	; '$use_opt'(threaded_bytecode_rel16) ->
	    ( '$use_opt'(specmode) ->
	        '$error'(['specmode must be disabled when threaded_bytecode_rel16 is active'])
	    ; true
	    ),
	    % TODO: improve: declared with a fixed asm label so that it can be accessed from outside the C function
	    '$decl_ins_pa_array'(staticasm, IL),
	    % note: in threaded bytecode rel16, each opcode is the offset from first label in the emulator function to the instruction label
	    %   A[~'$elem_inv'(A, B)] is cancelced out as B in the compilation process
	    % opcodes are encoded as relative offsets to the function address
	    Sw1 = ~'$predabs'([propargs], [Op], [IL], '$call_pa_array'(IL, ~'$elem_inv'(IL, ~'$trust_typed'(~'$mut_move'(~'$ccons'('((char *)wam__2)', mut(char)), ~'$trust_typed'(Op, intmach)), clabel))))
	; '$use_opt'(threaded_bytecode) ->
	    ( '$use_opt'(specmode) ->
	        '$error'(['specmode must be disabled when threaded_bytecode is active'])
	    ; true
	    ),
	    % TODO: improve: declared with a fixed asm label so that it can be accessed from outside the C function
	    '$decl_ins_pa_array'(staticasm, IL),
	    % note: in threaded bytecode rel16, each opcode is the offset from first label in the emulator function to the instruction label
	    %   A[~'$elem_inv'(A, B)] is cancelced out as B in the compilation process
	    % opcodes are encoded as relative offsets to the function address
	    Sw1 = ~'$predabs'([propargs], [Op], [IL], '$call_pa_array'(IL, ~'$elem_inv'(IL, ~'$trust_typed'(~'$cast'(Op, mut(char)), clabel))))
	; Sw = ~'$predabs'([propargs], [Z], [], '$insswitch'(Z)),
	  Sw1 = ~'$predabs'([propargs], [Op], [Sw], (~opcode <- Op, '$sub4call'(Sw(@(~opcode)))))
	),
	BcpJump = ~'$predabs'([propargs], [Y], [Sw1], '$unfold'(Sw1(~'$op_at_p'(Y)))).

% instruction switch with optional specmode
:- pred def_bcp_jump__2/1 + prop(unfold).
def_bcp_jump__2(BcpJump) :-
	( '$use_opt'(specmode) ->
	    def_bcp_jump__1(BcpJumpR),
	    def_bcp_jump__1(BcpJumpW),
	    BcpJump = ~'$predabs'([propargs], [Y], [BcpJumpR, BcpJumpW],
	      ( '$readmode' ->
		  '$unfold'(BcpJumpR(Y))
	      ; '$unfold'(BcpJumpW(Y))
	      ))
	; def_bcp_jump__1(BcpJump)
	).

% TODO: this is not 'det' (throw exception!)
:- pred serious_fault/1 + foreign([cstring], det, 'SERIOUS_FAULT').
:- pred illop/0 + prop(unfold).
illop :-
	serious_fault(~'$string'("unimplemented WAM instruction")).
:- pred end_emu/1 + prop(unfold) + prop(propargs).
end_emu(P) :-
	(~w).insn <- @P.

:- pred '$set_mode'/1 + prop(unfold).
'$set_mode'(M) :- '$use_opt'(specmode), !,
	( '$use_opt'(cached_h) ->
            % Update cached registers
            % (Read versions: with H in the worker)
            % (Write versions: with H in a local variable)
	    ( @(~mode) = r -> 
	        ( M = w -> ~cached_r_h <- @ (~g).heap_top ; true )
	    ; ( M = r -> (~g).heap_top <- @(~cached_r_h) ; true )
	    )
	; true
	),
	% Change mode
	~mode <- M.
'$set_mode'(_).

:- pred '$cached_h'/1 + prop(unfold).
'$cached_h'(H) :-
	( '$use_opt'(cached_h) ->
	    ( @(~mode) = r ->
	        H = ~'$evalmem'((~g).heap_top)
	    ; H = ~'$evalmem'(~cached_r_h)
	    )
	; H = ~'$evalmem'((~g).heap_top)
	).

:- pred '$cache_h_load'/1 + prop(unfold).
'$cache_h_load'(H) :- '$use_opt'(specmode), !, '$cached_h'(H).
%'$cache_h_load'(H) :- '$cached_h'(H).
'$cache_h_load'(H) :-
	'$cached_h'(H0),
	H = ~newmut(mut(tagged)), % TODO: strange type, not mut(mut()), a subarray? array? stack?
	H <- @H0.
:- pred '$cache_h_store'/1 + prop(unfold).
'$cache_h_store'(H) :- '$use_opt'(specmode), !.
%'$cache_h_store'(_).
'$cache_h_store'(H) :- '$cached_h'(H0), H0 <- @H.

:- pred '$set_mode__nospecmode'/1 + prop(unfold).
'$set_mode__nospecmode'(M) :- '$use_opt'(specmode), !.
'$set_mode__nospecmode'(M) :-
	~mode <- M.
% TODO: not working well... mode analysis invalidated
%	( M = w ->
%	    S = ~'$evalmem'(~r_s),
%	    S <- ~'$ccons'('NULL', mut(tagged))
%	; true
%	).

:- pred '$readmode'/0 + prop(unfold).
'$readmode' :- '$use_opt'(specmode), !,
	@(~mode) = r.
'$readmode' :-
	@(~mode) = r.
% TODO: not working well... mode analysis invalidated
%	S = ~'$evalmem'(~r_s),
%	@S \== ~'$ccons'('NULL', mut(tagged)).

% ===========================================================================
% Definitions for compilation to C
% TODO: merge notation and code with previous definitions

% definitions of global variables
% TODO: define as cached worker registers?
% (variables with local scope to the basic block that must be declared
% only when necessary)
:- pred globvardefs/0 + prop(unfold).
globvardefs :-
%	T.e = ~'$gvar'('E', mut(mut(frame))),
	'$add_autov'(frame, mut(ref1(frame))),
	'$add_autov'('a_s', mut(mut(tagged))). % TODO: strange type, not mut(mut()), a subarray? array? stack?

% TODO: wrong name
% TODO: the nva x nva case is forbidden, can I use a general case?
% pre: variables are dereferenced
% note: If I disable explicit derefs for bind in ptoc__lowcomp and enable derefvar here, performance decreases (maybe it improves if some deref analysis is moved here...)
:- pred v__bind/2 + prop(unfold) + prop(propargs).
v__bind(A, B) :-
	% TODO: a kludge... arguments of the bind codedef are tagged but they are interpreted as mut(tagged)
	A2 = ~initmut(tagged, A),
	B2 = ~initmut(tagged, B),
	v__bind0(A2, B2).
:- pred v__check_events/1 + prop(unfold) + prop(propargs).
% TODO: change arity by some sort of live register array?
% TODO: merge with enter/2
v__check_events(Args) :-
	'$cached_h'(H),
	( test_event_or_heapwarn_overflow(@H) ->
	    % TODO: incomplete!! (see enter/2)
	    v__handle_event(~'$vargxarity'(Args))
	; true
	).
:- pred v__ensure_heap/3 + prop(unfold).
% TODO: change arity by some sort of live register array?
v__ensure_heap(_, Size, Args) :-
	% TODO: use ChoiceState?
        '$cached_h'(H),
	test_heap_overflow(@H, Size, ~'$vargxarity'(Args)).
% TODO: do not ignore! environment trimming is very important for garbage collection, some simple examples like exp does not end without it
:- pred v__trim_frame/1 + prop(unfold) + prop(propargs).
v__trim_frame(FrameSize) :-
	(~g).next_insn <- ~'$vargnullsuccesscont'(FrameSize).
:- pred v__recover_frame/0 + prop(unfold).
v__recover_frame :- ~'$vargv'(frame) <- @ (~g).frame. % TODO: this predicate is called more than required, optimize
:- pred v__invalidate_local_top/0 + prop(unfold).
v__invalidate_local_top :- invalidate_local_top. % TODO: this predicate is called more than required, optimize
:- pred v__validate_local_top/0 + prop(unfold).
v__validate_local_top :- validate_local_top(@(~'$vargv'(frame))). % TODO: this predicate is called more than required, optimize
:- pred v__set_liveinfo/1 + prop(unfold) + prop(propargs).
v__set_liveinfo(X) :-
	(~w).liveinfo <- X.
:- pred v__set_success_cont/1 + prop(unfold) + prop(propargs).
v__set_success_cont(Successcont) :-
	(~g).next_insn <- Successcont.
:- pred v__set_fail_cont/1 + prop(unfold) + prop(propargs).
v__set_fail_cont(Failcont) :-
	choice_patch((~w).choice, Failcont).
:- pred v__update_default_choice/0 + prop(unfold).
v__update_default_choice :-
	(~w).previous_choice <- @ (~w).choice.
:- pred v__push_choice/1 + prop(unfold) + prop(propargs).
v__push_choice(Args) :-
        % TODO: pending alt, previously I put here only the arity... livearity is too simple!!!???
	code_choice_new((~w).choice, ~'$vargnullfailcont'(Args)).
:- pred v__restore_all/1 + prop(unfold) + prop(propargs).
v__restore_all(Args) :-
	v__restore_mem,
	v__restore_args(Args).
:- pred v__restore_mem/0 + prop(unfold).
v__restore_mem :- % TODO: consider pending callundogoals!
	Choice = @ (~w).choice,
	code_restore_mem_ng(Choice).
:- pred v__restore_args/1 + prop(unfold) + prop(propargs).
v__restore_args(Args) :- % TODO: args cannot be ignored... use ~liveset_arity(Args)?
	Choice = @ (~w).choice,
	code_restore_args0(Choice, Args).
:- pred v__neck/1 + prop(unfold) + prop(propargs).
v__neck(Args) :- % TODO: args cannot be ignored...
	Choice = @ (~w).choice,
	code_neck_try0(Choice, Args),
	set_deep.
:- pred v__maybe_neck/1 + prop(unfold) + prop(propargs).
v__maybe_neck(Args) :- % TODO: args cannot be ignored...
	maybe_neck0(no_clock, Args),
	set_deep.
:- pred v__alloc/0 + prop(unfold).
v__alloc :- alloc0(~'$vargv'(frame)).
:- pred v__cframe/1 + prop(unfold) + prop(propargs).
v__cframe(FrameSize) :- code_cframe(~'$vargv'(frame), ~'$vargnullsuccesscont'(FrameSize)).
:- pred v__dealloc/0 + prop(unfold).
v__dealloc :- deallocate(@(~'$vargv'(frame))).
:- pred v__globalize_if_unsafe/2 + prop(unfold) + prop(propargs).
% TODO: merge with globunsafe_common (but remember that this version does not dereference)
v__globalize_if_unsafe(X, Y) :-
	T0 = ~initmut(tagged, X),
	globunsafe_common__2((~'$vargv'(frame)), T0),
	Y <- @T0.
:- pred v__globalize_to_arg/2 + prop(unfold) + prop(propargs).
v__globalize_to_arg(B, A) :-
	T1 = ~initmut(tagged, B),
	unify_local_value__2(T1, A),
	A <- @T1.
:- pred v__deref/2 + prop(unfold) + prop(propargs).
v__deref(X, Y) :- deref(Y, X). 
:- pred v__inits/1 + prop(unfold).
v__inits(A) :- inits(A).
:- pred v__inith/1 + prop(unfold).
v__inith(A) :- inith(A).
:- pred v__consh/1 + prop(unfold).
v__consh(A) :- A <- ~tagged(hva, A).
:- pred v__load/3 + prop(unfold) + prop(propargs).
v__load(A, MaybeArgs, B) :-
	( '$vcccteq'(MaybeArgs, just(_)) ->
	    ( '$vcccteq'(B, list) ->
	        '$cache_h_load'(H),
	        A <- ~tagged(lst, @H),
		% TODO: hardwired args
		~'$vargv'('a_s') <- @H,
		v__inch0(H, 2),
		'$cache_h_store'(H)
	    ; '$vcccteq'(B, str(_)) ->
	        '$cache_h_load'(H),
	        A <- ~tagged(str, @H),
		% TODO: hardwired args
		~'$vargv'('a_s') <- ~'$mut_move'(@H, 1),
		~v__n('a_s', 0) <- B,
		v__inch0(H, 1 + ~'$vargtype_arity'(B)),
		'$cache_h_store'(H)
	    ; '$error'(['bad type in load ', B])
	    )
	; '$vcccteq'(B, float(_)) ->
	    A <- ~make_blob(B)
	; '$vcccteq'(B, bignum(_)) ->
	    A <- ~make_blob(B)
	; A <- B
	).
:- pred v__read/3 + prop(unfold) + prop(propargs).
v__read(X, Type, As) :- '$vcccteq'(Type, list), !,
	As <- (~'$trust_typed'(X, lsttagged)).ptr.
v__read(X, Type, As) :- '$vcccteq'(Type, str(_)), !,
	As <- ~'$mut_move'((~'$trust_typed'(X, strtagged)).ptr, 1).
v__read(X, Type, As) :-
	'$error'(['bad type in read ', Type]).
:- pred v__move/2 + prop(unfold) + prop(propargs).
v__move(X, Y) :- Y <- X.
:- pred v__inch/1 + prop(unfold).
v__inch(Y) :-
	'$cache_h_load'(H),
	v__inch0(H, Y),
	'$cache_h_store'(H).
:- pred v__inch0/2 + prop(unfold) + prop(propargs).
v__inch0(H, Y) :-
	H <- ~'$mut_move'(@H, Y).
:- pred v__erroneous/1 + prop(unfold) + prop(propargs).
v__erroneous(X) :- X == ~'$ccons'('ERRORTAG', tagged).
:- pred v__isnonvar/1 + prop(unfold) + prop(propargs).
v__isnonvar(A) :- \+ is_var(A).
:- pred v__callpad/0 + prop(unfold).
v__callpad := ~call_pad.
:- pred v__default_choice/1 + prop(unfold).
v__default_choice := ~'$trust_typed'(~'$vargaddress'(~choice_to_tagged(@ (~w).previous_choice)), mut(tagged)).
:- pred v__x/2 + prop(unfold).
v__x(X) := ~'$trust_typed'(~'$vargaddress'(~'X'(X)), mut(tagged)).
:- pred v__y/2 + prop(unfold).
v__y(X) := ~'$trust_typed'(~'$vargaddress'(~'$vargelement'((~'$vargv'(frame)).x, X)), mut(tagged)).
:- pred v__n/3 + prop(unfold).
v__n(As,I) := ~'$trust_typed'(~'$vargaddress'(~'$vargelement'(@(~'$vargv'(As)), I - 1)), mut(tagged)).
:- pred v__inline_choice/1 + prop(unfold).
v__inline_choice := @ (~w).choice.
:- pred v__inline_equal/2 + prop(unfold) + prop(propargs).
v__inline_equal(A, B) :- B == A.
:- pred v__inline_test/2 + prop(unfold) + prop(propargs).
v__inline_test(A, B) :- '$vcccteq'(B, list), !,
	A.tag = lst.
v__inline_test(A, B) :- '$vcccteq'(B, str(_)), !,
	A.tag = str, ~tagged_to_head_functor(A) == B.
v__inline_test(A, B) :- '$vcccteq'(B, float(_)), !,
	blob__check(A, B).
v__inline_test(A, B) :- '$vcccteq'(B, bignum(_)), !,
	blob__check(A, B).
v__inline_test(A, B) :- A == B.
:- pred v__blob__check/2 + prop(unfold).
v__blob__check(T1, L) :- blob__check(T1, L).
:- pred v__inline_trail_if_conditional/1 + prop(unfold).
v__inline_trail_if_conditional(A) :- trail_if_conditional_sva(A).
:- pred v__inline_cut/1 + prop(unfold).
v__inline_cut(A) :- cutf(A).

