% ===========================================================================
% = Testing module information
% ===========================================================================

%! @title Test File for Lightweight Markup Language
%
%  @author Jose F. Morales, Manuel Hermenegildo
%
%  @usage  At your own risk.
%
%  @module
%
%    This file tests the elements of our lightweight markup language

% ===========================================================================
% = Testing sections.
% ===========================================================================

%! * First Section
%    The text for the first section
%
%  ** A subsection
%     The text for the subsection
%
%  *** One subsubsection
%      The text for the subsubsection

% ===========================================================================
% = Some general markup 
% ===========================================================================

% * Changing font style
%
% Text can be /emphasized/ or *bold*.
%
% TODO: Support +striked+ text like in org? (it looks cool but we will
%   almost never use it)
%   
%! * Lists and enumerations
%
%  This is a simple list:
%   - Elem a
%   - Elem b
%   - Elem c
%
%  This is another simple list:
%  - Elem a
%  - Elem b
%  - Elem c
%
%  This is another simple list:
%
%   - Elem a
%
%   - Elem b
%
%   - Elem c
%
%  This is list of paragraphs:
%
%   - Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed
%     do eiusmod tempor incididunt ut labore et dolore magna
%     aliqua.
%
%   - Ut enim ad minim veniam, quis nostrud exercitation
%     ullamco laboris nisi ut aliquip ex ea commodo consequat.
%
%   - Duis aute irure dolor in reprehenderit in voluptate velit esse
%     cillum dolore eu fugiat nulla pariatur.
%
%  Nested lists:
%   - Elem a
%   - Elem b
%     - Elem ba
%     - Elem bb
%   - Elem c
%
%  Explicit enumeration lists (numbered lists):
%   1. Elem a
%   2. Elem b
%   3. Elem c
%
%  Automatic enumeration lists (picked from Doxygen, add numbers
%  automatically):
%   -# Elem a
%   -# Elem b
%   -# Elem c
%
%  Description lists:
%   - tree :: a woody perennial plant
%   - shrub :: a woody plant that is smaller than a tree
%
% * Links
%
%   A link to [[http://ciaohome.org][Ciao]] hiding its URL.
%   A link to [[http://ciaohome.org]] shows its URL.
%
%! * Anchors, labels, references, bibliographical citations
%
%  *Note*: Due to limitations of @tt{texinfo}, LPdoc does not have labels.
% TODO: Add them.
%
% TODO: The elements `word` and @tt{word} are different. Add wiki
%   syntax for it? (see Haddock)
%
% * Other elements
%
% We will not include lightweight mark-up syntax for anything else
% not described in this document (e.g., images)
%
% ===========================================================================
% = Variations of mark-up
% ===========================================================================
%
% The following lines should be presented exactly in the same way:
%
%  - Text can be /emphasized/ or *bold*.
%  - Text can be \em{emphasized} or \bf{bold}.
%  - Text can be @em{emphasized} or @bf{bold}.
%
% ===========================================================================
% = Syntax for code
% ===========================================================================
%
% * Code spans
%
% These two lines should be equivalent:
% 
%  - This is a predicate name `append/3`, a variable name `X`, an atom
%    name `foo`, a quoted atom name `'foo'`.
%
%  - This is a predicate name @code{append/3}, a variable name
%    @code{X}, an atom name @code{foo}, a quoted atom name
%    @code{'foo'}.
%
% TODO: Add @code{append/3} as the long mark-up counterpart?
% TODO: How ` can be escaped? (is there any other option than using
%  the long mark-up language? 
%
% * Blocks of code
%
% Text that is 4-char indented is recognized as code:
%
%     list([]).
%     list([X|Xs]) :- list(Xs)
%
% Code itself can have comments:
%
%     % definition for lists
%     list([]). % see append/3
%     list([X|Xs]) :- list(Xs)
%
% Code itself can have documentation comments:
%
%     %! definition for lists
%     list([]). %< see `append/3`
%     list([X|Xs]) :- list(Xs)
%
% * Blocks of other code
%
% This is a piece of C code:
%   @begin{code}{c}
%   #include <stdio.h>
%   int main() { return 0; }
%   @end{code}

% ===========================================================================
% = Documentation comments in programs
% ===========================================================================
%
%! * Documentation in programs
%
%  Syntax for documentation comments is @tt{%!} and @tt{%<} for
%  documenting the preceding program element.
%
%  The following are some programs with comments:

%! The `foo` regular type
:- regtype foo :=
         case1  %< First case. See `bar/0`.
       | case2. %< Second case

%! Again, the `foo` regular type
:- regtype foo :=
         case1 /*< First case. See `bar/0`. */
       | case2 /*< Second case. */.

% ===========================================================================
% = Documentation of assertions in programs
% ===========================================================================

%! * Assertions in programs
%
%  Complex assertions can be included in special @tt{@assertions}
%  blocks. The compiler can optionally read them if necessary (they
%  are not merely documentation):

%! @assertions
%    :- true comp length(A,B) + native.
%    :- true pred length(L,N) : list * var => list * int.
%                 %< Computes the length of `L`
%    :- true pred length(L,N) : var * int => list * int.
%                 %< Outputs `L` of length `N`
%    :- true pred length(L,N) : list * int => list * int.
%                 %< Checks that `L` is of length `N`

length(L, N) :- var(N), !, llength(L, 0, N).
length(L, N) :- dlength(L, 0, N).

% TODO: We need more compact ways of writing assertions (not only
%   syntactic sugar, we need semantics for that). E.g.
%
%       :- true pred length(L,N):
%          ( list * var => list * int  %< Computes the length of `L`
%          | var  * int => list * int  %< Outputs `L` of length `N`
%          | list * int => list * int  %< Checks that `L` is of length `N`
%          ).
%
%       :- true pred length(L,N) =>
%          ( list * int  %< Computes the length of `L`
%          | list * int  %< Outputs `L` of length `N`
%          | list * int  %< Checks that `L` is of length `N`
%          ).

%! * Simpler assertions
%
%  Syntax for simpler assertions.
%
% TODO: Choose one of them

%@ pred length(+list,-int).
%< Computes the length of the first argument.
length(_,_).

%:- pred length(+list,-int).
%< Computes the length of the first argument.
length(_,_).

%- pred length(+list,-int).
%< Computes the length of the first argument.
length(_,_).

% TODO: Is it worth for very complex assertions?

%- true comp length(A,B) + native.
%- true pred length(L,N) : list * var => list * int.
             %< Computes the length of `L`
%- true pred length(L,N) : var * int => list * int.
             %< Outputs `L` of length `N`
%- true pred length(L,N) : list * int => list * int.
             %< Checks that `L` is of length `N`

length(L, N) :- var(N), !, llength(L, 0, N).
length(L, N) :- dlength(L, 0, N).

%! * Combining assertions with assertion-related predicates

%! This is the definition of a regular type `foo` and its code.

%- regtype foo/1.
%< Defines foo.

%@ foo := a
%      |  b
%      |  c.
%  foo := ~number.

%! It can be described more compactly as:

%- regtype foo/1. %< Defines foo.

%@ foo := a
%      |  b
%      |  c.
%  foo := ~number.

%! It can be described even more compactly as:

%! Defines foo
%- regtype foo :=
%      a
%    | b
%    | c.
%    | ~number.

% TODO: How is this last notation shown a .lpdoc file?
% TODO: The goal here is including optional definitions. Consider 
%   :- if(...) directives too?

% ===========================================================================
% = Missing elements in the long mark-up language
% ===========================================================================
%
% TODO: Missing elements for the large mark-up:
%  - Image description (for browsers)
%  - Command for table of contents
%  - Tables
%  - More complex code blocks (with options, such as the language)
%  - Distinguish between code examples and real code (for literate programs)
%
% ===========================================================================
% = Tables
% ===========================================================================
%
% Proposal:
%
%   Use orgtbl mode for tables. It can be enabled with 'M-x
%   orgtbl-mode'. However, it has some nasty interactions with the
%   Ciao mode for code (e.g., see:
%
%      foo := a
%           | b.
%   ).
%
%   References:
%    http://www.gnu.org/software/emacs/manual/html_node/org/A-LaTeX-example.html

/*
#+ORGTBL:
| Benchmark | A   | B   | C |
|-----------+-----+-----+---|
| epoa      | eee | foo |   |
*/
