%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Hindley-Milner Type Checker for Ciao
%
%   Based on 'type_check.pl' by Tom Schrijvers (and others).
%
% Author(s):
%
%   Jose F. Morales <jfmcjf@gmail.com> (port, integration, improvements)
%   and the authors of original checker.
%
% Changes with respect to 'type_check.pl':
%
%   - port to Ciao's expansion system
%   - predicate signatures specified as assertions
%   - type definitions specified as assertions and clauses
%       
%       (alias types)
%       e.g. :- type t(A) == p(int,A).
%       ===>
%            :- prop t/1 + hmtype.
%            t(X,A) :- p(X,int,A).
%
%       (polymorphic data types)
%       e.g. :- type t(A) ---> p(int,A).
%       ===>
%            :- prop t/1 + hmtype.
%            t(X,A) :- p(X,int,A).
%   - type 'string' (old version) is 'atm'
%   - fixed type arithmetic expression integer/1
%
% TODO:
%   - arithexp/1 is incomplete! I do not know how to treat the last case.
%     Could I define it as a type class? or not?
% 
%   - generalizing 'numeric' as type classes
%
%   - Declarations are local to each module, but not yet fully
%     integrated into the module system (they cannot be exported).
%
%   - Error messages not integrated with Ciao
%
%   - We need a logical implication '==>' for assertions
%     (see handwritten annotations -- JFMC)
%
% NOTE: 
%
%   Revision r12997 was the latest compatible with the original
%   'type_check.pl'.  Newer versions are based on the Ciao's assertion
%   system.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% ORIGINAL DOCUMENTATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%
% All authors agree to the licences of SWI-Prolog and YAP 
%
% AUTHORS OF CODE: 
%   Tom Schrijvers
%   Bart Demoen
%   Markus Triska
%   YOUR NAME HERE
%
% ACKNOWLEDGEMENTS:
%   Ulrich Neumerkel for providing feedback	
%   Vitor Santos Costa
%   Jose Santos
%   YOUR NAME HERE
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% DOCUMENTATION
%
% Type Definitions
% ----------------
%
% Define polymorphic algebraic data types like:
%
%	:- hm_type pair(A,B) ---> A - B.
%	:- hm_type list(T)   ---> [] ; [T|list(T)].
%	:- hm_type boolean   ---> true ;  false.
%
% (NOTE: the above types are predefined, as well as integer and float.)
%
% TODO: This is strange; did nothing! --JFMC
%
% Type definitions can also be empty, e.g.
%
%	:- hm_type an_empty_type.
%
% This means that the type is not inhabited by instantiated values. Only
% logical variables are possible.
%
% Predicate Signatures
% --------------------
%
% Predicates are given a signature like:
%
%	:- hm_pred append(list(E), list(E), list(E)).
%	:- hm_pred not(boolean, boolean).
%	:- hm_pred lookup(list(pair(Key,Value)), Key, Value).
%
% Interfacing Untyped Code
% ------------------------
%
% 1) Calling typed code from untyped code (and the Prolog top-level)
%    results in runtime type checks.
%
% 2) One may annotate calls to untyped predicates from within typed predicates:
%
%       :- hm_pred concat(list(list(integer)),list(integer)).
%
%	concat(LL,L) :- flatten(LL,L) :: flatten(list(list(integer)),list(integer)).
%
%    which results in runtime type checking. The annotation is also used for static
%    type checking of the code surrounding the annotated call.
%
%    A variant of the annotation is only used for static type checking, and does
%    not result in runtime checks:
%	
%	concat(LL,L) :- flatten(LL,L) :< flatten(list(list(integer)),list(integer)).
%
% 3) A second way is to provide a signature for untypable code with: 
%	
%	:- trust_hm_pred sort(list(integer),list(integer)).
%
%    This signature is only into account when checking calls from
%    typed code.
%
% Coping with Untypable Code
% --------------------------
%
% Untypable code, e.g. using Prolog built-ins, may be encapsulated 
% in a trusted predicate. E.g.
%
%	:- trust_hm_pred new_array(list(T),array(T)).
%
%	new_array(List,Array) :- Array =.. [array|List].
%
% No additional runtime checks are performed for trusted predicates.
%
% Similarly, untyped imported predicates may be given a type signature
% with the 'trust_hm_pred' declaration.
%
% Type Checker Options
% --------------------
%
% Options can be passed to the type checker with the declaration
%
%	:- hm_type_check_options(Options).
%
% where Options is a list containing zero or more of the following elements:
%
%	check(Flag) where Flag is on or off, 
%		to enable or disable the type checker
%		enabled by default
%
%	runtime(Flag) where Flag is on or off,
%		to enable or disable runtime type checking
%		disabled by default
%
%	verbose(Flag) where Flag is on or off,
%		to enable or disable printed summary at end of type checking
%		enabled by default
%
% The declaration ':- runtime_hm_type_check(Flag)' is a shorthand for:
%
%	:- hm_type_check_options([runtime(Flag)]).
% 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% CURRENT LIMITATIONS 
%
%	* global namespace for types
%	* runtime type checks are not exhaustive for (non-ground) polymorphic types
%
% TODO:
%	* check uniqueness of defined types, e.g. list(T) not defined twice
%	* check syntactic well-formedness of type definitions and declarations
%	* add module awareness for predicates
%	* add module awareness for types 
%	* take care with variables used in :: (also used for values / other annotations)
%	* improve error messages with tc_stats(Errors,Total)
%		- source location information
%	  	- what the inconsistency is
%	* support for more built-ins 
%	* higher-order types for meta-predicates
%	* exported vs. hidden types
%	* abstract types (hidden definition)
%	* automatic inference of signatures
%	* type classes
%
% CHANGES:
%	* added cmp/0 type
%	* added compare/3 built-in
%	* added statistics printing at end of type checking
%	* fixed detection of less general types than signature
%	* added error message for less polymorphic signature
%	* added error message for duplicate predicate signature
%	* added :< annotation, which is a variant of  the :: annotation,
%	        where the semantics is not to include runtime assertions, 
%               but to simply trust the programmer.
%	* added type pred/0 for goals
%	* added call/1 built-in
%	* added type pred/1 and pred/2
%	* addded call/1 and call/2 built-ins
%	* improved error message for less polymorphic inferred types
%	* added string/0 type
%	* added get_char/1 built-in
%	* added atom_concat/3 built-in
%	* added atom_length/2 built-in
%	* added atom_chars/2 built-in
%	* added concat_atom/2 built-in
%	* added any/0 type
%	* added coercion predicate any_to_type/3
%	* added coercion predicate type_to_any/2
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
