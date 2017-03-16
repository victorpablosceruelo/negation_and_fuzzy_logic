:- module(atomic_basic, [], [pure, assertions, isomodes]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(basic_props)).
:- use_module(engine(term_typing)).
:- use_module(engine(term_basic)).
:- use_module(engine(exceptions)).

:- doc(title, "Basic predicates handling names of constants").

:- doc(author, "The CLIP Group").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module, "The Ciao system provides builtin predicates which
   allow dealing with names of constants (atoms or numbers).  Note that
   sometimes strings (character code lists) are more suitable to handle
   sequences of characters.").

:- '$native_include_c_source'(.(atomic_basic)).

% ---------------------------------------------------------------------------
:- export(name/2).
:- doc(name(Const,String), "@var{String} is the list of the ASCII
   codes of the characters comprising the name of @var{Const}.  Note
   that if @var{Const} is an atom whose name can be interpreted as a
   number (e.g. '96'), the predicate is not reversible, as that atom
   will not be constructed when @var{Const} is uninstantiated.  Thus it
   is recommended that new programs use the ISO-compliant predicates
   @pred{atom_codes/2} or @pred{number_codes/2}, as these predicates do
   not have this inconsistency.").

:- true pred name(+constant,?string) .
:- true pred name(-constant,+string)
   # "If @var{String} can be interpreted as a number, @var{Const} is unified
      with that number, otherwise with the atom whose name is @var{String}.".
:- true comp name/2 + ( sideff(free), native ).
:- '$props'(name/2, [impnat=cbool(prolog_name)]).

% ---------------------------------------------------------------------------
:- export(atom_codes/2).
:- doc(atom_codes(Atom,String), "@var{String} is the list of the ASCII
   codes of the characters comprising the name of @var{Atom}.").

:- true pred atom_codes(+atm,?string) .
:- true pred atom_codes(-atm,+string) .
:- true comp atom_codes/2 + ( sideff(free), native, iso ).
:- '$props'(atom_codes/2, [impnat=cbool(prolog_atom_codes)]).

% ---------------------------------------------------------------------------
:- export(number_codes/2).
:- doc(number_codes(Number,String), "@var{String} is the list of the
   ASCII codes of the characters comprising a representation of
   @var{Number}.").

:- true pred number_codes(+num,?string) .
:- true pred number_codes(-num,+string) .
:- true comp number_codes/2 + ( sideff(free), native, iso ).
:- '$props'(number_codes/2, [impnat=cbool(prolog_number_codes_2)]).

% ---------------------------------------------------------------------------
:- export(number_codes/3).
:- doc(number_codes(Number,String,Base), "@var{String} is the list
   of the ASCII codes of the characters comprising a representation of
   @var{Number} in base @var{Base}.").

:- true pred number_codes(+num,?string,+int) .
:- true pred number_codes(-num,+string,+int) .
:- true comp number_codes/3 + ( sideff(free), native ).
:- '$props'(number_codes/3, [impnat=cbool(prolog_number_codes_3)]).

% ---------------------------------------------------------------------------
:- export(atom_number/2).
:- doc(atom_number(Atom,Number), "@var{Atom} can be read as a
   representation of @var{Number}.").

:- true pred atom_number(+atm,?num) .
:- true pred atom_number(-atm,+num) .
:- true comp atom_number/2 + ( sideff(free), native ).

atom_number(A, N) :-
        atom(A), number(N), !,
        atom_codes(A, S),
        number_codes(N0, S),
        N = N0.               % So that atom_number('2.3e1',23.0) succeeds
atom_number(A, N) :-
        atom(A), var(N), !,
        atom_codes(A, S),
        number_codes(N, S).
atom_number(A, N) :-
        var(A), number(N), !,
        number_codes(N, S),
        atom_codes(A, S).
atom_number(A, N) :-
        ( var(A) ->
          ( var(N) ->
            throw(error(instantiation_error, atom_number/2-1))
          ; throw(error(type_error(number, N), atom_number/2-2))
          )
        ; atom(A) ->
            throw(error(type_error(number, N), atom_number/2-2))
        ; throw(error(type_error(atom, A), atom_number/2-1))
        ).

% ---------------------------------------------------------------------------
:- export(atom_length/2).
:- doc(atom_length(Atom,Length), "@var{Length} is the number of
   characters forming the name of @var{Atom}.").

:- true pred atom_length(+atm,?int) .
:- true comp atom_length/2 + ( sideff(free), native, iso ).
:- '$props'(atom_length/2, [impnat=cbool(prolog_atom_length)]).

% ---------------------------------------------------------------------------
:- export(atom_concat/3).
:- doc(atom_concat(Atom_1,Atom_2,Atom_12), "@var{Atom_12} is the
   result of concatenating @var{Atom_1} followed by @var{Atom_2}.").

:- true pred atom_concat(+atom,+atom,?atom)
   # "Concatenate two atoms.".
:- true pred atom_concat(-atom,-atom,+atom)
   # "Non-deterministically split an atom.".
:- true pred atom_concat(-atom,+atom,+atom)
   # "Take out of an atom a certain suffix (or fail if it cannot be done).".
:- true pred atom_concat(+atom,-atom,+atom)
   # "Take out of an atom a certain prefix (or fail if it cannot be done).".
:- true comp atom_concat/3 + ( sideff(free), native, iso ).
:- '$props'(atom_concat/3, [impnat=cbool(prolog_atom_concat)]).

% ---------------------------------------------------------------------------
:- export(sub_atom/4).
:- doc(sub_atom(Atom,Before,Length,Sub_atom), "@var{Sub_atom} is
   formed with @var{Length} consecutive characters of @var{Atom}
   after the @var{Before} character.  For example, the goal
   @tt{sub_atom(summer,1,4,umme)} succeeds.").

:- true pred sub_atom(+atm,+int,+int,?atm).
:- true comp sub_atom/4 + ( sideff(free), native ).
:- '$props'(sub_atom/4, [impnat=cbool(prolog_sub_atom)]).
