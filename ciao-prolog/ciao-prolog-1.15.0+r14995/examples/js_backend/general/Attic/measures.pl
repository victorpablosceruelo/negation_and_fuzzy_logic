% Distances
:- class distance {
    :- export(s/1).
    :- attr s.

    :- virtual name/1.
    :- virtual norm/1.
    :- virtual cons/2.

    :- export(cons__/1).
    cons__(X) :- ~s = X.

    % TODO: extend an interface for those operations
    :- export('$to_str'/1).
    '$to_str' := ~s + " " + ~name.
    :- export('$add'/2).
    '$add'(Y) := ~cons(~norm.s + ~Y.norm.s).
    :- export('$div'/2).
    '$div'(Y) := ~norm.s / ~Y.norm.s.
    :- export('$gt'/1).
    '$gt'(Y) :- ~norm.s > ~Y.norm.s.
}.
% Distances in the I.S. units
% TODO: FIX!!!!!!!!!!!!!!!!!!!!!!
% TODO: bug! no cons__! no initialization! look at inits from parents...
:- op(50, yf, [m, dm, cm, mm]).
:- fun_eval(m/1).
:- class (m) { 
    :- extends distance.

    :- export(s/1).
    :- attr s. % TODO: inherit form 'distance'

    :- export(cons__/1).
    cons__(X) :- ~s = X. % TODO: inherit cons__, removed
    :- export(norm/1).
    norm := ~self. 
    :- export(cons/2).
    cons(X) := X m. 
    :- export(name/1).
    name := "meters".
}.
% Distances that normalize to 'm'
:- class m_distance {
    :- extends distance.
    :- virtual f/1.
    norm := (~self.s * ~f) m.
    :- export(cons/2).
    cons(X) := X m. % TODO: strange...
}.
% Definition of other standard distances
:- fun_eval(dm/1).
:- class (dm) {
    :- extends m_distance.

    :- export(s/1).
    :- attr s. % TODO: inherit form 'distance'

    :- export(cons__/1).
    cons__(X) :- ~s = X. % TODO: inherit cons__, removed
    % (virtual)
    f := 0.1.
    :- export(name/1).
    name := "decimeters".
}.
:- fun_eval(cm/1).
:- class (cm) {
    :- extends m_distance.

    :- export(s/1).
    :- attr s. % TODO: inherit form 'distance'

    :- export(cons__/1).
    cons__(X) :- ~s = X. % TODO: inherit cons__, removed
    % (virtual)
    f := 0.01.
    :- export(name/1).
    name := "centimeters".
}.
:- fun_eval(mm/1).
:- class (mm) {
    :- extends m_distance.

    :- export(s/1).
    :- attr s. % TODO: inherit form 'distance'

    :- export(cons__/1).
    cons__(X) :- ~s = X. % TODO: inherit cons__, removed
    % (virtual)
    f := 0.001.
    :- export(name/1).
    name := "millimeters".
}.
% Distances in the Imperial units
:- op(50, yf, [inches, feet, yards, furlongs, miles]).
:- fun_eval(inches/1).
:- class (inches) {
    :- extends m_distance.

    :- export(s/1).
    :- attr s. % TODO: inherit form 'distance'

    :- export(cons__/1).
    cons__(X) :- ~s = X. % TODO: inherit cons__, removed
    % (virtual)
    f := 25.4 mm / 1 m.
    :- export(name/1).
    name := "inches".
}.
:- fun_eval(feet/1).
:- class (feet) {
    :- extends m_distance.

    :- export(s/1).
    :- attr s. % TODO: inherit form 'distance'

    :- export(cons__/1).
    cons__(X) :- ~s = X. % TODO: inherit cons__, removed
    % (virtual)
    f := 12 inches / 1 m.
    :- export(name/1).
    name := "feet".
}.
:- fun_eval(yards/1).
:- class (yards) {
    :- extends m_distance.

    :- export(s/1).
    :- attr s. % TODO: inherit form 'distance'

    :- export(cons__/1).
    cons__(X) :- ~s = X. % TODO: inherit cons__, removed
    % (virtual)
    f := 3 feet / 1 m.
    :- export(name/1).
    name := "yards".
}.
:- fun_eval(furlongs/1).
:- class (furlongs) {
    :- extends m_distance.

    :- export(s/1).
    :- attr s. % TODO: inherit form 'distance'

    :- export(cons__/1).
    cons__(X) :- ~s = X. % TODO: inherit cons__, removed
    % (virtual)
    f := 220 yards / 1 m.
    :- export(name/1).
    name := "furlongs".
}.
:- fun_eval(miles/1).
:- class (miles) {
    :- extends m_distance.

    :- export(s/1).
    :- attr s. % TODO: inherit form 'distance'

    :- export(cons__/1).
    cons__(X) :- ~s = X. % TODO: inherit cons__, removed
    % (virtual)
    f := 8 furlongs / 1 m.
    :- export(name/1).
    name := "miles".
}.

% TODO: with clp it is possible to ask things like: 10 cm = X m.
:- module measures {
    main :-
        console.display("Creating a distance..."), console.nl,
        M = 2 dm,
        console.display("Created, its value is:"), console.nl,
        console.display(M), console.nl,
        console.display("The normalized value is:"), console.nl,
        console.display(~M.norm), console.nl,
        console.display("Some distances:"), console.nl,
        console.display(1 m), console.nl,
        console.display(2 dm), console.nl,
        console.display(3 cm), console.nl,
        console.display(4 mm), console.nl,
        console.display("Sum of distance is:"), console.nl,
        console.display(1 m + 2 dm + 3 cm + 4 mm), console.nl.
    
%test :-
%    1.234 m = 1 m + 2 dm + 3 cm + 4 mm,
%    10 miles + 1 furlongs > 10 m.

    :- export(attach/1).
    attach(Container) :-
        simple_run_widget("Units of Measurement", run).attach(Container).
  
    :- export(run/0).
    run :-
        console.clear,
        main.
}.
