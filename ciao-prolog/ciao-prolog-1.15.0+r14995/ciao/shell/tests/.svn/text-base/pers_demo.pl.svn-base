:-module(pers_demo, [p/2, q/3], [persdb]).

:- multifile persistent_dir/2.
:- data persistent_dir/2.

persistent_dir(db, '/home/clip/tmp/db').

:- persistent(p/2, db).
:- persistent(q/3, db).
