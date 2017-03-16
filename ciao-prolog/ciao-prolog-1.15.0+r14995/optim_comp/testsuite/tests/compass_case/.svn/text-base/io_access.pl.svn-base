:- module(_, [], [pure, foreign_interface]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(basic_props)).

:- export(read_mono/2).
:- true pred read_mono(+term, -int) + foreign_low(read_mono).

:- export(write_stereo/2).
:- true pred write_stereo(+int, +int) + foreign_low(write_stereo).

:- export(open_read_mono/2).
:- true pred open_read_mono(+term, -term) + foreign_low(open_read_mono).

:- export(open_write_stereo/0).
:- true pred open_write_stereo + foreign_low(open_write_stereo).

:- export(close_write_stereo/0).
:- true pred close_write_stereo + foreign_low(close_write_stereo).

:- export(read_compass/1).
:- true pred read_compass(-int) + foreign_low(read_compass).

:- true pred init_compass + foreign_low(init_compass_prolog).
:- initialization(init_compass).

:- use_foreign_source(.(io_access_c)).
