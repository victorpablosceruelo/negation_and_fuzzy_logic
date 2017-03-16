:- module(engine_monitor,[],[foreign_interface,assertions]).

:- extra_linker_opts('LINUXi86', ['-lGL -lGLU -lglut']). % TODO: not tested
:- extra_linker_opts('DARWINi86', ['-m32 -framework GLUT -framework OpenGL -framework Cocoa']).
:- extra_linker_opts('Win32', ['-lopengl32 -lglut32']). % TODO: not tested
:- use_foreign_source([engine_monitor_c]).

:- export(begin_monitor/0).
:- true pred begin_monitor + foreign.

:- export(monitor_eng/1).
:- true pred monitor_eng_(+int) + foreign_low(prolog_monitor_eng).
monitor_eng('$goal_id'(GoalDesc, _)) :- monitor_eng_(GoalDesc).




