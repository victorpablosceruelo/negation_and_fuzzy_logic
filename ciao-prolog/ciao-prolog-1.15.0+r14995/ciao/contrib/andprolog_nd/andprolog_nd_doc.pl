:- use_package(assertions).


:- comment(nodoc, assertions).

:- comment(filetype, package).

:- comment(title,  "And-parallel execution").

:- comment(author, "Amadeo Casas Cuadrado").
:- comment(author, "@tt{http://www.ece.unm.edu/~amadeo}").
:- comment(author, "University of New Mexico").

:- comment(module, "This library allows and-parallel execution of goals in
                    (Herbrand-)independent fashion. It resembles the
                    execution rules of &-Prolog.").

:- comment(usage, "The AND_PARALLEL_EXECUTION flag must be set to
                   \"yes\" in order to compile the engine with support
                   for the and-parallel execution of
                   goals. Concurrency primitives are defined in the
                   @tt{apll\_nd} library. That flag may be set to
                   \"visandor\" to, in addition, add support for
                   VisAndOr's events for deterministic parallel
                   goals.").


:- include(library(andprolog_nd(andprolog_nd_ops))).

:- use_module(library(andprolog_nd(andprolog_nd_rt))).


