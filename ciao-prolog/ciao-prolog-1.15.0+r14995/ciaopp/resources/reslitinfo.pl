:- module(reslitinfo, [litinfo_get_mode/2, litinfo_get_measure/2],
	    [assertions, resources(inferres_decl), hiord]).

:- use_package(library(resdefs(resources_decl))).

:- use_module(resources(init_res(symtable_res)), [literal_property/10]).
:- reexport(resources(resources_basic), [litinfo_get_key/2,
		litinfo_get_litnum/2, litinfo_get_lit/2, slitinfo/1]).

:- pred litinfo_get_mode/2 :: slitinfo * list.
litinfo_get_mode(litinfo${literal => Lit, approx => Approx,
		extra => stat_litinfo${bt => BT, st => ST,
		    clauseppkey => ClausePPKey, key => Key, ppkey => PPKey,
		    litnum => LitNum}}, Mode) :-
	literal_property(BT, ST, Lit, ClausePPKey, Key, PPKey, LitNum, mode,
	    Approx, Mode).

:- pred litinfo_get_measure/2 :: slitinfo * list.
litinfo_get_measure(litinfo${literal => Lit, approx => Approx,
		extra => stat_litinfo${bt => BT, st => ST,
		    clauseppkey => ClausePPKey, key => Key, ppkey => PPKey,
		    litnum => LitNum}}, Measure) :-
	literal_property(BT, ST, Lit, ClausePPKey, Key, PPKey, LitNum, measure,
	    Approx, Measure).
