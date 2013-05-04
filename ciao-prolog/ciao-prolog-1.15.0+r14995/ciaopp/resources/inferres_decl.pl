:- package(inferres_decl).
:- use_package(assertions).
:- use_package(nativeprops).
:- use_package(regtypes).
:- use_package(argnames).

%% Uncomment the following packages to perform debugging and/or
%% run-time checking in the resources library (EMM):

% :- use_package(rtchecks).
% :- use_package(library(rtchecks(rtchecks_rt_library))).
% :- use_package(debug).

:- use_module(library(resdefs(resources_props)), [approx/1]).


:- doc(author, "Edison Mera").

:- doc(module, "This module contains declarations that are common
	to some modules of the resources analysis.").

:- argnames stat_litinfo(litnum, key, bt, st, clauseppkey, ppkey).

:- doc(init_resource(ResourceModule),
"If this predicate is defined in a module that is loaded using the
 @pred{:- load_resource_module(ResourceModule)} directive, then it is
 called before to begin the resources analysis.").

:- multifile init_resource/1.
