:- module(paths_,[],[assertions]).

:- include( .(path_setup_) ).
:- include( .(paths_common_) ).


:- doc(version_maintenance,dir('version')).


:- doc(version(1*0+574,2004/07/20,15:32*04+'CEST'), "Moved options
   to paths_common.  (Edison Mera)").

:- doc(version(1*0+504,2004/07/02,13:40*00+'CEST'), "added path to
   @tt{api} (David Trallero Mena)").

:- doc(version(1*0+406,2004/04/04,15:38*06+'CEST'), "Added path
   @tt{rtchecks}.  (German Puebla)").

