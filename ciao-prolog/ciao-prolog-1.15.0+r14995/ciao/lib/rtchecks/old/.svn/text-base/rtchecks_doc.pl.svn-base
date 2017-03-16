:- use_package([assertions,regtypes]).

:- doc(nodoc,assertions).
:- doc(nodoc,regtypes).

:- doc(filetype,package).

:- doc(title,"Run-time checking of assertions").

:- doc(author, "David Trallero Mena").

:- doc( module , 
"This library package can be used to perform run-time checking of
assertions. Properties are checked during execution of the program and
errors found (when the property does not hold) are reported."
).

:- use_module(library(assertions(assertions_props)), [property_conjunction/1]).

:- doc(check/1,"See @ref{The Ciao assertion package}.").
:- trust pred check( Prop ) : property_conjunction
   # "@var{Prop} is checked. If it fails, an exception is raised.".
:- impl_defined(check/1).
