% Syntax translations for the foreign interface

:- module(foreign_interface__syntax, [foreign_interface__syntax/3], []).

foreign_interface__syntax((:- use_foreign_source(Spec)), 
                          [(:- '$native_include_c_source'(Spec))], _).
