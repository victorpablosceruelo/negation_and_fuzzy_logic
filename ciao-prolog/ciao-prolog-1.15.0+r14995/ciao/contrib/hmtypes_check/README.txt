Description of the Ciao port of Tom's Schrijvers type checker for
Prolog.

  hmtypes_check_common.pl: 

    Core of the type checking algorithm, this file is included from
    hmtypes_check_rt.pl (runtime) and hmtype_check_tr.pl (compile-time).

  hmtypes_check_doc.pl:
  
    Documentation.
  
  hmtypes_check_ops.pl:

    Definitions for syntax (operators)
  
  hmtypes_check_prelude.pl:

    Some useful type definitions.

  hmtypes_check_runtime_db.pl:

    Definition of runtime information stored for each program to do
    runtime type checks.
  
  hmtypes_check_rt.pl:

    The runtime system for type checking. It includes
    the file hmtypes_check_common.pl.
  
  hmtypes_check_tr.pl:

    The compilation module. It type checks the program and adds
    runtime checks when necessary (when going from typed to untyped
    predicates). It includes the file hmtypes_check_common.pl.

  hmtypes_check.pl:

    The type checker package file. It loads the compilation module,
    the syntax, the runtime DB definitions, and the prelude.
  
--
Jose F. Morales <jfmcjf@gmail.com>
Sat Feb 14 19:37:39 CET 2009
