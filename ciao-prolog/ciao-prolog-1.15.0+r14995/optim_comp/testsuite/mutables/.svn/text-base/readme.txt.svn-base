This directory contains several implementation of mutable variables
(ala ImProlog) in Prolog.

'test_code.pl' implements a sample program whose behaviour should be
(almost) identical with all versions.

---------------------------------------------------------------------------
Notes and TO-DO list:

- implement a version (e.g. 'mutables_dic') that uses dictionaries
  (e.g. key/value lists)

- copying (like in copy_term/2 or assert/consult in dynamic
  predicates) may behave differently. For an atomic key-based
  implementation, the mutable is shared on copy. For a variable-based
  implementation, the mutable may be copied (along with all descendant
  mutables in its value).

  It may not be a good idea to mix copy_term and assert/consult with
  mutable variables in real programs.

---------------------------------------------------------------------------

Author: Jose F. Morales
Tue Oct 28 10:13:31 CET 2008
