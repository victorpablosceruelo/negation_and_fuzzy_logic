================================
BUILD (AND REBUILD) INSTRUCTIONS
================================

First, you must make sure that 'ciaoppcl' has been enabled in the Ciao
configuration. If not, you can enable it by configuring Ciao with the
'--with-ciaoppcl=yes' option, e.g.:

  $ ./ciaosetup configure \
      --silent=true --instype=local \
      --unused-pred-warnings=yes --with-ciaoppcl=yes

The rest of options may change depending on your build preferences.
Once the configuration finishes (make sure that 'ciaoppcl' is enabled
in the configuration report), you can (re)build the 'ciaoppcl'
executable with:

  $ cd ciaopp
  $ lpmake build_ciaoppcl

======================
SIMPLE EXAMPLES OF USE
======================

Run 'analyse' with default options:
  
  ./ciaoppcl -A ~/PrologTemp/fact.pl

Run 'optimise' with default options:

  ./ciaoppcl -O ~/PrologTemp/fact.pl

Some options can be set in order to modify default behaviour.

Examples:

 -- for setting a different 'modes' domain:

   ./ciaoppcl -A ~/PrologTemp/fact.pl -fmodes=pd

 -- for disabling 'types' analysis:

   ./ciaoppcl -A ~/PrologTemp/fact.pl -ftypes=none

The list of available flags is at [TODO: to be defined].

======================
MORE ADVANCED EXAMPLES
======================
[TODO: to be written]

A) Picking configuration options interactively (see the "Menu
   Configuration Name" option below):

[staff-vpn172:CiaoDE/ciaopp/command_line] leuschel% ./ciaoppcl -Q ~/PrologTemp/fact.pl
Ciao Precompiler V1.0
 | We are in the process of merging all 0.8 functionality into  1.0.
 | Please bear with us in the meantime. Sorry for any inconvenience.



                        (Press h for help)

Select Menu Level:               [naive, expert] (naive) ? 
Select Action Group:             [analyze, check_assertions, optimize]
                                  (analyze) ? 
Select Cost Analysis:            [none, steps_ub, steps_lb, steps_ualb, 
                                  steps_o] (none) ? 
Select Mode Analysis:            [none, pd, pdb, def, gr, share, shareson, 
                                  shfr, shfrson, shfrnv, son, share_amgu, 
                                  share_clique, sharefree_amgu, 
                                  sharefree_clique, aeq, depth, path, 
                                  difflsign, fr, frdef, lsign] (shfr) ? 
Select Type Analysis:            [none, eterms, ptypes, svterms, terms]
                                  (eterms) ? 
Select Type Output:              [defined, all] (all) ? 
Perform Non-Failure Analysis:    [none, nf, nfg] (none) ? 
Perform Determinism Analysis:    [none, det] (none) ? 
Print Program Point Info:        [off, on] (off) ? 
Collapse AI Info:                [off, on] (on) ? 
Note: Current Saved Menu Configurations: []
Menu Configuration Name:          (none) ? default

-------------------------------

B) Now we can use the stored configuration (called 'default'):

(For the moment the analysed files have to have a module declaration, e.g.,
 :- module(_,_). )
 
 

[staff-vpn172:CiaoDE/ciaopp/command_line] leuschel% ./ciaoppcl -U default ~/PrologTemp/fact.pl
Ciao Precompiler V1.0
 | We are in the process of merging all 0.8 functionality into  1.0.
 | Please bear with us in the meantime. Sorry for any inconvenience.

Restoring Menu Configuration default
{Loading current module from /Users/leuschel/PrologTemp/fact.pl
{loaded in 1725.257999833503 msec.}
}
{Analyzing /Users/leuschel/PrologTemp/fact.pl
{preprocessed for plai in 0.9519999998892808 msec.}
{In /Users/leuschel/PrologTemp/fact.pl
WARNING (trust): (lns 2-3) Cannot analyze one version of 
 arithmetic:<(_25990,2)
  i.e., there is no trust for call pattern:
   [rt0(_25990)]
}
{In /Users/leuschel/PrologTemp/fact.pl
WARNING (trust): (lns 2-3) Cannot analyze one version of 
 arithmetic:<(_34909,2)
  i.e., there is no trust for call pattern:
   [num(_34909)]
}
{In /Users/leuschel/PrologTemp/fact.pl
WARNING (trust): (lns 2-3) Cannot analyze one version of 
 arithmetic:<(_25983,2)
  i.e., there is no trust for call pattern:
   [term(_25983)]
}
{analyzed by plai using eterms in 31.22699999766077 msec.}
}
{Analyzing /Users/leuschel/PrologTemp/fact.pl
{preprocessed for plai in 0.7749999999098663 msec.}
{analyzed by plai using shfr in 3.68299999965624 msec.}
}
{written file /Users/leuschel/PrologTemp/fact_eterms_shfr_co.pl}

===========================
SOME NOTABLE CONFIGURATIONS
===========================

1) Options for specialisation:

[staff-vpn172:CiaoDE/ciaopp/command_line] leuschel% ./ciaoppcl -Q ~/PrologTemp/fact.pl
Ciao Precompiler V1.0
 | We are in the process of merging all 0.8 functionality into  1.0.
 | Please bear with us in the meantime. Sorry for any inconvenience.



                        (Press h for help)

Use Saved Menu Configuration:    [none, default] (none) ? none
Select Menu Level:               [naive, expert] (naive) ?    
Select Action Group:             [analyze, check_assertions, optimize]
                                  (analyze) ? optimize
Select Optimize:                 [none, spec, parallelize, slice, poly_spec]
                                  (spec) ? spec
Select Abs Specialization:       [off, mono, poly] (off) ? poly
Select Analysis Domain:          [none, pd, pdb, def, gr, share, shareson, 
                                  shfr, shfrson, shfrnv, son, share_amgu, 
                                  share_clique, sharefree_amgu, 
                                  sharefree_clique, aeq, depth, path, 
                                  difflsign, fr, frdef, lsign, eterms, ptypes, 
                                  svterms, terms] (pd) ? 
Note: Current Saved Menu Configurations: [default]
Menu Configuration Name:          (none) ? simple_pe
{Loading current module from /Users/leuschel/PrologTemp/fact.pl
{loaded in 1818.388999824515 msec.}
}




