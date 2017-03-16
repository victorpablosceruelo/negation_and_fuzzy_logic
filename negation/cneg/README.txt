
Last update date: 2011-09-20
Authors: Víctor Pablos Ceruelo, Susana Munoz-Hernandez and Juan José Moreno-Navarro.

Related information: paper sent to PADL'2012

-------------------------------------------------------------------------------------------------------------------------------

Ciao Prolog version used (negation works in this version of Ciao Prolog): 
subversion repository revision r11293.
You can download a Ciao Prolog r11293 precompiled package for Debian from 
http://babel.ls.fi.upm.es/~vpablos/others/packages/debian/
More info at: http://babel.ls.fi.upm.es/%7Evpablos/others/packages/readme.html
Equivalent official package for Ciao Prolog (if any) can be found at:
http://www.ciaohome.org/packages/

-------------------------------------------------------------------------------------------------------------------------------

This is a negation system for Prolog.
We provide three different implementations of negation in Prolog:
- Chan's // Munoz-Hernandez's proposal Constructive Negation:
@inproceedings{DBLP:conf/iclp/Chan88,
  author    = {David Chan},
  title     = {Constructive Negation Based on the Completed Database},
  booktitle = {ICLP/SLP},
  year      = {1988},
  pages     = {111-125},
  bibsource = {DBLP, http://dblp.uni-trier.de}
}

@inproceedings{DBLP:conf/slp/Chan89,
  author    = {David Chan},
  title     = {An Extension of Constructive Negation and its Application in Coroutining},
  booktitle = {NACLP},
  year      = {1989},
  pages     = {477-493},
  bibsource = {DBLP, http://dblp.uni-trier.de}
}

@inproceedings{DBLP:conf/iclp/Munoz-HernandezM04,
  author    = {Susana Mu{\~n}oz-Hern{\'a}ndez and
               Juan Jos{\'e} Moreno-Navarro},
  title     = {Implementation Results in Classical Constructive Negation},
  booktitle = {ICLP},
  year      = {2004},
  pages     = {284-298},
  ee        = {http://springerlink.metapress.com/openurl.asp?genre=article{\&}issn=0302-9743{\&}volume=3132{\&}spage=284},
  crossref  = {DBLP:conf/iclp/2004},
  bibsource = {DBLP, http://dblp.uni-trier.de}
}

- Barbuti's // Munoz-Hernandez's proposal Constructive Intensional Negation:

@article{Barbuti1,
   author = {R. Barbuti and D. Mancarella and D. Pedreschi and
   F. Turini},
   title     = {Intensional Negation of Logic Programs},
   booktitle = {Procedings of International Joint Conference on Theory
   and Practice of Software Development, TAPSOFT'87},
   journal   = {LNCS},
   pages     = {96--110},
   year      = {1987},
   month     = {},
   volume    = {250},
   note      = {},
   publisher = {Springer-Verlag},
   address   = {},
   editor    = {H. Ehrig and R. Kowalski and G. Levi and U. Montanari}
}

@article{Barbuti2,
   author = {R. Barbuti and D. Mancarella and D. Pedreschi and
   F. Turini},
   title = {A Transformational Approach to Negation in Logic Programming},
   journal   = {JLP},
   pages     = {201--228},
   year      = {1990},
   volume    = {8},
   number    = {3}
}

@article{DBLP:journals/ngc/MarinoMM08,
  author    = {Julio Mari{\~n}o and
               Juan Jos{\'e} Moreno-Navarro and
               Susana Mu{\~n}oz-Hern{\'a}ndez},
  title     = {Implementing Constructive Intensional Negation},
  journal   = {New Generation Comput.},
  volume    = {27},
  number    = {1},
  year      = {2008},
  pages     = {25-56},
  ee        = {http://dx.doi.org/10.1007/s00354-008-0053-9},
  bibsource = {DBLP, http://dblp.uni-trier.de}
}

- Stuckey's proposal Negation in Constraint Logic Programming, 
  restricted by Pablos-Ceruelo to constraints formed only by the relations
  =/2 and =/=/2 :
@MISC{Stuckey95negationand,
    author = {Peter J. Stuckey},
    title = {Negation and Constraint Logic Programming},
    year = {1995}
}
  And the paper sent to PADL'2012.

-------------------------------------------------------------------------------------------------------------------------------

Please note there is a huge difference between \== (Prolog's built-in disequality)
and =/= (Our implementation of disequalities).

Chan's proposal problems:

Barbuti's proposal problems:

Stuckey's proposal problems:

p(X) :- X =/= A, A = b, X = c

frontier: X =/= A, A = b, X = c

negation: forall A. X = A ; ( forall A. X =/= A, ( forall A. A =/= b ; ( forall A = b, X =/= c )))

Pablos-Ceruelo proposal:

p(X) :- X =/= A, A = b, X = c

frontier: X =/= A, A = b, X = c

reordering: A = b, X = c, X =/= A

negation: forall A. A =/= b ; ( forall A. A = b, ( X =/= c  ; ( X=c, forall A. X = A )))

-------------------------------------------------------------------------------------------------------------------------------
