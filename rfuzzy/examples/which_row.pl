:- module(which_row,_,[rfuzzy, clpr]).

rows_list(row1).
rows_list(row2).
rows_list(row3).
rows_list(row4).

:- set_prop which_row/1 => rows_list.
:- default(which_row/1, 0.1).
(which_row(J) cred (luka,0.9)) :~ prod people(J), products_people(J).


:- set_prop people/1 => rows_list.
:- default(people/1, 0.1).

people(row1) value 0.6.
people(row2) value 0.15.
people(row3) value 0.95.
people(row4) value 0.7.

:- set_prop products_people/1 => rows_list.
:- default(products_people/1, 0.5).

products_people(row2) value 0.2.
products_people(row4) value 0.8.