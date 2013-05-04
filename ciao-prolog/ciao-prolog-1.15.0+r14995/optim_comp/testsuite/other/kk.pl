/*
   test steps:

   compile kk (it should not complain)
   rename kk2.pl to kk3.pl
   compile again kk (it should complain)
*/
:- use_module(.(kk2)).
