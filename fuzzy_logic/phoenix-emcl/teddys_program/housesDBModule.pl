:- module(housesDBModule,_,[rfuzzy,clpr,.(qualifiersPack),.(generalQueryPack),debugger_pkg]).
:- use_module(engine(hiord_rt)).

% TYPE DECLARATION

% TYPE DEFINITION
housetype('apartment'). housetype('villa'). housetype('town_house').

codetype('lfs2168'). codetype('lfs2144'). codetype('lfs2147'). codetype('lfs2145').
codetype('c358'). codetype('lfs2110'). codetype('lfs2124'). codetype('lfs2123').
codetype('lfs2155'). codetype('lfs2111'). codetype('lfs2047'). codetype('lfs2041').
codetype('es13462'). codetype('lfs1942'). codetype('lfs1917'). codetype('lfb143').
codetype('5607/152'). codetype('es13340'). codetype('lfs1939'). codetype('lfs1938').

% DATABASE
%List of attributes for the database 
attributes([house_code,house_type,size,number_of_rooms,price,distance_from_center, distance_from_beach]).

house(lfs2168,'apartment',114,5,630000,2,5700).
house(lfs2144,'apartment',77,3,420000,7,3500).
house(lfs2147,'apartment',80,2,675000,12,200).
house(lfs2145,'apartment',224,8,790000,20,100).
house(c358,'apartment',74,3,340000,5,3100).
house(lfs2110,'apartment',415,9,2500000,8,2400).
house(lfs2124,'apartment',63,2,275000,15,450).
house(lfs2123,'apartment',62,3,285000,6,1000).

house(lfs2111,'villa',700,10,1100000,9,4500).
house(lfs2047,'villa',1750,11,1650000,15,1000).
house(lfs2041,'villa',4000,13,2500000,4,1800).
house(es13462,'villa',600,6,4000000,6,1500).
house(lfs1942,'villa',900,10,3100000,3,3400).
house(lfs1917,'villa',210,5,590000,13,5000).
house(lfb143,'villa',1200,9,2750000,7,4000).
house(5607/152,'town_house',161,7,815000,6,1200).
house(es13340,'town_house',1025,8,2800000,25,7000).
house(lfs1939,'town_house',860,9,1800000,14,2400).
house(lfs1938,'town_house',520,11,1990000,19,80).
house(lfs2155,'villa',2300,9,3000000,13,800).

% FUZZY FUNCTIONS OVER THE DATABASE
expensive(X,Y):- house(X,_,_,_,P,_,_),expensive_func(P,Y).
cheap(X,Y):- house(X,_,_,_,P,_,_),cheap_func(P,Y).
big(X,Y):- house(X,_,S,_,_,_,_),big_func(S,Y).
small(X,Y):- house(X,_,S,_,_,_,_),small_func(S,Y).
close_to_center(X,Y):- house(X,_,_,_,_,D,_),close_to_center_func(D,Y).
far_from_center(X,Y):- house(X,_,_,_,_,D,_),far_from_center_func(D,Y).
close_to_beach(X,Y):- house(X,_,_,_,_,_,D),close_to_beach_func(D,Y).

%The whole set of FUZZY FUNCTIONS is represented by the fact below:
fuzzy_Func_List([expensive,cheap,big,small,close_to_center,far_from_center,close_to_beach]).

% CRISP FUNCTIONS
house_type(X,Y):- house(X,Y,_,_,_,_,_).
number_of_rooms(X,Y):-house(X,_,_,Y,_,_,_).
house_price(X,Y):- house(X,_,_,_,Y,_,_).


%The whole set of CRISP PREDICATES is represented by the fact below:
crisp_Pred_List([house_type,number_of_rooms,house_price]).

%Crisp functions taken to 'expsearch' from here

% FUZZY FUNCTIONS OVER QUANTITATIVE ATTRIBUTES
%:- set_prop expensive_func/1 => positive_integer/1.
:- default(expensive_func/1,1).
expensive_func :# ([(50000,0),(100000,0.1),(250000,0.2),(350000,0.3),(450000,0.5),(550000,0.6),
	            (800000,0.7),(1000000,0.8),(1500000,0.9),(2500000,1)]).

%:- set_prop cheap_func/1 => positive_integer/1.
:- default(cheap_func/1,0).
cheap_func :# ([(0,1),(30000,1),(50000,0.8),(100000,0.7),(250000,0.5),(350000,0.3),
	            (450000,0.1),(550000,0)]).

%:- set_prop big_func/1 => positive_integer/1.
:- default(big_func/1,1).
big_func :# ([(0,0),(50,0.1),(80,0.2),(120,0.3),(200,0.4),(300,0.5),(500,0.7),(1000,0.8),(1500,0.9),(2500,1)]).

%:- set_prop small_func/1 => positive_integer/1.
:- default(small_func/1,0).
small_func :# ([(0,1),(50,1),(80,0.9),(100,0.8),(150,0.7),(200,0.5),(300,0.2),(400,0.1),(500,0)]).

%:- set_prop close_to_center_func/1 => positive_integer/1.
:- default(close_to_center_func/1,0).
close_to_center_func :# ([(0,1),(2,1),(4,0.8),(7,0.6),(10,0.5),(12,0.3),(15,0.2),(20,0)]).

%:- set_prop far_from_center_func/1 => positive_integer/1.
:- default(far_from_center_func/1,1).
far_from_center_func :# ([(0,0),(7,0),(8,0.1),(10,0.3),(14,0.4),(20,0.7),(25,0.8),(30,1)]).

%:- set_prop close_to_beach_func/1 => positive_integer/1.
:- default(close_to_beach_func/1,0).
close_to_beach_func :# ([(0,1),(100,1),(1000,0.5),(2000,0)]).


%SAMPLE GENERAL QUERIES TO RUN ON THIS DATABASE:
% genQuery([(not,id,expensive),(id,id,big),(id,very,close_to_beach)],[(number_of_rooms,lessthan,10)],all,P).
% genQuery([(not,id,expensive),(id,id,big),(id,very,close_to_beach)],[(number_of_rooms,lessthan,10)],nonzero,P).
%genQuery([(not,id,expensive),(id,id,big),(id,very,close_to_beach)],[(number_of_rooms,lessthan,10)],best,P).