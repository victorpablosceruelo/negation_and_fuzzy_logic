:- module(cars3, _, [rfuzzy, clpr]).
car(fiat_cinquecento).
car(vw_caddy).
car(alfa_romeo_gt).
car(aston_martin_bulldog).
car(ford_fiesta).
rfuzzy_type_for(has_lower_price/2, [car/1, car/1]).
rfuzzy_default_value_for(has_lower_price/2, 0.5).
 
 
rfuzzy_type_for(expensive_car/1, [car/1]).
rfuzzy_default_value_for(expensive_car/1, 0.9) if expensive_type/1.
rfuzzy_default_value_for(expensive_car/1, 0.5).
expensive_type(X) :-
X = alfa_romeo;
X = aston_martin;
X = vw_phaeton.
expensive_car(ferrari) value 1.0.
rfuzzy_type_for(better_car/2, [car/1, car/1]).
rfuzzy_default_value_for(better_car/2, 0.5).
rfuzzy_type_for(expensive_manpower/1, [car/1]).
rfuzzy_default_value_for(expensive_manpower/1, 0.9) if expensive_type/1.
rfuzzy_default_value_for(expensive_manpower/1, 0.5).
rfuzzy_type_for(expensive_parts/1, [car/1]).
rfuzzy_default_value_for(expensive_parts/1, 0.9) if expensive_type/1.
rfuzzy_default_value_for(expensive_parts/1, 0.5).
rfuzzy_type_for(garage_taxes/1, [car/1]).
rfuzzy_default_value_for(garage_taxes/1, 0.1).
garage_taxes(Car) :~ prod expensive_car(Car), expensive_parts(Car), expensive_manpower(Car).
garage_taxes(ferrari) value 0.9 .
positive_integer(1).
positive_integer(X) :-
positive_integer(Y),
number(Y),
X is Y + 1.
rfuzzy_type_for(car_life_is_10_years/1, [positive_integer/1]).
rfuzzy_default_value_for(car_life_is_10_years/1, 1).
car_life_is_10_years :# ([(0,0.2),(24000,0.5),(30000,0.7),(50000,0.9),(60000,1)]).
% First variable is price ...
 