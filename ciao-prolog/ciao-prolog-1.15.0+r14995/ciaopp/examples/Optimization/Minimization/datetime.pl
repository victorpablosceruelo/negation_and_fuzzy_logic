:- module(datetime,[main/3],[assertions]).


% The Date Time Library contains predicates that
% perform logical arithmetic on dates and times.
% The distinction is most noticable when dealing
% with months, which have varying numbers of days.

%-----------------------------------------------------------
% date_add(+DATE_1, +DATE_QUANTITIES, -DATE_2)
%
% Adds the DATE_QUANTITIES to DATE_1 structure,
% returning DATE_2 structure.  The DATE_QUANTITIES
% are either a single structure or list of structures
% of the form days(D), months(M), or weeks(W).  Each
% is an operator, so can be written as '3 months' for
% example.
%
% The arithmetic is pure date arithmetic.  That is
% it adds calendar months, so Feb 15th plus one
% month yields Mar 15th.  Adding years over leap
% years winds up on same days as well.  Dates are
% correctly fixed for the corner cases, so an intermediate
% result of Feb 30th will become Mar 2nd in a non leap year
% and Mar 1st in leap year.
%
% ?- date_add(date(2002,1,15), [1 months, 2 days], D).
% D = date(2002, 2, 17) 
% yes
%
% ?- date_add(date(2002,1,15), [1 years, 1 months, 15 days], D).
% D = date(2003, 3, 2) 
% yes
%
% The special case of the last day of the month is
% recognized as well, so adding one month to the last
% day of a month gets the last day of the next month.
%
% ?- date_add(date(2002,1,31), 1 months, X).
% X = date(2002, 2, 28) 
% yes
%
% ?- date_add(date(2002,2,28), 1 months, X).
% X = date(2002, 3, 31) 
% yes
%

:- op(50, xf, days).
:- op(50, xf, months).
:- op(50, xf, weeks).
:- op(50, xf, years).
:- op(50, xf, hours).
:- op(50, xf, mins).
:- op(50, xf, secs).


main(A,B,C):- 
	add_dates([date(2002,2,28),date(2002,2,28),date(2002,2,28),
	           date(2002,2,28),date(2002,2,28),date(2002,2,28),
		   date(2002,1,15),date(2002,1,15),date(2002,1,15),
		   date(2002,1,15),date(2002,1,15),date(2002,1,15)|A],
	          [[1 months,1 weeks],1 months,[1 years, 1 months, 15 days],
		   [1 months, 2 days],3 days,[7 years, 15 days],
		   [1 months,1 weeks],1 months,[1 years, 1 months, 15 days],
		   [1 months, 2 days],3 days,[7 years, 15 days]|B],C).
%         sub_dates([date(2002,3,2), date(2002,2,28), date(2002,7,28),
%   	           date(2003,4,2), date(2002,3,27), date(2003,7,27),
%   		   date(2004,5,2), date(2002,4,26), date(2004,7,26),
% 		   date(2005,6,2), date(2002,5,25), date(2005,7,25)|A],
% 	          [date(2002,1,15),date(2002,1,31), date(2002,6,31),
%   		   date(2002,2,14),date(2002,1,31), date(2003,5,31),
%   		   date(2002,3,13),date(2002,1,31), date(2004,4,31),
% 		   date(2002,4,12),date(2002,1,31), date(2005,3,31)|B],C).



add_dates([],L,L):-!.
add_dates(L,[],L):-!.
add_dates([D1|R1],[D2|R2],[D3|R3]):-
	date_add(D1,D2,D3),
	add_dates(R1,R2,R3).

sub_dates([],L,L):-!.
sub_dates(L,[],L):-!.
sub_dates([D1|R1],[D2|R2],[D3|R3]):-
	date_difference(D1,D2,D3),
	sub_dates(R1,R2,R3).


date_add(DATE, [], DATE) :-
   !.
date_add(D1, A1 + A2, DATE) :-
   convert_exp(A1+A2, AList),
   date_add(D1, AList, DATE),
   !.
date_add(D1, A1 - A2, DATE) :-
   convert_exp(A1-A2, AList),
   date_add(D1, AList, DATE),
   !.
date_add(D1, [DUNIT|DUNITS], DATE) :-
   date_add(D1, DUNIT, D2),
   !,
   date_add(D2, DUNITS, DATE).
date_add(today, ADD, DATE) :-
   date_get(today, D1),
   !,
   date_add(D1, ADD, DATE).
date_add(D1, -ADD, DATE) :-
   ADD =.. [UNIT, AMOUNT],
   MADD =.. [UNIT, -AMOUNT],
   date_add(D1, MADD, DATE).
date_add(date(Y,M,D), days(D1), date(YY,MM,DD)) :-
   D2 is D + D1,
   date_fix(date(Y,M,D2), date(YY,MM,DD)).
date_add(date(Y,M,D), weeks(D1), date(YY,MM,DD)) :-
   D2 is D + 7 * D1,
   date_fix(date(Y,M,D2), date(YY,MM,DD)).
date_add(date(Y,M,D), months(M1), date(YY,MM,DD)) :-
   M2 is M + M1,
   date_islast(date(Y,M,D), D2),
   date_fix(date(Y,M2,D2), date(YY,MM,DD)).
date_add(date(Y,M,D), years(Y1), date(YY,MM,DD)) :-
   Y2 is Y + Y1,
   date_islast(date(Y,M,D), D2),
   date_fix(date(Y2,M,D2), date(YY,MM,DD)).

convert_exp(Exp, List) :-
   convert_exp_(Exp, [], List).

convert_exp_(I1+I2, SoFar, List) :-
   !, convert_exp_(I1, [I2|SoFar], List).
convert_exp_(I1-I2, SoFar, List) :-
   !, convert_exp_(I1, [-I2|SoFar], List).
convert_exp_(-Int, SoFar, [-Int|SoFar]) :-
   !.
convert_exp_(Int, SoFar, [Int|SoFar]) :-
   !.

% make a date correct

date_fix(date(Y,M,D), date(YY,MM,DD)) :-
   M < 1,
   !,
   M2 is M + 12,
   Y2 is Y - 1,
   date_fix(date(Y2,M2,D), date(YY,MM,DD)).
date_fix(date(Y,M,D), date(YY,MM,DD)) :-
   M > 12,
   !,
   M2 is M - 12,
   Y2 is Y + 1,
   date_fix(date(Y2,M2,D), date(YY,MM,DD)).
date_fix(date(Y,M,last), date(Y,M,MD)) :-
   !,
   date_month_days(M,Y,MD).
date_fix(date(Y,M,D), date(YY,MM,DD)) :-
   D < 1,
   !,
   M2 is M - 1,
   date_month_days(M2,Y,MD),
   D2 is D + MD,
   date_fix(date(Y,M2,D2), date(YY,MM,DD)).
date_fix(date(Y,M,D), date(YY,MM,DD)) :-
   date_month_days(M,Y,MD),
   D > MD,
   !,
   M2 is M + 1,
   D2 is D - MD,
   date_fix(date(Y,M2,D2), date(YY,MM,DD)).
date_fix(date(Y,M,D), date(Y,M,D)).


%-----------------------------------------------------------
% date_get(+DATE_TYPE, -DATE)
%
% Given one of the DATE_TYPEs, seen below, returns
% the DATE structure.
%

%date_get(today, date(Y,M,D)) :- date(M,D,Y).
date_get(yesterday, DATE) :- date_add(today, days(-1), DATE).
date_get(tomorrow, DATE) :- date_add(today, days(1), DATE).
date_get(last_week, DATE) :- date_add(today, weeks(-1), DATE).
date_get(next_week, DATE) :- date_add(today, weeks(1), DATE).
date_get(last_month, DATE) :- date_add(today, months(-1), DATE).
date_get(next_month, DATE) :- date_add(today, months(1), DATE).
date_get(last_year, DATE) :- date_add(today, years(-1), DATE).
date_get(next_year, DATE) :- date_add(today, years(1), DATE).

% date_islast(+DATE, -DAY)
%
% if the day is the last day of the month,
% mark it as 'last', instead of its number.

date_islast(date(Y,M,MD), last) :-
   date_month_days(M,Y,MD), !.
date_islast(date(_Y,_M,D), D).

date_month_days(0,_,31).
date_month_days(1,_,31).
date_month_days(2,Y,29) :- date_leap_year(Y), !.
date_month_days(2,_,28).
date_month_days(3,_,31).
date_month_days(4,_,30).
date_month_days(5,_,31).
date_month_days(6,_,30).
date_month_days(7,_,31).
date_month_days(8,_,31).
date_month_days(9,_,30).
date_month_days(10,_,31).
date_month_days(11,_,30).
date_month_days(12,_,31).
date_month_days(13,_,31).

date_leap_year(Y) :- 0 =:= Y mod 4, 0 =\= Y mod 1000.


% date_difference(+DATE_1, +DATE_2, -DATE_QUANTITIES).
%
% Subtracts, in pure date mode, DATE_2 date structure
% from DATE_1 date structure, providing a result of
% a list of date quantities.  Note that years are
% rounded, but that the result in the days(D) structure
% might be negative.  This is to allow the correct
% behavior when reapplying the difference by adding it
% to another date.
%
% ?- date_difference(date(2002,3,2), date(2002,1,15), D).
% D = [0 years, 2 months, -13 days] 
% yes
%
% The special case of both dates being end of month
% is recognized as being just a difference of one month.
%
% ?- date_difference(date(2002,2,28), date(2002,1,31), X).
% X = [0 years, 1 months, 0 days] 
% yes
%

date_difference(date(Y1,M1,D1), date(Y2,M2,D2),
      [years(Y), months(M), days(D)]) :-
   (D2 > D1 ->
      (date_islast(date(Y1,M1,D1), last) ->
         M1a is M1,
         D1a is D2
         ;
         M1a is M1 - 1,
         date_month_days(M1a,Y1,Dprev),
         D1a is D1 + Dprev )
      ;
      D1a = D1,
      M1a = M1 ),
   (M2 > M1a ->
      M1b is M1a + 12,
      Y1b is Y1 - 1
      ;
      M1b = M1a,
      Y1b = Y1 ),
   Y is Y1b - Y2,
   M is M1b - M2,
   D is D1a - D2.
