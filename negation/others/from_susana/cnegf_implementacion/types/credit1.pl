
%% Revised and corrected version by Pedro Lopez-Garcia 9-Jan-97
%% find %%-PL 

:- module(credit1, [credit/2], [assertions, regtypes]).

% :- use_module(library(lists), [list/2, list1/2]).
:- use_module(library(aggregates), [findall/3]).

%:- entry credit(A, B): (ground(A), var(B), mshare([[B]])).
:- entry credit(A, B): (ground(A), var(B)).

:- entry collateral(A, B).

:- entry amount(A, B, C).

 %% :- entry collateral(A, B): (ground(A), coll(A), var(B)).
 %% 
 %% :- entry amount(A, B, C): (ground(A), amo(A), ground(B), cli(B), var(C)).
 %% 
 %% :- regtype coll/1.
 %% 
 %% coll(local_currency_deposits).
 %% coll(foreign_currency_deposits).
 %% coll(negotiate_instruments).
 %% coll(mortgage).
 %% 
 %% :- regtype amo/1.
 %% 
 %% amo(local_currency_deposits).
 %% amo(foreign_currency_deposits).
 %% amo(bank_guarantees).
 %% amo(negotiate_instruments).
 %% amo(stocks).
 %% amo(mortgage).
 %% amo(documents).
 %% 
 %% :- regtype cli/1.
 %%  
 %% cli(client1).

    /*   Client data  */
 
 bank_yield(client1,excellent).
 requested_credit(client1,5000).
 
 amount(local_currency_deposits,client1,3000).
 amount(foreign_currency_deposits,client1,2000).
 amount(bank_guarantees,client1,300).
                      
 amount(negotiate_instruments,client1,500).
 amount(stocks,client1,900).
 
 amount(mortgage,client1,1200).
 amount(documents,client1,1400).
 
 value(net_worth_per_assets,client1,40).
 value(last_year_sales_growth,client1,20).
 value(gross_profits_on_sales,client1,45).
 value(short_term_debt_per_annual_sales,client1,9).
 
 ok_profile(client1).
 
 
 %  Program 22.2: Test data for the credit evaluation system


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*   Credit Evaluation

     credit(Client,Answer) :-
	Answer is the reply to a request by Client for credit.
*/
      credit(Client,Answer) :-
           ok_profile(Client),
           collateral_rating(Client,CollateralRating),
           financial_rating(Client,FinancialRating),
           bank_yield(Client,Yield),
           evaluate(profile(CollateralRating,FinancialRating,Yield),Answer) , !.

/*  The collateral rating module

     collateral_rating(Client,Rating) :-
	Rating is a qualitative description assessing the collateral
	offered by Client to cover the request for credit.
*/
     collateral_rating(Client,Rating) :-
        collateral_profile(Client,FirstClass,SecondClass,Illiquid),
        collateral_evaluation(FirstClass,SecondClass,Illiquid,Rating).

     collateral_profile(Client,FirstClass,SecondClass,Illiquid) :-
	requested_credit(Client,Credit),
	collateral_percent(first_class,Client,Credit,FirstClass),
	collateral_percent(second_class,Client,Credit,SecondClass),
	collateral_percent(illiquid,Client,Credit,Illiquid).
        %%-PL illiquid was Illiquid
     collateral_percent(Type,Client,Total,Value) :-
	findall(X,(collateral(Collateral,Type),
			amount(Collateral,Client,X)),Xs),
   	sumlist(Xs,Sum),
	Value is Sum*100/Total.

     /*   Evaluation rules    */

     collateral_evaluation(FirstClass,_SecondClass,_Illiquid,excellent) :-
  	FirstClass >= 100.
     collateral_evaluation(FirstClass,SecondClass,_Illiquid,excellent) :-
  	FirstClass > 70, FirstClass + SecondClass >= 100.
     collateral_evaluation(FirstClass,SecondClass,Illiquid,good) :-
  	FirstClass + SecondClass > 60,
  	FirstClass + SecondClass < 70,
  	FirstClass + SecondClass + Illiquid >= 100.

     /*  Bank data - classification of collateral   */
 

     collateral(local_currency_deposits,first_class).
     collateral(foreign_currency_deposits,first_class).
     collateral(negotiate_instruments,second_class).
     collateral(mortgage,illiquid).

%    Financial rating

/*  
    financial_rating(Client,Rating) :-
	Rating  is a qualitative description assessing the financial 
	record offered by Client to support the request for credit.
*/	
     financial_rating(Client,Rating) :-
        financial_factors(Factors),
        score(Factors,Client,0,Score),
        calibrate(Score,Rating).

     /*   Financial evalauation rules   */

     calibrate(Score,bad) :- 	   Score =< -500.
     calibrate(Score,medium) :-    -500 < Score, Score < 150.
     calibrate(Score,good) :- 	   150 =< Score, Score < 1000.
     calibrate(Score,excellent) :- Score >= 1000.

     /*  Bank data - weighting factors	*/

     financial_factors([(net_worth_per_assets,5),
         (last_year_sales_growth,1),
         (gross_profits_on_sales,5),
         (short_term_debt_per_annual_sales,2)  ]).

     score([(Factor,Weight)|Factors],Client,Acc,Score) :-
        value(Factor,Client,Value),
        Acc1 is Acc + Weight*Value,
        score(Factors,Client,Acc1,Score).
     score([],_Client,Score,Score).

/*  Final evaluation    

     evaluate(Profile,Outcome) :-
	Outcome is the reply to the client's Profile.
*/
     evaluate(Profile,Answer) :- 
	rule(Conditions,Answer), verify(Conditions,Profile).

     verify([condition(Type,Test,Rating)|Conditions],Profile) :-
        scale(Type,Scale),
        select_value(Type,Profile,Fact),
        compare(Test,Scale,Fact,Rating),
        verify(Conditions,Profile).
     verify([],_Profile).

     compare(equal,_Scale,Rating,Rating).
     compare(greater_than,Scale,Rating1,Rating2) :-
        precedes(Scale,Rating1,Rating2).
 %%      compare(greater_or_equal,Scale,Rating1,Rating2) :-
 %%         precedes(Scale,Rating1,Rating2) ; Rating1 = Rating2.
     compare(greater_or_equal,Scale,Rating1,Rating2) :-
        precedes(Scale,Rating1,Rating2).
     compare(greater_or_equal,_Scale,Rating1,Rating2) :-
        Rating1 = Rating2.

     compare(less_than,Scale,Rating1,Rating2) :-
        precedes(Scale,Rating2,Rating1).
 %%      compare(less_or_equal,Scale,Rating1,Rating2) :-
 %%         precedes(Scale,Rating2,Rating1) ; Rating1 = Rating2.
     compare(less_or_equal,Scale,Rating1,Rating2) :-
        precedes(Scale,Rating2,Rating1).
     compare(less_or_equal,_Scale,Rating1,Rating2) :-
        Rating1 = Rating2.

     precedes([R1|_Rs],R1,_R2).
     precedes([R|Rs],R1,R2) :- R \== R2, precedes(Rs,R1,R2).

	select_value(collateral,profile(C,_F,_Y),C).
	select_value(finances,profile(_C,F,_Y),F).
	select_value(yield,profile(_C,_F,Y),Y).

     /*  Utilities   */

	sumlist(Is,Sum) :-
		sumlist3(Is,0,Sum).

	sumlist3([I|Is],Temp,Sum) :-
		Temp1 is Temp + I,
		sumlist3(Is,Temp1,Sum).
	sumlist3([],Sum,Sum).


/*  Bank data and rules	*/

rule([condition(collateral,greater_or_equal,excellent),condition(finances,greater_or_equal,good),
			condition(yield,greater_or_equal,reasonable)],give_credit).  
rule([condition(collateral,equal,good),condition(finances,equal,good),
			condition(yield,greater_or_equal,reasonable)],consult_superior).
rule([condition(collateral,less_or_equal,moderate),condition(finances,less_or_equal,medium)],
							  refuse_credit).

scale(collateral,[excellent,good,moderate]).
scale(finances,[excellent,good,medium,bad]).
scale(yield,[excellent,reasonable,poor]).

%  Program 22.1:  A credit evaluation system

%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "on"
%% End:

