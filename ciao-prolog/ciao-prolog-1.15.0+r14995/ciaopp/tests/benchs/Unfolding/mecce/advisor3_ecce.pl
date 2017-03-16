% [what_to_do_todayX( Weather1, Program1, Weather2, Program2,  Weather3, Program3)].

what_to_do_todayX( Weather1, Program1, Weather2, Program2,  Weather3, Program3):-
	what_to_do_today( Weather1, Program1),
	what_to_do_today( Weather2, Program2),
	what_to_do_today( Weather3, Program3).

what_to_do_today( Weather, Program ):-
	what_to_do_today_( first_of_may, Weather, Program ).

what_to_do_today_( _today, _weather, _program ):-
    kind_of_day( _today, _daykind ),
    kind_of_weather( _weather, _weatherkind ),
    proposal( _daykind, _weatherkind, _program ).

kind_of_day( monday, workday ). 
kind_of_day( thuesday, workday ).
kind_of_day( wednesday, workday ).
kind_of_day( thursday, workday ).
kind_of_day( friday, workday ).
kind_of_day( saturday, weekend ).
kind_of_day( sunday, weekend ).
kind_of_day( eastern, feastday ).
kind_of_day( first_of_may, feastday ).
kind_of_day( christmas, feastday ).
kind_of_day( new_years_day, badday ).
kind_of_day( friday_the_13th, badday ).

kind_of_weather( sunny, nice ).
kind_of_weather( rainy, nasty ).
kind_of_weather( foggy, nasty ).
kind_of_weather( windy, nasty ).

proposal( workday, _, go_to_work ).
proposal( weekend, nice, go_out_to_the_nature ).
proposal( weekend, nice, visit_the_golf_club ).
proposal( weekend, nice, wash_your_car ).
proposal( weekend, nasty, go_out_to_the_town ).
proposal( weekend, nasty, visit_the_bridge_club ).
proposal( weekend, nasty, enjoy_yourself_at_home ).
proposal( weekend, _, it_is_fun_to_learn_Japanese ).
proposal( badday, _, you_had_better_stay_in_bed ).
proposal( feastday, _weather, _program ) :-
    proposal( weekend, _weather, _program ).




%renamed runtime queries
% rrtq([
%     what_to_do_today__1( sunny, _program ),
%     what_to_do_today__1( _wheather, enjoy_yourself_at_home ),
%     what_to_do_today__1( foggy, _program ),
%     what_to_do_today__1( _wheather, wash_your_car ),
%     what_to_do_today__1(  nice, wash_your_car )
%     ]).

% sol( what_to_do_today__1(sunny,go_out_to_the_nature)).
% sol( what_to_do_today__1(sunny,visit_the_golf_club)).
% sol( what_to_do_today__1(sunny,wash_your_car)).
% sol( what_to_do_today__1(sunny,it_is_fun_to_learn_Japanese)).
% sol( what_to_do_today__1(rainy,go_out_to_the_town)).
% sol( what_to_do_today__1(rainy,visit_the_bridge_club)).
% sol( what_to_do_today__1(rainy,enjoy_yourself_at_home)).
% sol( what_to_do_today__1(rainy,it_is_fun_to_learn_Japanese)).
% sol( what_to_do_today__1(foggy,go_out_to_the_town)).
% sol( what_to_do_today__1(foggy,visit_the_bridge_club)).
% sol( what_to_do_today__1(foggy,enjoy_yourself_at_home)).
% sol( what_to_do_today__1(foggy,it_is_fun_to_learn_Japanese)).
% sol( what_to_do_today__1(windy,go_out_to_the_town)).
% sol( what_to_do_today__1(windy,visit_the_bridge_club)).
% sol( what_to_do_today__1(windy,enjoy_yourself_at_home)).
% sol( what_to_do_today__1(windy,it_is_fun_to_learn_Japanese)).
