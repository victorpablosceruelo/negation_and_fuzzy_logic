
:- use_module(db_leisure,_).

t1(R, TV) :- assertLocalUserName('Testing_User_at_localhost_localnet'),(restaurant(_12, _13, _14, _15, _16, _17, _18), =(R, restaurant(_12, _13, _14, _15, _16, _17, _18))), not_very_expensive(R, TV), rfuzzy_var_truth_value(R, _22, _21), rfuzzy_var_truth_value(_12, _24, _23), rfuzzy_var_truth_value(_13, _26, _25), rfuzzy_var_truth_value(_14, _28, _27), rfuzzy_var_truth_value(_15, _30, _29), rfuzzy_var_truth_value(_16, _32, _31), rfuzzy_var_truth_value(_17, _34, _33), rfuzzy_var_truth_value(_18, _36, _35), rfuzzy_var_truth_value(TV, _38, _37).

t2(R, TV) :- assertLocalUserName('Testing_User_at_localhost_localnet'),(restaurant(_12, _13, _14, _15, _16, _17, _18), =(R, restaurant(_12, _13, _14, _15, _16, _17, _18))), fnot(very(expensive(R, _A), _B), TV), rfuzzy_var_truth_value(R, _22, _21), rfuzzy_var_truth_value(_12, _24, _23), rfuzzy_var_truth_value(_13, _26, _25), rfuzzy_var_truth_value(_14, _28, _27), rfuzzy_var_truth_value(_15, _30, _29), rfuzzy_var_truth_value(_16, _32, _31), rfuzzy_var_truth_value(_17, _34, _33), rfuzzy_var_truth_value(_18, _36, _35), rfuzzy_var_truth_value(TV, _38, _37).

t3(R, TV1, TV2) :- 
	(restaurant(_12, _13, _14, _15, _16, _17, _18), =(R, restaurant(_12, _13, _14, _15, _16, _17, _18))), 
	 t3_aux(R, TV1, TV2).

t3_aux(R, TV1, TV2) :- 
	(
	     (
		 t1(R, TV1), 
		 t2(R, TV2), !
	     )
	 ;
	     (
		 t1(R, TV1), !
	     )
	 ;
	     (
		 t2(R, TV2), !
	     )
	 ;
	     (
		 true
	     )
	 ).


t4(A, B, C, D, E, F, G, H, I) :- restaurant(_12, _13, _14, _15, _16, _17, _18), =(_19, restaurant(_12, _13, _14, _15, _16, _17, _18)), rfuzzy_compute(=~=, tapas, food_type(_19), restaurant, _20), rfuzzy_var_truth_value(_19, _22, I), rfuzzy_var_truth_value(_12, _24, H), rfuzzy_var_truth_value(_13, _26, G), rfuzzy_var_truth_value(_14, _28, F), rfuzzy_var_truth_value(_15, _30, E), rfuzzy_var_truth_value(_16, _32, D), rfuzzy_var_truth_value(_17, _34, C), rfuzzy_var_truth_value(_18, _36, B), rfuzzy_var_truth_value(_20, _38, A).

t5(A, B, C, D, E, F, G, H, I) :- restaurant(_12, _13, _14, _15, _16, _17, _18), =(_19, restaurant(_12, _13, _14, _15, _16, _17, _18)), rfuzzy_compute(=~=, spanish, food_type(_19), restaurant, _20), rfuzzy_var_truth_value(_19, _22, I), rfuzzy_var_truth_value(_12, _24, H), rfuzzy_var_truth_value(_13, _26, G), rfuzzy_var_truth_value(_14, _28, F), rfuzzy_var_truth_value(_15, _30, E), rfuzzy_var_truth_value(_16, _32, D), rfuzzy_var_truth_value(_17, _34, C), rfuzzy_var_truth_value(_18, _36, B), rfuzzy_var_truth_value(_20, _38, A).

