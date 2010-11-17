Con el mio obtenemos:


?- frontier(dist_list([(X,Y)]),L).

L = [(dist_list([(_A,_B)]),[dist(_A,_B),dist_list([])])] ? 

yes


Con normalize_frontier queremos:

normalize_frontier([(dist_list([(_A,_B)]),[dist(_A,_B),
                                           dist_list([])])],
		   [(dist_list(_C), [ _C=[(_A,_B)], 
				      dist(_A,_B),
                                      dist_list([])])]
