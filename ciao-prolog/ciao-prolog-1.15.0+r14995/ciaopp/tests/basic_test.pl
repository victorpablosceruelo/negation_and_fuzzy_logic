:- module( basic_test , [
	                 atom_concat_list/2,
			 build_dump_name/6,
			 build_spec_name/6,
			 build_source_name/3,
			 combine/7
			] , [] ).

%% atom_concat_list
%% given a list of atom, atom_concat_list concatenates them into one atom.
atom_concat_list( [A] , A ) :- 
	!.

atom_concat_list( [A|R] , Out ) :-
	!,
	atom_concat_list( R , Rout ),
	atom_concat( A , Rout , Out ).


% example: build_dump_name( 'my_dump_store_dir/' , my_file , ana1, ana2, ana3, X ).
% X = 'my_dump_store_dir/my_file_ana1_ana2_ana3.dump

build_dump_name( Storage, File , Fixpoint , AbsInt , Dump_Level ,  Dump_Name ) :-
	atom_concat_list( [
                           Storage    , 
	                   File       , '_' , 
	                   Fixpoint   , '_' ,
			   AbsInt     , '_' ,
			   Dump_Level , '.dump' 
			  ] ,
			  Dump_Name ).


build_spec_name( Storage, File , Fixpoint , AbsInt , Spec_Level ,  Spec_Name ) :-
	atom_concat_list( [
                           Storage    , 
	                   File       , '_' , 
	                   Fixpoint   , '_' ,
			   AbsInt     , '_' ,
			   Spec_Level , '.spec' 
			  ] ,
			  Spec_Name ).



build_source_name( Dir , F , Source_Name ) :-
	atom_concat_list( [
			   Dir,
			   F
			  ],
			  Source_Name ).


combine( [A] , B , C , D , E , F , [(A,B,C,D,E,F)] ) :- 
	!.

combine( [A|R] , B , C , D , E , F , [(A,B,C,D,E,F)|RR] ) :-
	combine( R , B , C , D , E , F , RR ).
