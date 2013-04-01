:- module(opt_cnegf,_,[]).
 
:- load_compilation_module('/home/clip/ciaopp-0.8p24/src/topcpaths').
%('/home/susana/src/Ciao/ciaopp-0.8p24/src/topcpaths').
:- use_module(ciaopp(m_ciaopp),[precompile/2]).

% Optimization of the negation calls of a list of programs Files.
main([]).
main([File|Files]):-
	neg_optimize(File,_File_out),
	main(Files).

% Optimization of the negation calls of a list of a program File.
neg_optimize(File,_File_out):-
	m_ciaopp:precompile(File,[none,none,typesfd,shfr,none,none,none,
                                  nf,none,none,none,none,yes]),
	m_ciaopp:precompile(File,[none,none,typesfd,shfr,none,none,upper,
                                  nf,none,none,none,none,yes]).
	m_ciaopp:precompile(File,[none,none,none,none,none,none,none,
                                  none,none,none,none,none,yes]).
