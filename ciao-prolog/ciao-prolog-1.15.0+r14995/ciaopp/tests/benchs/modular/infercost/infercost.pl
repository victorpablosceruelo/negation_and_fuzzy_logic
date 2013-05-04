
:- module(infercost,
	[ dependency_analysis/7,
	  determinacy_analysis/4,
	  relation_analysis/9,
	  size_analysis/7,
	  solution_analysis/9,
	  time_analysis/9
	],[assertions,regtypes]).
% 	,[]).
%% :- module(infercost,[
%%                   second_order_predicate_pred_arg/2,
%%                   init_buildin_table/1,
%% 	          clause_type/2,
%% 		  insert_symbol_table/4,
%% 		  call_graph/5,
%% 		  strongly_connected_component/2,
%% 		  analysis_check/3,
%% 		  dependency_analysis/7,
%% 		  size_analysis/7,
%% 		  find_symbol_field/4,
%% 		  gen_clause_pos/2,
%% 		  find_adg_field/4,
%% 		  pos_litnum/2,
%% 		  number_of_literals/3,
%% 		  ith_clause_literal/3,
%% 		  ith_body_literal/3,
%% 		  pos_argnum/2,
%% 		  literal_property/5,
%% 		  ith_list_element/3,
%% 		  implicit_output_size/8,
%% 		  explicit_output_size/8,
%% 		  init_normalize_queue/3,
%% 		  normalize/13,
%% 		  simplification/2,
%% 		  print_size/2,
%% 		  relation_analysis/9,
%% 		  print_relation/2,
%% 		  determinacy_analysis/4,
%% 		  solution_analysis/9,
%% 		  print_solution/2,
%% 		  time_analysis/9,
%% 		  print_time/2,
%% 		  find_symbol_entry/3,
%% 		  variable/1,
%% 		  member/2
%% 		 ]).
%%                  % [assertions, basicmodes]).
%% 

:- use_module(library(lists), [length/2	]).
:- use_module(database, 
	[ approximation/1,
%	  loaded/1,
	  % trust_nonfail/5
          db_get/1,
          flag_is_not_fails/1,
          flag_is_possibly_fails/1,
          flag_is_covered/1,
%          flag_is_not_covered/1,
%          flag_set_not_fails/1,
          flag_set_possibly_fails/1,
%          flag_set_covered/1,
          flag_set_not_covered/1
	]).
% messages (must go after database) MH
:- use_module(library(messages)).

% :- initialization(asserta_fact(loaded(infercost))).

:- push_prolog_flag(multi_arity_warnings,off).
:- push_prolog_flag(discontiguous_warnings,off).
:- push_prolog_flag(single_var_warnings,off).

:- include(top(utility)).
%:- include(top(top_)).
:- include(top(output)).
:- include(top(error)).

:- include(init(initsystem)).
:- include(init(builtin)).
:- include(init(symtable)).
:- include(init(dec)).
:- include(init(callgraph)).
:- include(init(scc)).

:- include(size(clause)).
:- include(size(ground_size)).
:- include(size(implicit_size)).
:- include(size(insert_size)).
:- include(size(normalize)).
:- include(size(size_)).
:- include(size(size_diff_equ)).
:- include(size(term_diff)).
:- include(size(term_size)).

:- include(diff_equation(diff_equ)).
:- include(diff_equation(diff_equ_utility)).
:- include(diff_equation(divide_conquer)).
:- include(diff_equation(explicit_size)).
:- include(diff_equation(first_order)).
:- include(diff_equation(higher_order)).
:- include(diff_equation(implicit_size)).
:- include(diff_equation(list_size)).
:- include(diff_equation(mutual_size)).
:- include(diff_equation(product)).
:- include(diff_equation(second_order)).

:- include(dependency(adg)).
:- include(dependency(build_adg)).
:- include(dependency(build_ldg)).
:- include(dependency(dependency_)).
:- include(dependency(gvars)).
:- include(dependency(ldg)).
:- include(dependency(position)).

:- include(algebraic(arithm_opers)).
:- include(algebraic(algebraic_)).
:- include(algebraic(general_form)).
%% :- consult('algebraic/math').
:- include(algebraic(maxmin)).
:- include(algebraic(normal_form)).
:- include(algebraic(sumprod)).

:- include(determinacy(mutual_exclusion)).
:- include(determinacy(determinacy_)).

:- include(solution(relation)).
:- include(solution(solution_)).
:- include(solution(binding)).
:- include(solution(comp_diff_equ)).

:- include(time(time_)).

:- include(csp(constraint)).
:- include(csp(csp_)).
:- include(csp(consistency)).
:- include(csp(clique)).
:- include(csp(unfold)).

:- include(color(disequality)).
:- include(color(gcp)).
:- include(color(cslpoly)).

:- pop_prolog_flag(single_var_warnings).
:- pop_prolog_flag(discontiguous_warnings).
:- pop_prolog_flag(multi_arity_warnings).


%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

