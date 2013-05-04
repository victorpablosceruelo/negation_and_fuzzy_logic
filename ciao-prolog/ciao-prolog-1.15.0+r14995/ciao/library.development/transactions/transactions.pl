:- package(transactions).
:- use_module(transaction_logging).
:- use_module(transaction_recovery).
:- use_module(transaction_concurrency).

:- set_prolog_flag(multi_arity_warnings, off).
:- load_compilation_module(transactionstr).

% TODO: Right priority? (applied after persdb)
:- add_sentence_trans(transaction_expander_tr/3, 1150).
:- add_sentence_trans(transaction_decl_expander_tr/2, 1150).   

:- add_term_trans(data_decl_expander_tr/2, 1150).  
:- add_term_trans(data_goal_expander_tr/4, 1150).      
:- add_term_trans(asserta_expander_tr/4, 1150).     
:- add_term_trans(assertz_expander_tr/4, 1150).     
:- add_term_trans(retract_expander_tr/4, 1150).     
:- add_term_trans(abort_expander_tr/4, 1150).    
