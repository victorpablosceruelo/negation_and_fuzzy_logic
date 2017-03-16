:- module(init, [], []).

:- reexport(infercost(init(gran_init)), [cls_init_gran_system/10]).
:- reexport(infercost(init(builtin)),
	[
	    legal_pred_arg/1,
	    second_order_predicate/1,
	    second_order_predicate_pred_arg/2,
	    second_order_predicate_pred_num/3
	]).
:- reexport(infercost(init(dec)), [analysis_check/3]).
:- reexport(infercost(init(initsystem_basic)), [clause_type/2]).
:- reexport(infercost(init(symtable)),
	[
	    find_symbol_field/4,
	    find_symbol_field_clause/3,
	    find_symbol_entry/3,
	    insert_symbol_field/4,
	    literal_property/5,
	    % For granularity control
	    get_input_arglist_from_st/3
	]).
