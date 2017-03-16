% Definition of type translations
% TODO: this should be a module (almost same problem than with spec_arithmetic)

:- '$pragma'(load_gluecode_ttr).

:- '$pragma'(ttr_def(in_int, [
        match       = (int, ground, ground),
	ctype_call  = long,
	ctype_decl  = long,
	check       = ciao_is_integer_s,
	exception   = error_in_arg('INTEGER'),
	to_c        = ciao_to_integer_s ])).

:- '$pragma'(ttr_def(go_int, [
        match       = (int, term, ground),
	ctype_res   = long,
	ctype_call  = pointer(long),
	ctype_decl  = long,
	call_cref   = yes,
	from_c      = ciao_integer_s ])).

:- '$pragma'(ttr_def(in_num, [
        match       = (num, ground, ground),
	ctype_call  = double,
	ctype_decl  = double,
	check       = ciao_is_number_s,
	exception   = error_in_arg('NUMBER'),
	to_c        = ciao_to_float_s ])).

:- '$pragma'(ttr_def(go_num, [
        match       = (num, term, ground),
	ctype_res   = double,
	ctype_call  = pointer(double),
	ctype_decl  = double,
	call_cref   = yes,
	from_c      = ciao_float_s ])).

:- '$pragma'(ttr_def(in_atm, [
        match       = (atm, ground, ground),
	ctype_call  = pointer(char),
	ctype_decl  = pointer(char),
	check       = ciao_is_atom_s,
	exception   = error_in_arg('STRICT_ATOM'),
	to_c        = ciao_atom_name_dup_s,
	free        = ciao_free ])).

:- '$pragma'(ttr_def(go_atm, [
        match       = (atm, term, ground),
	ctype_res   = pointer(char),
	ctype_call  = pointer(pointer(char)),
	ctype_decl  = pointer(char),
	call_cref   = yes,
	from_c      = ciao_atom_s,
	free        = ciao_free ])).

:- '$pragma'(ttr_def(in_string, [
        match       = (string, ground, ground),
	ctype_call  = pointer(char),
	ctype_decl  = pointer(char),
	check       = ciao_is_char_code_list,
	exception   = error_in_arg('CHARACTER_CODE_LIST'),
	to_c        = ciao_list_to_str,
	free        = ciao_free ])).

:- '$pragma'(ttr_def(go_string, [
        match       = (string, term, ground),
	ctype_res   = pointer(char),
	ctype_call  = pointer(pointer(char)),
	ctype_decl  = pointer(char),
	call_cref   = yes,
	from_c      = ciao_str_to_list,
	free        = ciao_free ])).

:- '$pragma'(ttr_def(in_address, [
        match       = (address, ground, ground),
	ctype_call  = pointer(void),
	ctype_decl  = pointer(void),
	check       = ciao_is_address,
	exception   = usage_fault("foreign interface: pointer conversion received ill argument (needed $address/1)"),
	to_c        = ciao_address_to_pointer ])).

:- '$pragma'(ttr_def(go_address, [
        match       = (address, term, ground),
	ctype_res   = pointer(void),
	ctype_call  = pointer(pointer(void)),
	ctype_decl  = pointer(void),
	call_cref   = yes,
	from_c      = ciao_pointer_to_address ])).

:- '$pragma'(ttr_def(in_byte_list, [
        match       = (byte_list, ground, ground),
	ctype_call  = pointer(char),
	ctype_decl  = pointer(char),
	check       = ciao_is_char_code_list,
	exception   = usage_fault("foreign interface: list length or data inconsistency."),
	to_c        = ciao_list_to_byte_array,
	compound    = yes ])).

:- '$pragma'(ttr_def(go_byte_list, [
        match       = (byte_list, term, ground),
	ctype_res   = pointer(char),
	ctype_call  = pointer(pointer(char)),
	ctype_decl  = pointer(char),
	call_cref   = yes,
	from_c      = ciao_byte_listn,
	compound    = yes ])).

:- '$pragma'(ttr_def(in_int_list, [
        match       = (int_list, ground, ground),
	ctype_call  = pointer(long),
	ctype_decl  = pointer(long),
	check       = ciao_is_int_list,
	exception   = usage_fault("foreign interface: list length or data inconsistency."),
	to_c        = ciao_list_to_int_array,
	compound    = yes ])).

:- '$pragma'(ttr_def(go_int_list, [
        match       = (int_list, term, ground),
	ctype_res   = pointer(long),
	ctype_call  = pointer(pointer(long)),
	ctype_decl  = pointer(long),
	call_cref   = yes,
	from_c      = ciao_int_listn,
	compound    = yes ])).

:- '$pragma'(ttr_def(go_any_term, [
        match       = (any_term, term, ground),
	ctype_res   = ciao_term,
	ctype_call  = pointer(ciao_term),
	ctype_decl  = ciao_term,
	call_cref   = yes,
	from_c      = '=' ])).
