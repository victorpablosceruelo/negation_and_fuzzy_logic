:- module(optim_comp,[optim_comp/1],[api(ciaopp_api)]).


:- use_module(ciaopp(preprocess_flags)).
:- use_module(ciaopp(printer), [output/2]).
% :- use_module(api(api_internal_mod), 
% 	[add_package_to_output/1,
% 	 get_output_name/2]).

:- use_module(library(system), [mktemp/2, system/1]).

optim_comp(Comp_Mode):-
 	get_output_name( _OptFile , BaseFile ),
	atom_concat(BaseFile, 'XXXXXX', Template),
	mktemp(Template, TmpFile),
	push_pp_flag(low_level_format,on),
%	push_pp_flag(pp_info,on),
	push_pp_flag(pp_info,off),
	add_package_to_output(nativeprops),
	add_compilation_mode(Comp_Mode),
	output(TmpFile, []),
	pop_pp_flag(low_level_format),
	pop_pp_flag(pp_info),
	atom_concat(' ', TmpFile, E),
	atom_concat(BaseFile, E, E1),
	atom_concat('ciaotool try --bootstrap ', E1, Command),
	system(Command).

add_compilation_mode(byte_code):-
	add_package_to_output(optim_byte_code).
add_compilation_mode(c_code):-
	add_package_to_output(optim_c_code_spec).

	
	
