:- module(descriptors_grammar,[parse_method_descriptor/3,parse_field_descriptor/2],[dcg]).

:- use_module(library(filenames)).


parse_method_descriptor(P,R,D) :- 
	atom_codes(D,SD),
	parse_method_descriptor_(P,R,SD,"").

parse_field_descriptor(T,D) :- 
	atom_codes(D,SD),
	parse_field_descriptor_(T,SD,"").

parse_field_descriptor_(F) --> parse_field_type(F).

parse_method_descriptor_(Params,Ret) --> 
	"(", parse_params(Params),")",parse_return(Ret).

parse_params([]) --> "".
parse_params([F|Ps]) --> parse_field_type(F),parse_params(Ps).

parse_field_type(F) --> parse_base_type(F),!.
parse_field_type(F) --> parse_object_type(F),!.
parse_field_type(F) --> parse_array_type(F),!.

parse_base_type(primitiveType(byte)) --> "B".
parse_base_type(primitiveType(char)) --> "C".
parse_base_type(primitiveType(double)) --> "D".
parse_base_type(primitiveType(float)) --> "F".
parse_base_type(primitiveType(int)) --> "I".
parse_base_type(primitiveType(long)) --> "J".
parse_base_type(primitiveType(short)) --> "S".
parse_base_type(primitiveType(boolean)) --> "Z".

parse_object_type(refType(classType(className(packageName(Package),
	                                      shortClassName(ClassOnly))))) --> 
	"L",parse_class(Class),";",
	{atom_codes(ClassAtom,Class),
	no_path_file_name(ClassAtom,ClassOnly),
	atom_concat(Package,ClassOnly,ClassAtom)}.

parse_array_type(refType(arrayType(F))) --> "[",parse_field_type(F).

parse_class([C]) --> [C].
parse_class([C|R]) --> [C],parse_class(R).

parse_return(none) --> "V".
parse_return(F) --> parse_field_type(F).
