:- module(flags_parser,[get_public_flag/2,get_private_flag/2,get_protected_flag/2,
	                get_static_flag/2,get_final_flag/2,get_synchronized_flag/2,
			get_native_flag/2,get_abstract_flag/2,get_strict_flag/2,
			get_volatile_flag/2,get_transient_flag/2,get_super_flag/2,
			get_interface_flag/2]).

get_public_flag(Flags,true) :- (Flags /\ 0x0001) =\= 0,!.
get_public_flag(_,false).

get_protected_flag(Flags,true) :- (Flags /\ 0x0004) =\= 0,!.
get_protected_flag(_,false).

get_final_flag(Flags,true) :- (Flags /\ 0x0010) =\= 0,!.
get_final_flag(_,false).
	
get_abstract_flag(Flags,true) :- (Flags /\ 0x0400) =\= 0,!.
get_abstract_flag(_,false).	

get_static_flag(Flags,true) :- (Flags /\ 0x0008) =\= 0,!.
get_static_flag(_,false).	

get_private_flag(Flags,true) :- (Flags /\ 0x0002) =\= 0,!.
get_private_flag(_,false).	

get_synchronized_flag(Flags,true) :- (Flags /\ 0x0020) =\= 0,!.
get_synchronized_flag(_,false).

get_super_flag(Flags,true) :- (Flags /\ 0x0020) =\= 0,!.
get_super_flag(_,false).

get_interface_flag(Flags,true) :- (Flags /\ 0x0200) =\= 0,!.
get_interface_flag(_,false).

get_volatile_flag(Flags,true) :- (Flags /\ 0x0040) =\= 0,!.
get_volatile_flag(_,false).

get_transient_flag(Flags,true) :- (Flags /\ 0x0080) =\= 0,!.
get_transient_flag(_,false).

get_native_flag(Flags,true) :- (Flags /\ 0x0100) =\= 0,!.
get_native_flag(_,false).

get_strict_flag(Flags,true) :- (Flags /\ 0x0800) =\= 0,!.
get_strict_flag(_,false).

