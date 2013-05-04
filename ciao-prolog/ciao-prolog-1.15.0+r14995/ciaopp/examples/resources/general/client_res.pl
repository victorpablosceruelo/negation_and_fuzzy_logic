:- module(_,
	    [head_bits_received/2, lit_bits_received/2],
	    [assertions, regtypes, library(resdefs(resources_decl))]).

% head_bits_received(LitInfo, inf ) :-  
% 	litinfo_get_lit( LitInfo, Head ),
% 	Head = 'client:exchange_byte'( _,_,_),
% 	!.
head_bits_received(_LitInfo, 0).

lit_bits_received(_LitInfo, 0).
