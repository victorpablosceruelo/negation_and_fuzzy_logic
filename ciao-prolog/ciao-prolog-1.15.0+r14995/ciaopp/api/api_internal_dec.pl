:- package(api_internal_dec).
% :- doc(filetype,user).

:- use_package( argnames ).

% TODO: Do not confuse with 'comment' assertions. This is a syntactic comment (e.g. "% ...")
:- argnames comment( where , type , comment ).

:- argnames loc( file , line_begin , line_end , module ).



:- argnames as( ref , status , type , head , compat , 
	        call , succ , comp , orig_call, orig_succ, 
                dic , locator , comment , fromwhere ).

:- argnames as_commented( ref , status , type , head , compat , 
                          call , succ ,  comp , dic ).

% fromwhere = read , asserted

:- argnames lit( id , key , goal , type , locator ).

:- argnames cls( ref , id , key , head , body , dic , locator ).

:- argnames direc( key , ref , body , dic , locator ).


:- argnames pred_type_selector( internal     ,
	                        imported     ,
				reexported   ,
				exported     ,
				metapred     ,
				dynamic      ,
				data         ,
				concurrent   ,
				multifile    ,
				impl_defines ).


% :- regtype as( Ref , Status , Type , Head , Compat , Call , Succ , OrigCall, OrigSucc, Comp , Dic , 
% 	        Locator , Comment , Fromwhere )
% #
% "
% @var{Ref} is internal abstract type (used to delete or consult in DB).
% @var{Status} is the assertion status (check, checkd, true, false...).
% @var{Type} is the assertion type (call,success,entry,exi).
% @var{Call} is the call field.
% @var{Succ} is the success field.
% @var{OrigCall} is the original call field.
% @var{OrigSucc} is the original success field.
% @var{Comp} is the computation field.
% @var{Dic}  is the assertion dictionary.
% @var{locator} is the assertion locator.
% @var{comment} is the assertion comment.
% @var{fromwhere}. Ignore this field (can take the values: read,
% asserted, commented), but maybe will dissapear in future
% implementations.  ".

% :- regtype cls/7.

% cls( _A1 , _A2 , _A3 , _A4 , _A5 , _A6 , _A7 ).
