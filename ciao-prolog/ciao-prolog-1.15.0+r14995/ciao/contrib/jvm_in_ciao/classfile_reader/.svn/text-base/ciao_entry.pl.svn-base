
/* Main entry file for Logen */
/* use ciaoc -S ciao_entry.pl  to create a stand-alone version of Logen */


:- use_module(classfile_reader).

/* ------------------- */
/* main/1: ENTRY POINT */
/* ------------------- */

main(Args) :-
	catch(go(Args),E,
	     (add_exception(ciao_entry, "Toplevel exception: ~n",[],E),
	      halt(1))).

%% Simple bta entry point

go(ArgV) :-
    get_options(ArgV,Opts,[Filename]),
    read_classfile(Filename,Opts).	      		
go(_) :- print_usage.



print_usage :-
	usage(Msg),
	print(user_error,Msg),nl,
    print(user_error,'  Possible Options are:'),nl,
    print_options.           
         %print(user_error,'  Note: the .pl and the . after Atom are optional'),nl.

print_options :- 
   recognised_option(Opt,_,Args,Msg),
   print(user_error,'      '), print(user_error,Opt),
   print_option_args(Args,1),
   print(user_error,': '), print(user_error,Msg),nl,
   fail.    
print_options.

print_option_args([],_).
print_option_args([_|T],N) :- print(user_error,' ARG'),print(user_error,N),
  N1 is N+1, print_option_args(T,N1).


/* ------------------- */

get_options([],[],[]).
get_options([X|T],Options,OtherArgs) :-
   (recognised_option(X,Opt,Values,_) ->       
       ( append(Values, Rest, T), RT = Rest,  /* extract args and insert into Opt by shared var*/
	     Options = [Opt|OT], OtherArgs = AT )   
   ;
       ( Options = OT,     OtherArgs = [X|AT],
	     RT = T )
   ),
   get_options(RT,OT,AT).


usage('Usage: pclass [Options] File.class').
recognised_option('--help',help,[],'Prints this message').
recognised_option('-r',r(on),[],'Resolve references to constants inside bytecode').
recognised_option('-f',f(on),[],'Factorize bytecodes').
recognised_option('-c',c(on),[],'Add class fact').
recognised_option('-o',o(FileName),[FileName],'Output to filename').
%recognised_option('-d',debug_mode,[],'debug mode for GX file').



%recognised_option('-xsb',xsb_mode,[]).
%recognised_option('-t', timeout(T), [T]).
%recognised_option('-errorlog', error_log(File), [File]).


