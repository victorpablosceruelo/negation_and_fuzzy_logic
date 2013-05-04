:- module(_, [], [compiler(complang), compiler(callback)]).

:- use_module(library(assertions(assertions_props))).
:- use_module(compiler(assertions__common)).

:- public normalize_if_assertion_body/4.
:- pred normalize_if_assertion_body(
   Ass,AssrtStatus,AssrtType,NBodyAndHead)
:: assrt_body * assrt_status * assrt_type * assrt_body

# "The assertion-related declaration @var{U} is in canonical format in
   @var{N}.".

normalize_if_assertion_body(Ass,AssrtStatus,AssrtType,NBodyAndHead) :-
	normalize_status_and_type(Ass,AssrtStatus,AssrtType,UBody),
	%% At this point it has been recognized as an assertion...
        normalize_assertion_body(AssrtType,UBody,NBodyAndHead).

normalize_assertion_body(AssrtType,UBody,NBodyAndHead) :-
	norm_body(UBody,Format,NBody),
	assertion_format(AssrtType,Format), % Do not put before norm_body!
	!,
	assertion_body(PD,DP,CP,AP,GP,CO,NBody),
	%% Put all heads  in f(var,..,var) form
	(  PD = F/A 
	-> functor(NPD,F,A)
	;  NPD=PD ),
	assertion_body(NPD,DP,CP,AP,GP,CO,NBodyAndHead).
normalize_assertion_body(AssrtType,UBody,_NBodyAndHead) :-
 	add_module_error(assertion_syntax(AssrtType, UBody)),
	fail.
	
%% ---------------------------------------------------------------------------
:- pred normalize_status_and_type(
  +assrt_body,go(assrt_status),go(assrt_type),go(assrt_body)).

normalize_status_and_type(Ass,AssrtStatus,AssrtType,UBody) :- 
	Ass  =.. [AssrtType,UBody],
	assrt_type(AssrtType),
	default_assrt_status(AssrtType,AssrtStatus),
	!.
normalize_status_and_type(Ass,AssrtStatus,AssrtType,UBody) :- 
	Ass  =.. [AssrtType,AssrtStatus,UBody],
	assrt_type(AssrtType),
	nonvar(AssrtStatus),
	assrt_status(AssrtStatus),
	!.
normalize_status_and_type(Ass,AssrtStatus,AssrtType,UBody) :- 
	Ass  =.. [AssrtType,AssrtStatus,UBody],
	assrt_type(AssrtType),
	var(AssrtStatus),
	default_assrt_status(AssrtType,AssrtStatus),
	!.

%% ---------------------------------------------------------------------------
:- pred default_assrt_status(+assrt_type,-assrt_status) 
# "Defines the status to be used for a given assertion type, if an
   assertion status is not specified explicitly.".

default_assrt_status(entry,   true ) :- !. %% ???
default_assrt_status(modedef, true ) :- !. %% ???
default_assrt_status(X,       check) :-
	assrt_type(X),
	!.

%% ---------------------------------------------------------------------------
:- pred assertion_format(assrt_type(AssrtType),assrt_format_code(Code)) 
# "@var{Code} describes an admissible format in which assertions of
   the class @var{AssrtType} can be written.".

%% Admissible assertion formats:
assertion_format(pred,    X) :- assrt_format_code(X).
assertion_format(decl,    X) :- assrt_format_code(X). %% ?
assertion_format(prop,    X) :- assrt_format_code(X). 
%% Obsolete: delete eventually...
%% assertion_format(type,    t).
%% Not needed any more...
%%assertion_format(type,    g). %% Added for now to put typedef there...
%% assertion_format(compat,  d). %% Not using these as basic any more?!
assertion_format(calls,   c).
assertion_format(success, s).
assertion_format(comp,    g).
%% These to become obsolete?
assertion_format(entry,   c).
assertion_format(entry,   t).
%% Not an assertion any more, but a status instead
%% assertion_format(trust,   X) :- assrt_format_code(X).
assertion_format(modedef, X) :- assrt_format_code(X).

:- prop assrt_format_code(X) + regtype
   # "@var{X} is a designator for an assertion format.".

assrt_format_code(p).
assrt_format_code(d).
assrt_format_code(c).
assrt_format_code(s).
assrt_format_code(g).
assrt_format_code(t).

%% ---------------------------------------------------------------------------
:- pred norm_body(B,_,NB) 
   # "@var{NB} is a normalized assertion body corresponding to the
     unnomalized assertion body @var{B}.".

%% MH: No comments allowed now in basic assertions (difficult to document).

% ------------ A  B   C  D  E --FormatId--------------------------- %ABCDE
norm_body((PD::DP:CP=>AP+GP#CO),p,(PD::DP  :CP  =>AP  +GP  #CO)):-!.%11111
norm_body((PD::DP:CP=>AP+GP   ),p,(PD::DP  :CP  =>AP  +GP  #"")):-!.%11110
norm_body((PD::DP:CP=>AP   #CO),p,(PD::DP  :CP  =>AP  +true#CO)):-!.%11101
norm_body((PD::DP:CP=>AP      ),p,(PD::DP  :CP  =>AP  +true#"")):-!.%11100
norm_body((PD::DP:CP    +GP#CO),p,(PD::DP  :CP  =>true+GP  #CO)):-!.%11011
norm_body((PD::DP:CP    +GP   ),p,(PD::DP  :CP  =>true+GP  #"")):-!.%11010
norm_body((PD::DP:CP       #CO),p,(PD::DP  :CP  =>true+true#CO)):-!.%11001
norm_body((PD::DP:CP          ),p,(PD::DP  :CP  =>true+true#"")):-!.%11000
norm_body((PD::DP   =>AP+GP#CO),p,(PD::DP  :true=>AP  +GP  #CO)):-!.%10111
norm_body((PD::DP   =>AP+GP   ),p,(PD::DP  :true=>AP  +GP  #"")):-!.%10110
norm_body((PD::DP   =>AP   #CO),p,(PD::DP  :true=>AP  +true#CO)):-!.%10101
norm_body((PD::DP   =>AP      ),p,(PD::DP  :true=>AP  +true#"")):-!.%10100
norm_body((PD::DP       +GP#CO),p,(PD::DP  :true=>true+GP  #CO)):-!.%10011
norm_body((PD::DP       +GP   ),p,(PD::DP  :true=>true+GP  #"")):-!.%10010
norm_body((PD::DP          #CO),p,(PD::DP  :true=>true+true#CO)):-!.%10001
norm_body((PD::DP             ),d,(PD::DP  :true=>true+true#"")):-!.%10000
norm_body((PD    :CP=>AP+GP#CO),p,(PD::true:CP  =>AP  +GP  #CO)):-!.%01111
norm_body((PD    :CP=>AP+GP   ),p,(PD::true:CP  =>AP  +GP  #"")):-!.%01110
norm_body((PD    :CP=>AP   #CO),s,(PD::true:CP  =>AP  +true#CO)):-!.%01101
norm_body((PD    :CP=>AP      ),s,(PD::true:CP  =>AP  +true#"")):-!.%01100
norm_body((PD    :CP    +GP#CO),p,(PD::true:CP  =>true+GP  #CO)):-!.%01011
norm_body((PD    :CP    +GP   ),g,(PD::true:CP  =>true+GP  #"")):-!.%01010
norm_body((PD    :CP       #CO),p,(PD::true:CP  =>true+true#CO)):-!.%01001
norm_body((PD    :CP          ),c,(PD::true:CP  =>true+true#"")):-!.%01000
norm_body((PD       =>AP+GP#CO),p,(PD::true:true=>AP  +GP  #CO)):-!.%00111
norm_body((PD       =>AP+GP   ),p,(PD::true:true=>AP  +GP  #"")):-!.%00110
norm_body((PD       =>AP   #CO),p,(PD::true:true=>AP  +true#CO)):-!.%00101
norm_body((PD       =>AP      ),s,(PD::true:true=>AP  +true#"")):-!.%00100
norm_body((PD           +GP#CO),p,(PD::true:true=>true+GP  #CO)):-!.%00011
norm_body((PD           +GP   ),g,(PD::true:true=>true+GP  #"")):-!.%00010
norm_body((PD              #CO),p,(PD::true:true=>true+true#CO)):-!.%00001
norm_body((PD                 ),t,(PD::true:true=>true+true#"")):-!.%00000
