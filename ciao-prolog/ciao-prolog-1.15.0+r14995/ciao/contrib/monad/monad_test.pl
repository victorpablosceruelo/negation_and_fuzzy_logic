:- module(monad_test, _, [monad/*, hmtypes_check*/]).

:- use_module(library(lists), [append/3]).

% Note: since Prolog is not typed, all monad operations has been
% extended with an identifer for the monad name.

% Todo:
%   - read this manual http://en.wikibooks.org/wiki/Programming:Haskell_monads

% :- type err_monad(M) ---> err(string) ; val(M).

% ---------------------------------------------------------------------------
% Identity monad
% Computation type: Simple function application

idm(V) >>= P := S :- S = ~P(V).
return(idm, A) := idm(A).

% ---------------------------------------------------------------------------
% Maybe monad
% Computation type: Computations which may return Nothing

% (how to extract the result from a monadic function)
maybe_nothing >>= _ := maybe_nothing.
maybe_just(V) >>= P := S :- S = ~P(V).
% (how a monadic function returns a result)
return(maybe, A) := maybe_just(A).
% (how to fail a monadic function)
fail(maybe, _String) := maybe_nothing.
mzero(maybe) := maybe_nothing.
mplus(maybe, A, B) := C :- A = maybe_nothing, !, C = B.
mplus(maybe, A, _) := C :- C = A.

% ---------------------------------------------------------------------------
% Error monad
% todo: this monad is not correctly implemented (see commented text bellow)
% Computation type: Computations which may fail or throw exceptions

errm_err(String) >>= _ := errm_err(String).
errm_val(V) >>= P := S :- S = ~P(V).
return(errm, A) := errm_val(A).
fail(errm, String) := errm_err(String).

/* TODO: Port this haskell example of error monad!
% This is the type of our parse error representation.
% data ParseError = Err {location::Int, reason::String}

% We make it an instance of the Error class
% instance Error ParseError where
%   noMsg    = Err 0 "Parse Error"
%   strMsg s = Err 0 s

noMsg := err(0, "parse error").
strMsg(S) := err(0, S).

% -- For our monad type constructor, we use Either ParseError
% -- which represents failure using Left ParseError or a
% -- successful result of type a using Right a.
% type ParseMonad = Either ParseError

% -- parseHexDigit attempts to convert a single hex digit into
% -- an Integer in the ParseMonad monad and throws an error on an
% -- invalid character
% parseHexDigit :: Char -> Int -> ParseMonad Integer
% parseHexDigit c idx = if isHexDigit c then
%                         return (toInteger (digitToInt c))
%                       else
%                         throwError (Err idx ("Invalid character '" ++ [c] ++ "'"))
parseHexDigit(C, Idx) := R :-
	( isHexDigit(C) ->
	    R = return(C)
	; R = throwError(err(Idx, ~sformat("Invalid character %s", tointeger(digitToIntc idx = if isHexDigit c then
                        return (toInteger (digitToInt c))
                      else
                        throwError (Err idx ("Invalid character '" ++ [c] ++ "'"))

-- parseHex parses a string containing a hexadecimal number into
-- an Integer in the ParseMonad monad.  A parse error from parseHexDigit
-- will cause an exceptional return from parseHex.
parseHex :: String -> ParseMonad Integer
parseHex s = parseHex' s 0 1
  where parseHex' []      val _   = return val
        parseHex' (c:cs)  val idx = do d <- parseHexDigit c idx
                                       parseHex' cs ((val * 16) + d) (idx + 1)

-- toString converts an Integer into a String in the ParseMonad monad
toString :: Integer -> ParseMonad String
toString n = return $ show n

-- convert takes a String containing a hexadecimal representation of
-- a number to a String containing a decimal representation of that
-- number.  A parse error on the input String will generate a
-- descriptive error message as the output String.
convert :: String -> String
convert s = let (Right str) = do {n <- parseHex s; toString n} `catchError` printError
            in str
  where printError e = return $ "At index " ++ (show (location e)) ++ ":" ++ (reason e)
*/

% ---------------------------------------------------------------------------
% List monad
% Computation type: Computations which may return 0, 1, or more possible results.

% instance Monad [] where
%     m >>= f  = concatMap f m
%     return x = [x]
%     fail s   = []
%     
% instance MonadPlus [] where
%     mzero = []
%     mplus = (++)

return(list, X) := [X].
fail(list, _String) := [].
Xs >>= F := ~concat_map(F, Xs) :- ( Xs = [] ; Xs = [_|_] ).
mzero(list) := [].
mplus(list, A, B) := ~append(A, B).

% todo: use :- index concat_map(?, +). ?

:- meta_predicate concat_map(pred(2), ?).
concat_map(F, Xs) := ~concat_map__2(Xs, F).
:- meta_predicate concat_map__2(?, pred(2)).
concat_map__2([], _) := [].
concat_map__2([X|Xs], F) := ~append(~F(X), ~concat_map__2(Xs, F)).

% ---------------------------------------------------------------------------
% State monad
% Computation type: Computations which maintain state.

% data SM a = SM (S -> (a,S))  -- The monadic type
%
% instance Monad SM where
%  -- defines state propagation
%  SM c1 >>= fc2         =  SM (\s0 -> let (r,s1) = c1 s0 
%                                          SM c2 = fc2 r in
%                                         c2 s1)
%  return k              =  SM (\s -> (k,s))
%
% -- extracts the state from the monad
%readSM                  :: SM S
%readSM                  =  SM (\s -> (s,s))
%
% -- updates the state of the monad
%updateSM                :: (S -> S) -> SM ()  -- alters the state
%updateSM f              =  SM (\s -> ((), f s)) 
%
%-- run a computation in the SM monad
%runSM                   :: S -> SM a -> (a,S)
%runSM s0 (SM c)         =  c s0

% (how to extract the result from a monadic function)
sm(C1) >>= FC2 := sm(SMF) :-
	SMF = (''(S0) := ~smf(S0, C1, FC2)). % todo: split to avoid a bug
smf(S0, C1, FC2) := ~C2(S1) :- (R, S1) = ~C1(S0), sm(C2) = ~FC2(R).
% (how a monadic function returns a result)
return(sm, K) := sm(SID) :-
	SID = (''(S) := (K, S)).

readSM(sm) := sm(SR) :-
	SR = (''(S) := (S, S)).

updateSM(sm, F) := sm(SUP) :-
	SUP = (''(S) := ('()', ~F(S))).

runSM(S0, sm(C)) := ~C(S0).

% ---------------------------------------------------------------------------
% Reader monad
% Computation type: Computations which read values from a shared environment.

% instance Monad (Reader e) where 
%     return a         = Reader $ \e -> a 
%     (Reader r) >>= f = Reader $ \e -> f (r e) e 
% 
% class MonadReader e m | m -> e where 
%     ask   :: m e
%     local :: (e -> e) -> m a -> m a 
%  
% instance MonadReader (Reader e) where 
%     ask       = Reader id 
%     local f c = Reader $ \e -> runReader c (f e) 
%  
% asks :: (MonadReader e m) => (e -> a) -> m a 
% asks sel = ask >>= return . sel

return(reader, A) := readerm((''(_E) := A)).
readerm(R) >>= F := readerm((''(E) := ~reader_apply(~F(~R(E)), E))).

ask(reader) := readerm((''(X) := X)).
local(reader, F, C) := readerm((''(E) := ~runReader(C, ~F(E)))).

asks(reader, Sel) := do(reader, [X<-ask, return(~Sel(X))]).

runReader(readerm(C), E) := ~C(E).

% todo: missing metatype info
reader_apply(readerm(EA), E) := ~EA(E).

% ---------------------------------------------------------------------------
% todo: are list comprehensions more powerful than bagof/findall?
% todo: Prolog can iterate and share variables (although in a strange way...)
%
% test(A, B) :- findall(A-C, foo(A, C), B0), maplist(bind(A),B0,B).
% bind(X,Y-B,B) :- var(Y) -> X=Y ; true.

% ---------------------------------------------------------------------------
% todo: Transformer version of monads, with lift operations

% ---------------------------------------------------------------------------

:- export(eval/2).
eval(sum(A, B)) := ~eval(A) + ~eval(B).
eval(C) := C :- integer(C).

% todo: do notation with a functional twist? (Av <- ffff ===> (Tv <- ffff, Av = T) === Av = ~ffff)
%   (the monad value cannot be accessed...)

% Monadic version of eval
% (it can be used with different monads)	
:- export(evalm/3).
%err_eval(^(A+B)) := (~err_eval(A) >>= (''(Av, S0) :- S0 = (~err_eval(B) >>= (''(Bv, S1) :- S1 = return(Av + Bv))))).
evalm(M, C) := return(M, C) :- integer(C), !.
evalm(M, C) := fail(M, "atom") :- atom(C), !.
%evalm(M, sum(A,B)) := ~evalm(M, A) >>= (''(Av) := ~evalm(M, B) >>= (''(Bv) := return(M, Av + Bv))).
evalm(M, sum(A,B)) := do(M, [Av<-evalm(A), Bv<-evalm(B), return(Av + Bv)]).

% accumulate and return
accum(M, V) := do(M, [updateSM((''(List) := [V|List])), return(V)]).

% Monadic version of eval that accumulates results
% (it can be used with different monads)	
:- export(evalma/3).
evalma(M, C) := ~accum(M, C) :- integer(C), !.
evalma(M, C) := fail(M, "atom") :- atom(C), !.
evalma(M, sum(A,B)) := do(M, [Av<-evalma(A), Bv<-evalma(B), accum(Av + Bv)]).

% Pending: monad transformers
% http://en.wikibooks.org/wiki/Haskell/Monad_transformers

% Monadic functions:
% http://members.chello.nl/hjgtuyl/tourdemonad.html

% todo: index on second arg
%sequence_(P, Xs) := ~sequence___2(Xs, P).
%sequence___2([], _) := return('').
%sequence___2([X|Xs], P) := ~P(X) >>= (''(_) := ~sequence___2(Xs, P)).

% http://www.haskell.org/all_about_monads/html/index.html

% err_eval(^(A+B)) := do {Av <- err_eval(A), Bv <- err_eval(B), return Av + Bv}.

% Test for the list monad
mrange(M, I) := ~mrange__2(M, 0, I).
mrange__2(M, I, N) := ~mplus(M, return(M, I), ~mrange__2(M, I + 1, N)) :- I < N, !.
mrange__2(M, _, _) := ~mzero(M).

test_list_monad :-
	% todo: Lists are monads and the monadic 'do' notation is an instance of list comprehensions!
        % From: [http://www.haskell.org/haskellwiki/List_comprehension]
        % In the first versions of Haskell, the comprehension syntax
        % was available for all monads. (See History of Haskell) Later
        % the comprehension syntax was restricted to lists. Since
        % lists are an instance of monads, you can get list
        % comprehension in terms of the do notation. Because of this,
        % several Haskell programmers consider the list comprehension
        % unnecessary now.
	X = do(list, [A<-mrange(2), B<-mrange(3), return(pair(A,B))]),
%	X = do(list, [A<-mplus(return(list,1),return(list,2)),B<-mplus(return(list,1),return(list,2)),return((A,B))]),
	display('List monad'), nl,
	display('  Expected: test_list_monad([pair(0,0),pair(0,1),pair(0,2),pair(1,0),pair(1,1),pair(1,2)])'), nl,
	display('  Result:   '), display(test_list_monad(X)), nl.

test_state_monad :-
	S5 = ~runSM(0, ~evalm(sm, sum(1, sum(2,3)))),
	display('  Expected: ,(6,0)'), nl,
	display('  Result:   '), display(S5), nl,
	S6 = ~runSM([], ~evalma(sm, sum(1, sum(2,3)))),
	display('  Expected: ,(6,[6,5,3,2,1])'), nl,
	display('  Result:   '), display(S6), nl.

test_reader_monad :-
	X = ~runReader(do(reader, [
          Y<-ask,
	  Z<-local((''(Old) := Old*10), do(reader, [Z0<-ask, return(Z0)])),
	  return(Z+Y)
        ]), 1),
	display('Reader monad'), nl,
	display('  Expected: 11'), nl,
	display('  Result    '), display(X), nl.

test_monad(Monad, Expr) :-
	display('  '), display(Monad), display(': expr '), 
	display('  '), display(Expr), display(' => '),
	( S0 = ~evalm(Monad, Expr) ->
	    display(S0)
	; display('(failure)')
	),
	nl.

test :-
	display('Identity monad'), nl,
	test_monad(idm, sum(1, sum(2,3))),
	test_monad(idm, sum(1, sum(a,3))),
	display('Error monad'), nl,
	test_monad(errm, sum(1, sum(2,3))),
	test_monad(errm, sum(1, sum(a,3))),
	display('Maybe monad'), nl,
	test_monad(maybe, sum(1, sum(2,3))),
	test_monad(maybe, sum(1, sum(a,3))),
	display('State monad'), nl,
	test_state_monad,
	test_list_monad,
	test_reader_monad.

