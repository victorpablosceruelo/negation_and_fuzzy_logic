:- module(test_username, _, [rfuzzy, clpr, pkgs_output_debug]).

test('Main', '', '') :- isUserNameLocalUserName('victorpablosceruelo_at_gmail_com').
test('Aux', Comparison, Result) :- isUserNameLocalUserNameAux('victorpablosceruelo_at_gmail_com', Comparison, Result).
test('Fail', 'Fail', 'Fail').

