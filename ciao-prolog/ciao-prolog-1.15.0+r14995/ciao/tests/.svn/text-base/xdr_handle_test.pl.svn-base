:- module(xdr_handle_test, [main/0], [assertions]).

:- use_module(library(xdr_handle)).
:- use_module(library(format)).

:- test main.

main :-
	line,
	format("Retrieving XDR document from URL:~n", []),
	format("http://clip.dia.fi.upm.es/Tests/xml_files/xdrdocs/fish2.xdr~n",
	    []),
	format("............~n~n", []),
	xdr_tree('http://clip.dia.fi.upm.es/Tests/xml_files/xdrdocs/fish2.xdr',
	    XDRTree, _),
	format("XDR document transformed into a Prolog term.~n~n",    []),
	format("Transforming XDR tree into equivalent HTML code. ~n", []),
	format("............~n~n",                                    []),
	xdr2html(XDRTree, _HTML),
	format("XDR tree successuflly transformed into HTML form~n",
	    []),
	format(
	    "(form expected to be used to generate XML instances of the XDR)~n",
	    []),
	!.

line:-
	nl,
	display(
'***************************************************************************'),
	nl.
