/*  COMPILE.PL  */


/*
    This and its associated files (except for the two Edinburgh Tools
    libraries) were written by Jocelyn Paine.
    Use them as you wish, provided that you retain this acknowledgement.
*/


/*
Demonstration compiler.
-----------------------

This is the master file for a demonstration compiler written in GRIPS.

It compiles programs written in a (very small) subset of Pascal. For
the syntax (which should also make the semantics obvious) see PARSER.PL.
An example program is in PROGRAM. The compiler generates stack-code for
a simple virtual machine. This can then be run by an interpreter.

The compiler consists of:
1.  A lexical analyser:                         LEX.PL.
2.  A parser:                                   PARSER.PL.
3.  A code-generator:                           CODE_GENERATE.PL.
4.  A loader (which fixes up label references): LOAD.PL.
5.  An interpreter:                             MACHINE.PL.

There are three auxiliary files: PLUSPLUS.PL, ORDSET.PL,
and MAP.PL. The latter two are from the Edinburgh Tools library, and 
define a representation for sets and mappings. The first defines 
a useful operator.

As far as possible, I have written the compiler in a functional style
(but without using higher-order functions). The functions model concepts
familiar in theoretical semantics, such as environments: I have used
the maps library here to represent some of the functions (such as the
store). I originally wrote this demonstration for a mathematician
who was starting a computer science MSc, to give him a simple model, in
his idiom, of how compilers work and what they do. Thank you Hendrik
for giving me something big to demonstrate GRIPS on.
*/


:- ['plusplus.pl'].
:- ['ordset.pl'].
:- ['map.pl'].
:- ['lex.pl'].
:- ['parser.pl'].
:- ['code_generate.pl'].
:- ['load.pl'].
:- ['machine.pl'].


compile( Program ) <-
    code_generate( parse( Program ) ).


demo does
    read_tokens( Tokens ) and
    write( `'Tokens           : ' ) and write( Tokens ) and nl and
    parse(Tokens,Tree) and
    write( `'Tree             : ' ) and write( Tree ) and nl and
    test_code_generate( Tree, 'CODE'(Code,InitialStore) ) and
    write( `'Code             : ' ) and write( Code ) and nl and
    loadcode( Code, LoadedCode ) and
    write( `'Loaded code      : ' ) and portray_map( LoadedCode ) and nl and
    write( `'Initial store    : ' ) and portray_map( InitialStore ) and nl and
    write( `'Input? ' ) and nl and
    read_list( In ) and
    test_run( 'MACHINE'( [], InitialStore, LoadedCode, 0, In, [] ) ).


read_tokens( L ) does
    seeing( CIS ) and
    see( `'program.pas' ) and seen and
    see( `'program.pas' ) and
    read_list( L ) and
    see( CIS ).
