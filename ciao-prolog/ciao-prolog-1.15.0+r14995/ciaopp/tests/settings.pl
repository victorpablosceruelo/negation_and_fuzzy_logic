:- use_package(fsyntax).

:- use_module(library(terms)).
:- use_module(library(lpdist(ciao_config_options)), [ciaoppsrc/1]).

%       ( UNIQUE_ID, DIRECTORY )
good_dir(spec_ult, 'Spec/Results-2003-10-03/').
good_dir(plai_ult, 'Plai/Results-2003-10-03/').
good_dir(ctchecks) :=
	~atom_concat([~ciaoppsrc, '/tests/benchs/ctchecks/good/']).
good_dir(plai_types_ult, ~atom_concat([~ciaoppsrc, '/tests/benchs/',
		    ~test_dir_base(plai_types)])).

%       ( UNIQUE_ID, DIRECTORY )

test_dir(Key) := ~atom_concat([~ciaoppsrc, '/tests/benchs/',
		~test_dir_base(Key)]).

test_dir_base(spec_modes, 'spec_modes/').
test_dir_base(spec_types, 'spec_types/').
test_dir_base(modes,      'modes/').
test_dir_base(ai_safety,  'ai_safety/').
test_dir_base(plai_types, 'types/').


% test_options( TEST, - names the test (same as subdirectory under here 
% which contains the executables for the test) 
% [ TESTDIR, - ID of directory with the benchmarks 
%   GOODDIR, - ID of directory with the good results 
%   OPTIONS - tuple of options to pass to the TEST 
% ] ).



test_options(spec,     [spec_types, spec_ult, (di,   eterms, simp)]).
test_options(spec,     [spec_modes, spec_ult, (di,   shfr,   simp)]).
test_options(plai,     [modes,      plai_ult, (plai, shfr,   dep)]).
test_options(ctchecks, [ctchecks,   ctchecks, (plai, shfr,   dep)]).
%test_options( out  , [ modes ,             _ , (shfr , none) ] ).
test_options(checkfixpo,
	    [modes, _,
		(di, check_di, check_di2, check_di3, check_di4, shfr)]).
test_options(plai_types, [plai_types, plai_types_ult, (plai, eterms, dep)]).

%           ( TEST, TESTDIR, IGNORE_THESE_FILES )
filtering_of(checkfixpo, modes,
	    [peephole,
		witt,
		warplan
	    ]).
filtering_of(plai, modes, [qplan]).
filtering_of(plai_types, plai_types,
	    [
		analisis,
		pv_plan,
		pv_read
	    ]).
