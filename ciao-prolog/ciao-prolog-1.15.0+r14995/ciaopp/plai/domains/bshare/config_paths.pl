:- module(config_paths,
	[
	 bSH_path/1,
	 tSH_path/1,
	 tNSH_path/1,
	 files_path/1
	]).

common_path('/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/low_level_ops/').

bSH_path(Path)  :- common_path(Path).
tSH_path(Path)  :- common_path(Path).
tNSH_path(Path) :- common_path(Path).

files_path( '/home/jorge/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/tmp/').

