:- doc(section, "JavaScript Foreign Code").

% TODO: make it optional
decl__treatDom(js_native(_)).
decl__treat(js_native(String)) :- !,
	Module = ~def_envmod,
	trust(Module instance_of module_s),
	Module.add_native(String).
