%% JFMC: Moved here from old install.pl (this code was not used now,
%% but it would be interesting to generalize as a online interface to
%% Ciao)

% ---------------------------------------------------------------------------
% Web interface to CiaoPP

generate_ciaopp_online <- [generate_load_file,
	    generate_button_bar,
	    generate_ciaopp_daemon]
# "Generate Java Menu and CiaoPP daemon executable." :-
	copy_file('site/ciaopp_online_installed.html' ,
	    'site/ciaopp_online.html' ,
	    [overwrite]),
	do('echo "main(''site/ciaopp_menu.js''),halt." | ciaosh -l ~/.ciaorc -u generate_js_menu.pl', fail).

generate_load_file <- :-
	do(['ciaoc -o site/cgi/load_file.cgi site/cgi/load_file.pl'], fail).

generate_button_bar <- :-
	replace_strings_in_file([
		["<bindir>", ~atom_codes(~atom_concat(~get_distdir, 'cgi/')
		    )]
	    ],
	    'site/cgi/buttonbar.pl.skel', 'site/cgi/buttonbar.pl'),
	do(['ciaoc -o site/cgi/buttonbar.cgi site/cgi/buttonbar.pl'], fail).

generate_ciaopp_daemon <- :-
	replace_strings_in_file([
		["<bindir>", ~atom_codes(~atom_concat(~get_distdir, 'cgi/')
		    )]
	    ],
	    'site/cgi/ciaopp_daemon_wrapper.c.skel',
	    'site/cgi/ciaopp_daemon_wrapper.c'),
	do(['gcc site/cgi/ciaopp_daemon_wrapper.c -o ',
		'site/cgi/ciaopp_daemon_wrapper'],
	    fail),
	do(['ciaoc -u ', ~ciaoppsrc,
'/paths.pl -a ''actmods/tmpbased_publish'' site/cgi/ciaopp_daemon'
	    ] , fail).

install_ciaopp_online <- [install_load_file, install_button_bar] :-
	-do(['chmod -R -f og-w site/cgi'],  nofail),
	-do(['chmod -R -f og+rX site/cgi'], nofail),
	copy_file('site/ciaopp_menu.js'     ,
	    ~atom_concat(~get_distdir, '/ciaopp_menu.js')     , yes),
	copy_file('site/ciaopp_online.html' ,
	    ~atom_concat(~get_distdir, '/ciaopp_online.html') , yes),
	copy_dir_rec('images_shared',
	    ~atom_concat(~get_distdir , '/images/'),
	    '*', '*~', '.svn', '', []),
	copy_dir_rec('site/cgi'    , ~atom_concat(~get_distdir , '/cgi/'
	    ),
	    '*', '*~', '.svn', '', []),
	copy_dir_rec('site/css'    , ~atom_concat(~get_distdir , '/css/'
	    ),
	    '*', '*~', '.svn', '', []),
	copy_dir_rec('site/images' , ~atom_concat(~get_distdir ,
		'/images/'),
	    '*', '*~', '.svn', '', []),
	make(set_perms).

install_load_file <- :- true.
% 	do( ['mv site/cgi/load_file site/cgi/load_file.cgi'] , fail ).

install_button_bar <- :- true.
% 	do( ['mv site/cgi/buttonbar site/cgi/buttonbar.cgi'] , fail ).

