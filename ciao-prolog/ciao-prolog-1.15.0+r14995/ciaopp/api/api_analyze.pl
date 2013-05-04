% Declaration for analyses

%:- use_package( argnames_fsyntax ).
:- use_package( argnames ).

:- argnames ana_ppi_complete( ppi, cls_inspected, call, head, extra_vars, abs_info ).
:- argnames ana_cls_complete( ppi, call_id, head, abs_info ).

:- argnames ana_state(cls,        % clause (or fact) being analized
	              head,       % Head of the current predicate being analized
	              head_vars,  % Variables of the head. Head is always normalized
		      call_vars,  % Variables of the head been analized
	              vars,       % Current variables at current point
		      abs_info,   % Current abstract information at current point
		      user_info,  % whatever user information
		      ppi,        % ppi being analyzed
		      path,       % solution path (path from pred clause to ppi)
		      cycles,     % argument for recursive hook
		      ppi_state,
		      info_saved, % Determines whether to save the ana info or not
		      extra_arg   % Extra argument needed in some hooks
		    ).
