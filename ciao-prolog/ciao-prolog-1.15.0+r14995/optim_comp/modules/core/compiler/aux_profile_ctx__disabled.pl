% Like aux_profile_ctx, but disables any profiling (without any
% runtime penalty).
%
% Author: Jose F. Morales

{
:- '$all_static'.
:- '$def_binder'(profile0(_), true).
:- '$def_binder'(profile(_), true).
:- '$def_binder'(profile_costly(_), true).
}.

