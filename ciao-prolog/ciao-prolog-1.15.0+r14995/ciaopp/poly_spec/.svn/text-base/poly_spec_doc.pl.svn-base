:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title,"Poly-Controlled Partial Evaluation").
:- doc(subtitle_extra,"REFERENCE MANUAL").
:- doc(subtitle_extra,"@em{Generated/Printed on:} @today{}").

:- doc(author,"Claudio Ochoa").
:- doc(author,"Germ@'{a}n Puebla").

:- doc(copyright,"
Copyright @copyright{} 2004-2006 Claudio Ochoa and Germ@'{a}n Puebla/The CLIP Group.").
%@include{Copyright.Manuals}
%").

:- doc(usage, "The poly-controlled partial evaluator can be
	accessed from the @tt{CiaoPP} interface.").

:- doc(summary, "@em{Poly-Controlled Partial Evaluation} is a
	novel framework for partial evaluation which is
	@em{poly-controlled} in that it can take into account
	@em{repertoires} of global control and local control rules
	instead of a single, predetermined combination (as done by
	traditional partial evaluation).  This approach is more
	flexible than existing ones since it allows assigning
	@em{different} global and local control rules to different
	call patterns, thus generating results that cannot be obtained
	using traditional partial evaluation.").

% 

:- doc(module, "
@cindex{A PCPE Session Example}
Existing partial evaluators usually offer a wide set of parameters to
choose from, most of them affecting the quality of the residual
program obtained.

In our implementation of the poly-controlled partial evaluator,
besides providing a graphical menu with very few options, intended for
the na\"ive user, and with some more extra options for the advanced
user, we also provide the possibility of setting low-level flags
modifying the behaviour of poly-controlled partial evaluation.

@section{A PCPE Session in the Top Level of Ciao}
@cindex{A PCPE Session in the Top Level of Ciao}

In this section we show an example session for expert users,
explaining the available flags and the effects on the achieved
specialization that is controlled by these flags.

In order to start a @tt{ciaopp} session,  the top level of @tt{ciao}
should be started. This can be done by typing @tt{ciao} in a shell, or
@tt{M-x ciao} from @tt{emacs}. 
%
From the top level of @tt{ciao}, we can load @tt{ciaopp} by issuing:

@begin{verbatim}
    :- use_module(ciaopp(ciaopp)).
@end{verbatim}

Once we have loaded @tt{ciaopp}, we can set the corresponding flags by
using the @tt{ciaopp} predicate @tt{set_pp_flag/1}. We list the currently
available flags below.

The only mandatory flag that needs to be set is the @tt{fixpoint}
flag, which controls the fixpoint algorithm to be used during
analysis. In order to use poly-controlled partial evaluation, you
should set this flag to @tt{poly_spec}:

@begin{verbatim}
     :- set_pp_flag(fixpoint, poly_spec).
@end{verbatim}

After setting the corresponding flags, we can load the module to be
analyzed by using the @tt{ciaopp} predicate @tt{module/1}:


@begin{verbatim}
    :- module(foo).
@end{verbatim}

where @tt{foo.pl} is in the current directory. Otherwise we should
use an absolute path. Now we can analyze this module using the @tt{ciaopp}
predicate @tt{analyze/1}. 


@begin{verbatim}
    :- analyze(pd).
@end{verbatim}

After the analysis is completed, all solutions found by PCPE are
asserted. Code can be generated for all of them, and the evaluation
step takes place by calling the @tt{ciaopp} predicate @tt{transform/1}.

@begin{verbatim}
    :- transform(codegen_poly).
@end{verbatim}

In the top-level a message will inform of which generated solution is
the best one.

@section{Available Flags for Controlling PCPE from the Top Level}
@cindex{Available Flags for Controlling PCPE from the Top Level}

The following flags are available from the top level, and they allow
to drive the behaviour of poly-controlled partial evaluation:

@begin{description}

@item{poly_global_control} Determines the set of abstraction functions
  to be used during poly-controlled global control. This flag is set
  to a list of global control rules, where the valid values are taken from
  the possible values of the flag @tt{global_control}: @tt{hom_emb},
    @tt{hom_emb_num}, @tt{dyn}, @tt{ id} and @tt{inst}.

  For example, in order to set the global control rules to a set
  containing the global control rules @tt{hom_emb} and @tt{dynamic},
  the following @tt{ciaopp} predicate call should be issued:

@begin{verbatim}
    :- set_pp_flag(poly_global_control,[hom_emb, dyn]).
@end{verbatim}
    
 

@item{poly_local_control} Determines the set of unfolding strategies
     to be used during poly-controlled global control. This flag is
     set to a list of local control rules, where a local control rule
     is a list containing the unfolding strategy (@tt{local_control}),
     the computation rule (@tt{comp_rule}) and the unfolding branching
     factor (@tt{unf_bra_fac}) (the last two are optional).

  @begin{description} 

  @item{local_control:} is the actual unfolding strategy.  It can take
     any of the values of the flag @tt{local_control} in
     @tt{preprocess_flags.pl}.

  @item{comp_rule:} determines the computation rule to be used. It
    can take any of the values of the flag @tt{comp_rule} in
    @tt{preprocess_flags.pl}.

  @item{unf_bra_fac} is a non-negative integer determining the
    unfolding branching factor to be used during unfolding.
  @end{description}

     For example, in order to set the local control rules to a set
     containing an aggressive and a conservative control rules, the
     following @tt{ciaopp} predicate call can be issued:

@begin{verbatim}
:- set_pp_flag(poly_lobal_control, [[local_control(det), comp_rule(leftmost), unf_bra_fac(1)],
        [local_control(df_hom_emb_as), comp_rule(bind_ins_jb), unf_bra_fac(0)]]).
@end{verbatim}
    

@item{poly_fitness:} specifies the fitness function to be used by the
    poly-controlled partial evaluator. The possible values are @tt{
    speedup, bytecode, memory, bounded_size, balance}. The default value is
    @tt{bytecode}.

@item{poly_strategy:} determines the kind of PCPE algorithm to be used. The
possible choices are 


@begin{description} 

  @item{all_sols:} corresponds to the all-solutions search-based
         algorithm. This algorithm produces @em{sets} of candidate
         specialized programs (most of them hybrid), instead of a
         single one.  The quality of each of these programs is
         assessed through the use of different fitness functions,
         which can be @em{resource aware}, taking into account
         multiple factors such as run-time, memory consumption, and
         code size of the specialized programs, among others. The
         pruning to be performed on this algorithm is determined by
         the @tt{poly_pruning} flag (see below).

  @item{oracle:} corresponds to the oracle-based PCPE algorithm. It
         basically applies all control strategies to a call pattern
         and then @em{an oracle} makes an informed decision|based on
         heuristics| of which control strategy is the most promising
         one in every step of the algorithm. Then, the best
         intermediate solution is kept active while the rest are
         discarded, thus drastically pruning the search space.

@end{description}

@item{poly_pruning:} determines the kind of pruning to be performed to
      the all-solutions PCPE algorithm. The possible choices are

@begin{description}

  @item{none:} no pruning is performed.

  @item{heuristic:} applies a heuristic determined by the
         @em{polyvar_pcpe} flag.  Given a selected atom in the current
         configuration, rather than trying all possible control
         strategies, it considers only those control strategies which
         are @em{consistent} with the choices previously taken in
         previous configurations. The type of heuristic is determined
         by the @tt{polyvar_pcpe} flag (see below).


  @item{bnb:} applies the branch and bound pruning by storing the
         fitness value of the best solution found so far, and pruning
         away those configurations which are guaranteed not to improve
         the (temporary) optimal solution.

  @item{both:} applies a combination of the branch and bound and the
         heuristics pruning techniques.

@end{description}


@item{polyvar_pcpe:} Controls  the polyvariance of poly-controlled
  partial evaluation. The possible values are

  @begin{description}
  @item{off:} no control of polyvariance is performed.

  @item{pred:} the @em{predicate-consistent} pruning technique is
       applied to the base algorithm, i.e., it uses the @em{same
       control strategy for all atoms which correspond to the same
       predicate}.

  @item{modes:} the @em{mode-consistent} pruning technique is applied
       to the base algorithm, i.e., two calls to a predicate with
       similar @em{modes} (instantiation level in their arguments)
       have to use the same control strategy, but not if they have
       different modes.

       In this sense, the more precise is the domain of modes, the
       less restrictive will be the simplifications made, thus
       producing a higher number of candidate solutions. The domain
       used is set by the @tt{poly_modes} flag, which takes one of the
       following values: @tt{sd}, @tt{sdl}, @tt{sd_depth2}.

  @end{description}

@item{poly_depth_lim:} is a non-negative integer value @tt{N}. This
     number means that every @tt{N} levels of depth in the search
     space tree, branch and bound pruning will be performed. If
     @tt{N=0} then no pruning is performed.

@item{output_info:} this flag can be set to either @tt{none, medium},
      or @tt{high}, and determines the amount of information to be
      written as comments in the residual files.

@item{inter_opt_arg_filt:} when set to @tt{on}, redundant arguments in
     predicates are filtered away.

@item{min_crit:} determines whether a post-minimization step will be
     performed for all found solutions. It can take any of the
     following values: @tt{none}, @tt{equal}, @tt{codemsg},
     @tt{nobindings}, @tt{bindings}, @tt{residual}.


@end{description}


All of these flags can be set by using the @tt{ciaopp} predicate
@tt{set_pp_flag/1}, in the following manner:

@begin{verbatim}
   :- set_pp_flag(flag, value).
@end{verbatim}


").

