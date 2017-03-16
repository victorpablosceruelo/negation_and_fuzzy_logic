:- module(attributes, [], [pure, assertions]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(basic_props)).
:- use_module(engine(term_typing)).
:- use_module(engine(term_basic)).

:- doc(title,"Attributed variables").

:- doc(author,"Christian Holzbaur").
:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Carro").

:- doc(module, "These predicates allow the manipulation of
   @index{attributed variables}. Attributes are special terms 
   which are attached to a (free) variable, and are hidden from
   the normal Prolog computation. They can only be treated by
   using the predicates below.").

:- doc(summary, "This library implements @index{attributed
variables}, which provide a mechanism for extensible unification.  The
library is based on the DMCAI CLP Library by Christian Holzbaur.  ").

:- doc(copyright,"Original copyright @copyright{} 1992 DMCAI

Department of Medical Cybernetics and Artificial Intelligence 
University of Vienna 
Freyung 6 
A-1010 Vienna, Austria").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- '$native_include_c_source'(.(attributes)).

:- export(attach_attribute/2). 
:- pred attach_attribute(Var,Attr) : var * nonvar
        # "Attach attribute @var{Attr} to @var{Var}.".
:- '$props'(attach_attribute/2, [impnat=cblt(bu2_attach_attribute,0)]).

% JFMC: I added this to avoid garbage collection problems
:- export(attach_attribute_weak/2). 
:- pred attach_attribute_weak(Var,Attr) : var * nonvar
        # "Attach attribute @var{Attr} to @var{Var} (but delete unbound CVAs in GC).".
:- '$props'(attach_attribute_weak/2, [impnat=cblt(bu2_attach_attribute_weak,0)]).

:- export(get_attribute/2).
:- pred get_attribute(Var,Attr) : var(Var) => nonvar(Attr)
        # "Unify @var{Attr} with the attribute of @var{Var}, or fail if
          @var{Var} has no attribute.".
:- '$props'(get_attribute/2, [impnat=cfunre(fu1_get_attribute,no)]).

:- export(update_attribute/2).
:- pred update_attribute(Var,Attr) : var * nonvar
        # "Change the attribute of attributed variable @var{Var} to
          @var{Attr}.".
:- '$props'(update_attribute/2, [impnat=cblt(bu2_update_attribute,0)]).

:- export(detach_attribute/1).
:- pred detach_attribute(Var) : var
        # "Take out the attribute from the  attributed variable @var{Var}.".
:- '$props'(detach_attribute/1, [impnat=cblt(bu1_detach_attribute,0)]).

:- multifile verify_attribute/2.
:- pred verify_attribute(Attr, Term): nonvar * nonvar # "@em{A user
defined predicate.} This predicate is called when an attributed
variable with attribute @var{Attr} is about to be unified with the
non-variable term @var{Term}.  The user should define this predicate
(as multifile) in the modules implementing special unification.".

:- multifile combine_attributes/2.
:- pred combine_attributes(Var1, Var2): var * var # "@em{A user
defined predicate.} This predicate is called when two attributed
variables with attributes @var{Var1} and @var{Var2} are about to be
unified.  The user should define this predicate (as multifile) in the
modules implementing special unification.".

:- doc(appendix, "Note that @pred{combine_attributes/2} and
@pred{verify_attribute/2} are not called with the attributed variables
involved, but with the corresponding attributes instead.  The reasons
are:

@begin{itemize} 

@item There are simple applications which only refer to the
attributes.

@item If the application wants to refer to the attributed variables
themselves, they can be made part the attribute term.  The
implementation of @pred{freeze/2} utilizes this technique.  Note that
this does not lead to cyclic structures, as the connection between an
attributed variable and its attribute is invisible to the pure parts
of the Prolog implementation. 

@item If attributed variables were passed as arguments, the user
code would have to refer to the attributes through an extra call to
@pred{get_attribute/2}.

@item As the/one attribute is the first argument to each of the two
predicates, indexing applies. Note that attributed variables
themselves look like variables to the indexing mechanism.

@end{itemize}

However, future improvements may change or extend the interface to
attributed variables in order to provide a richer and more expressive
interface.

For customized output of attributed variables, please refer to the
documentation of the predicate @pred{portray_attribute/2}.  ").
