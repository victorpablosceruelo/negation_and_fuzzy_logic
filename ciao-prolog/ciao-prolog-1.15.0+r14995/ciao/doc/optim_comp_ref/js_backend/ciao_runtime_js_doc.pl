:- module(ciao_runtime_doc, [], [assertions]).

:- doc(title, "Runtime for the JavaScript backend of Ciao").
:- doc(subtitle, "Internal's Manual").

:- doc(author, "Jose F. Morales").

:- doc(summary, "
   @href{https://developer.mozilla.org/en/JavaScript/Reference}{JavaScript}
   is a simple imperative language where data is either primitive,
   records (map from names to values), or closures (anonymous
   functions). Its has a C-like syntax, but at a deep level it is
   closer to Scheme and Self. There are no classes or modules, but
   they can be implemented using the language mechanism for scopes and
   prototypes.

   This Internal's Manual for the JavaScript backend of Ciao describes
   the design and details of the backend runtime and auxiliary
   definitions.

   In particular, this code defines a novel modular framework for
   Javascript programs. It allow defining modules and classes with
   mechanisms for importing other modules, hiding symbols from not
   imported code, and support for more advanced features like circular
   dependencies. The primary goal was to design a system with no
   performance penalty (in modern engines), and reusing the symbol
   hiding capabilities of JavaScript. This framework is recommended
   for a JavaScript backend rather than direct usage by users.

   The system was built from scratch, since many required features are
   not easily given by other existing frameworks or proposals (e.g.,
   Prototype, CommonJS, CoffeeScript, etc.).
").

:- doc(module, "
@section{Design of the module system}

   We assume a good knowledge of the JavaScript inner workings. See
   @ref{APPENDIX: JavaScript Semantics} for details.

   The module system is based on closures (see @ref{Hidding names}),
   to use the closure scope as a private namespace, and data
   structures to keep track of the modules and their dependency
   graph. The core structure is the @tt{mod__} object, which stores the
   module metadata, such as exported symbols, status of the module,
   and nested submodules.

   Closures for hidding names is a widely accepted and common
   @em{programming pattern} in JavaScript. We combine it with known
   techniques to perform symbol resolution in object code linkers
   (e.g., the definition of predicates symbols in Ciao).

@section{Defining and querying modules}

   Given an initial module (we assume that an initial @tt{rootmod}
   module is the root of all modules), we can perform two operations
   to obtain a reference to other new, nested module. Module
   definition creates a new nested module inside a given module, and
   initializes it (given the @tt{definition} code). On the other hand,
   module query just obtains a reference to a specified module, but
   leaves it as @tt{uninitialized} if it was not defined.

@section{Symbol resolution}

   In order to support circular dependencies, symbols are treated in
   two different passes. First, there is the module definition
   closure, which fills the exported symbol information, as well as
   defining nested modules. That code is executed when the module is
   defined. Second, there is a special @tt{link} function, provided by
   the module definition code, that fills the value of imported
   symbols (initialize local variables from the module scope to point
   to the values of the imported modules).

   Symbols are resolved by calling the @tt{prepare} method of a
   module.

@section{Modules defining classes (or instantiable modules)}

  A module can define a class. In that case, the module object will
  also contain the constructor function, the reference to the module
  implementing the base class, and a table of methods.  When the
  module is @tt{prepared}, the prototype will be filled with the
  methods.

  In V8 engine, @tt{c.prototype = function(...) @{ ... @}} results in
  faster code than defining it separatelly. So, the methods are
  inserted in this way, from a @tt{mlink} method similar to the
  @tt{link} function previously described for symbol resolution.

@section{APPENDIX: JavaScript Semantics}

   This appendix briefly describes the core concepts
   about JavaScript that must be taken into account in order to
   understand this code.

@section{Scopes, prototypes, and symbol lookup}

   Scopes in Javascript are records containing the names of all the
   local functions and variables. Scopes are chained, so that inner
   scopes are linked to outer scopes. Symbol lookup can see the names
   of outer scopes but there is no way to access names of inner
   scopes.

   Records contain a special entry called @tt{prototype}. The @tt{prototype}
   entry points to another record that can be shared between different
   records.  This mechanism is the basis for inheritance and
   object-oriented programming in JavaScript. It is not possible to
   directly modify the @tt{prototype} entry. Creation of records sharing
   some prototype is done by using the @tt{new} builtin. See @ref{Creating a
   new object} for more details.

@section{Hidding names}

   Closures and scopes are commonly used in JavaScript to encapsulate
   names. The basic schema to define a chunk of code with only some
   exported symbols is as follows:

@begin{verbatim}
    function() {
      var s1 = v1, ... sn = vn;
      function f1() { c1 }
      ...
      function fm() { cm }
      export({e1 ... ep});
    }
@end{verbatim}

   Where @tt{export} is some function that binds each (e_?) for
   external use. The symbols s_? are used from the functions f_?,
   and v_? the initializers for those symbols. All s_? and f_? will
   be visible and @em{cheaply accessible} from any c_?.

   The runtime code in this module is based on this same mechanism.

@section{Subclassing}

   See the @tt{extends__} code for explanation of subclassing.

@section{Creating a new object}

   Given a function @tt{ctor} the expression @tt{new ctor(args)} does the
   following:

@begin{itemize}
@item Create a new object @tt{o} of type @tt{object}.
@item Set the internal, innaccessible @tt{[[prototype]]} property to 
      @tt{ctor.prototype} (which is accessible).
@item Execute @tt{ctor} with @tt{this} bound to @tt{o}.
@end{itemize}

@section{Checking the value of the @tt{prototype} field of an object}

   The builtin @tt{instanceof} can indirectly compare the prototype of
   a given object. The @tt{o instanceof ctor} expression does the
   following:

@begin{itemize}
@item If @tt{ctor.prototype} is in the @tt{o} prototype chain, return true.
@item Otherwise, return false.
@end{itemize}

   Some JavaScript implementations offer access to the non-standard
   @tt{__proto__} object.

").


