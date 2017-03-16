// ===========================================================================
// Minimal runtime for the JavaScript backend of Ciao
// (c) 2012 Jose F. Morales
//
//   For extended documentation regarding this code, please check
//   docs/ciao_runtime_js_doc.pl (LPdoc manual)
// ===========================================================================

// Namespace for the core 'Ciao' runtime
var Ciao = (function () {
  var exports = {};

  // Modules and classes
  function mod__(name) {
    this.name = name; // name of the module
    this.exports = {}; // table of exports
    this.status = this.UNDEFINED; // not defined yet
    this.nested = {}; // database of nested modules
    this.link = null; // optional linking code
    this.mlink = null; // optional method linking code
  }
  // Status of a module
  mod__.prototype.UNDEFINED = 0; // queried, but not defined
  mod__.prototype.NOT_READY = 1; // not prepared
  mod__.prototype.PREPARING = 2; // in the process of preparing it
  mod__.prototype.READY = 3; // prepared

  // Define a nested module (returns the new module).
  // 'def' is the 'module definition' function.
  mod__.prototype.def = function(name, def) {
    var m = this.query(name);
    m.status = this.NOT_READY; // mark the module as not ready
    def(m);
    return m;
  }
  // Query an existing or new nested module 
  mod__.prototype.query = function(name) {
    // Query a module (initialize if necessary)
    var m = this.nested[name];
    if (m === undefined) {
      m = new mod__(name);
      this.nested[name] = m;
    }
    return m;
  }
  // Ensure that the module is ready
  mod__.prototype.prepare = function() {
    if (this.status === this.UNDEFINED) {
      throw "Module '" + this.name + "' is not defined";
    } else if (this.status === this.NOT_READY) {
      this.status = this.PREPARING; // preparing the module (not yet ready)
      // Prepare prototype chain and constructor, if the module defines a class
      if (this.ctor !== undefined) {
	if (this.base !== null) {
	  this.base.prepare();
	  if (this.base.status !== this.READY) {
	    throw "Base class '" + this.base.name + "' is not ready";
	  }
	  extends__(this.ctor, this.base.ctor);
	}
	// Invoke definition of instance methods and properties
	if (this.mlink !== null) {
	  this.mlink(this.ctor);
	  this.mlink = null; // (so that GC can collect those objects)
	}
      }
      // Invoke 'link' to link local symbols from imported modules
      if (this.link !== null) {
	this.link();
	this.link = null; // (so that GC can collect those objects)
      }
      this.status = this.READY; // mark the module as ready
      // Invoke 'prepare()' on nested modules and classes
      // TODO: Is correct doing it here?
      var has_prop = Object.prototype.hasOwnProperty; // cache for better performance
      for (var key in this.nested) {
	if (has_prop.call(this.nested, key)) this.nested[key].prepare();
      }
    }
    return this;
  }
  // Explanation of subclassing in JavaScript:
  //   http://www.2ality.com/2011/06/coffeescript-classes.html
  function extends__(c, base) {
    // Copy class methods from 'base' to 'c'
    // (it seems that there is no better way)
    var has_prop = Object.prototype.hasOwnProperty; // cache for better performance
    for (var key in base) {
      if (has_prop.call(base, key)) c[key] = base[key];
    }
    // Ensure that the object c.prototype has the prototype base.prototype
    // TODO: in ECMAScript 5, it should be:
    //   c.prototype = Object.create(base.prototype);
    function ctor() {}; ctor.prototype = base.prototype; c.prototype = new ctor;
    c.prototype.constructor = c;
    // Cache as a class method the base prototype
    // TODO: It is necessary for runtime information. If known, it can be replaced by explicit
    //   reference to the base class, e.g.:
    //     Base.constructor.call(this, <ARGS>);
    //   However, in JS it may not be efficienty (there is always some scope lookup).
    c.__super__ = base.prototype;
    // NOTE: This is they way to invoke the super constructor:
    //   MyClass.__super__.constructor.call(this, <ARGS>);
  }

  // -----------------------------------------------------------------------
  // The initial root module
  var $r = new mod__("root");
  $r.status = $r.NOT_READY;
  exports.root_mod = $r;
  
  // -----------------------------------------------------------------------
  // Finish definition of 'Ciao'
  return exports;
})();

// ---------------------------------------------------------------------------

// Shortcut to Ciao.root_mod
var $r = Ciao.root_mod;

// Shortcut to the bootstrap code
function __ciao_start__() {
  return Ciao.root_mod.exports.__start__$0();
}
