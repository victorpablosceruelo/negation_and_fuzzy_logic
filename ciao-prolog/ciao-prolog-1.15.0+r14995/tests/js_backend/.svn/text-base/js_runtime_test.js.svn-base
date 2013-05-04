// Hand-coded example for the core Ciao runtime system (defining
// modules).

// Execute with: $ node example.js
// The expected result is:
//   DUMMY: 111
//   point 1 2
//   point 2 3
//   label label1
//   point 3 4
//   color color1

var fs = require('fs');
eval(fs.readFileSync('../lib/ciao_runtime.js')+'');

// ---------------------------------------------------------------------------
// Two sample modules with a circular dependency
$r.def("mymod1", function($m) {
    var $e = $m.exports;
    var $m_mymod1 = $r.query("mymod1");
    var $m_mymod2 = $r.query("mymod2");
    var o;
    function foo() {
	return 1 + o.foo();
    };
    $e.foo = foo;
    function bar() {
	return 10;
    };
    $e.bar = bar;
    // point class
    var $c_point = $m.def("point", function($m) {
	$m.base = null;
	var $e = $m.exports;
	function point(x, y) { this.x = x; this.y = y; };
	$m.ctor = point;
	$m.mlink = function($c) {
	    $c.prototype.show = function() {
		console.log("point " + this.x + " " + this.y);
	    };
	};
	// ----------
	// $m.link = function() {};
    });
    var point = $c_point.ctor;
    $e.point = point;
    // labeled point class
    var $c_lpoint = $m.def("lpoint", function($m) {
	$m.base = $c_point;
	var $e = $m.exports;
	function lpoint(x, y, l) { lpoint.__super__.constructor.call(this, x, y); this.l = l; };
	$m.ctor = lpoint;
	$m.mlink = function($c) {
	    $c.prototype.show = function() {
		lpoint.__super__.show.call(this);
		console.log("label " + this.l);
	    };
	};
	// ----------
	// $m.link = function() {};
    });
    var lpoint = $c_lpoint.ctor;
    $e.lpoint = lpoint;
    // ----------
    $m.link = function() {
	o = $m_mymod2.exports;
    };
});
$r.def("mymod2", function($m) {
    var $e = $m.exports;
    var $m_mymod1 = $r.query("mymod1");
    var o;
    function foo() {
	return 100 + o.bar();
    };
    $e.foo = foo;
    // colored point class
    // (extends mymod1.point, which may not be initialized)
    var $c_cpoint = $m.def("cpoint", function($m) {
	$m.base = $m_mymod1.query("point");
	var $e = $m.exports;
	function cpoint(x, y, c) { cpoint.__super__.constructor.call(this, x, y); this.c = c; };
	$m.ctor = cpoint;
	$m.mlink = function($c) {
	    $c.prototype.show = function() {
		cpoint.__super__.show.call(this);
		console.log("color " + this.c);
	    };
	};
	// ----------
	// $m.link = function() {};
    });
    var cpoint = $c_cpoint.ctor;
    $e.cpoint = cpoint;
    // ----------
    $m.link = function() {
	o = $m_mymod1.exports;
    };
});
(function() {
    var $m_mymod1 = $r.query("mymod1");
    var $m_mymod2 = $r.query("mymod2");
    $m_mymod1.prepare();
    $m_mymod2.prepare();
    var mm = $m_mymod1.exports;
    var mm2 = $m_mymod2.exports;
    console.log("DUMMY: " + mm.foo());
    (new mm.point(1,2)).show();
    (new mm.lpoint(2,3,"label1")).show();
    (new mm2.cpoint(3,4,"color1")).show();
})();
