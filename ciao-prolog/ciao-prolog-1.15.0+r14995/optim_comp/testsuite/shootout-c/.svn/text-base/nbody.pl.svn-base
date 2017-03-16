:- module(_, [], ['$purest']).
:- include(.(include(common))).
:- include(.(include(c_stdio))).
:- include(.(include(c_stdlib))).
:- include(.(include(c_math))).

:- '$native_weak_inline'(include('engine/nbody.native.h')).

:- '$improlog_begin'.

:- pred pi/1 + lowentrymacrocons(flt64, 'pi').
pi := 3.141592653589793.
:- pred solar_mass/1 + lowentrymacrocons(flt64, 'solar_mass').
solar_mass := 4.0 * ~pi * ~pi.
:- pred days_per_year/1 + lowentrymacrocons(flt64, 'days_per_year').
days_per_year := 365.24.

:- lowtype(planet).
:- class planet {
  :- struct.
  :- mut x :: flt64.
  :- mut y :: flt64.
  :- mut z :: flt64.
  :- mut vx :: flt64.
  :- mut vy :: flt64.
  :- mut vz :: flt64.
  :- mut mass :: flt64.
}.

:- pred advance/3 + lowentry(det, [intmach, ref1(array(ref0(planet))), flt64], 'advance').
advance(NBodies, Bodies, Dt) :-
	'$for_each'(I, ~intrange(NBodies), (
% TODO: use (B^...) or something similar to delimit its scope of required variables to the loop body 
	   B = ~'$to_ref1'(Bodies[@I]),
	   '$for_each'(J, ~intrange2(@I + 1, NBodies), (
	     B2 = ~'$to_ref1'(Bodies[@J]),
	     Dx = @B.x - @B2.x,
	     Dy = @B.y - @B2.y,
	     Dz = @B.z - @B2.z,
	     Distance = ~sqrt(Dx * Dx + Dy * Dy + Dz * Dz),
	     Mag = Dt / (Distance * Distance * Distance),
	     B.vx <- @B.vx - Dx * @B2.mass * Mag,
	     B.vy <- @B.vy - Dy * @B2.mass * Mag,
	     B.vz <- @B.vz - Dz * @B2.mass * Mag,
	     B2.vx <- @B2.vx + Dx * @B.mass * Mag,
	     B2.vy <- @B2.vy + Dy * @B.mass * Mag,
	     B2.vz <- @B2.vz + Dz * @B.mass * Mag
	   ))
         )),
	 % TODO: use I when local scope in for_each work...
	 '$for_each'(I2, ~intrange(NBodies), (
% TODO: use B instead of Bn (when added scope idioms)
           Bn = ~'$to_ref1'(Bodies[@I2]),
	   Bn.x <- @Bn.x + Dt * @Bn.vx,
	   Bn.y <- @Bn.y + Dt * @Bn.vy,
	   Bn.z <- @Bn.z + Dt * @Bn.vz
	 )).				    

:- pred energy/3 + lowentryfun([intmach, ref1(array(ref0(planet)))], flt64, 'energy').
energy(NBodies, Bodies, Result) :-
	E = ~initmut(flt64, 0.0),
	'$for_each'(I, ~intrange(NBodies), (
          B = ~'$to_ref1'(Bodies[@I]),
	  E <- @E + 0.5 * @B.mass * (@B.vx * @B.vx + @B.vy * @B.vy + @B.vz * @B.vz),
	  '$for_each'(J, ~intrange2(@I + 1, NBodies), (
            B2 = ~'$to_ref1'(Bodies[@J]),
	    Dx = @B.x - @B2.x,
	    Dy = @B.y - @B2.y,
	    Dz = @B.z - @B2.z,
	    Distance = ~sqrt(Dx * Dx + Dy * Dy + Dz * Dz),
	    E <- @E - (@B.mass * @B2.mass) / Distance
	  ))
        )),
	Result = @E.

:- pred offset_momentum/2 + lowentry(det, [intmach, ref1(array(ref0(planet)))], 'offset_momentum').
offset_momentum(NBodies, Bodies) :-
	Px = ~initmut(flt64, 0.0),
	Py = ~initmut(flt64, 0.0),
	Pz = ~initmut(flt64, 0.0),
	'$for_each'(I, ~intrange(NBodies), (
          Px <- @Px + @Bodies[@I].vx * @Bodies[@I].mass,
          Py <- @Py + @Bodies[@I].vy * @Bodies[@I].mass,
          Pz <- @Pz + @Bodies[@I].vz * @Bodies[@I].mass
        )),
	Bodies[0].vx <- - @Px / ~solar_mass,
	Bodies[0].vy <- - @Py / ~solar_mass,
	Bodies[0].vz <- - @Pz / ~solar_mass.

:- globalvar(bodies/1) + lowentry(ref0(array(ref0(planet))), 'bodies').
bodies__(T) :-
% TODO: WRONG! use struct initializers
	T = ~'$array_elems'(~'$array'(ref0(planet), [
              % sun
              ~'$array_elems'(~'$array_r0'(char, [
                0, 0, 0, 0, 0, 0, ~solar_mass
              ])),
              % jupiter
              ~'$array_elems'(~'$array_r0'(char, [
                4.84143144246472090e+00,
                -1.16032004402742839e+00,
                -1.03622044471123109e-01,
                1.66007664274403694e-03 * ~days_per_year,
                7.69901118419740425e-03 * ~days_per_year,
                -6.90460016972063023e-05 * ~days_per_year,
                9.54791938424326609e-04 * ~solar_mass
              ])),
              % saturn
              ~'$array_elems'(~'$array_r0'(char, [
                8.34336671824457987e+00,
                4.12479856412430479e+00,
                -4.03523417114321381e-01,
                -2.76742510726862411e-03 * ~days_per_year,
                4.99852801234917238e-03 * ~days_per_year,
                2.30417297573763929e-05 * ~days_per_year,
                2.85885980666130812e-04 * ~solar_mass
              ])),
              % uranus
              ~'$array_elems'(~'$array_r0'(char, [
                1.28943695621391310e+01,
                -1.51111514016986312e+01,
                -2.23307578892655734e-01,
                2.96460137564761618e-03 * ~days_per_year,
                2.37847173959480950e-03 * ~days_per_year,
                -2.96589568540237556e-05 * ~days_per_year,
                4.36624404335156298e-05 * ~solar_mass
              ])),
              % neptune
              ~'$array_elems'(~'$array_r0'(char, [
                1.53796971148509165e+01,
                -2.59193146099879641e+01,
                1.79258772950371181e-01,
                2.68067772490389322e-03 * ~days_per_year,
                1.62824170038242295e-03 * ~days_per_year,
                -9.51592254519715870e-05 * ~days_per_year,
                5.15138902046611451e-05 * ~solar_mass
              ]))
        ])).

% TODO: improve!
:- pred nbodies/1 + lowentrymacrocons(intmach, 'NBODIES').
nbodies := ~'$ccons'('(sizeof(bodies) / sizeof(planet_t))', intmach).

:- pred begin/1 + lowentry(det, [intmach], 'begin').
begin(N) :-
	offset_momentum(~nbodies, ~'$to_ref1'(~bodies)),
	printf2("%.9f\n", ~energy(~nbodies, ~'$to_ref1'(~bodies))),
	'$for_each'(I, ~intrangeclosed(1, N), (
          advance(~nbodies, ~'$to_ref1'(~bodies), 0.01)
	)),				     
	printf2("%.9f\n", ~energy(~nbodies, ~'$to_ref1'(~bodies))).

:- '$improlog_end'.
