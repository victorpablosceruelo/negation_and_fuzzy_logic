% This a sample program, which uses CiaoDHT library, which is 
% meant to be a prolog DHT implementation
% Copyright (C) 2006 "Arsen Kostenko" <kosty@clip.dia.fi.upm.es>
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this library; if not, write to the Free Software
% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
:- module(_, _, []).

:- use_module(library(dht(dht_server))).
:- use_module(library(concurrency)).

main([]):-
      display('This example illustrates starting a DHT from CiaoProlog.'),\
      nl,
      eng_call(dht_prolog([s2c_threads(15), s2s_threads(30)]), 
               create,
               create).
