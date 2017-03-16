:- module(A,[],[]).
:- use_module(library(terms), [atom_concat/2]).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).
:- use_module(library(xml(xml_rt)), [continue_index_querying/4]).
:- reexport(library(xml(xml_rt)),[xml_index_query/3]).
:- multifile xml_attribute/4.
:- dynamic xml_attribute/4.
:- multifile xml_element/4.
:- dynamic xml_element/4.

xml_element('$product',A,B,C) :-
        !,
        continue_index_querying(A,'$product',B,C).

xml_element('$product$currently',[],1,[]).

xml_attribute('$product$currently_att',[currency=12],1,A) :-
        A=[currency=12].

xml_attribute('$product$currently_att',[],1,[]).

xml_element('$product$quantity',[100],1,[100]).

xml_attribute('$product$quantity_att',[],A,[]).

xml_element('$product$time-left',[20],1,[20]).

xml_attribute('$product$time-left_att',[],A,[]).

xml_element('$product$first-bid',[],1,[]).

xml_attribute('$product$first-bid_att',[],A,[]).

xml_element('$product$number-of-bids',[],1,[]).

xml_attribute('$product$number-of-bids_att',[],A,[]).

xml_element('$product$location',[77],1,[[77,97,100,114,105,100]]).

xml_attribute('$product$location_att',[],A,[]).

xml_element('$product$started',[],1,[]).

xml_attribute('$product$started_att',[],A,[]).

xml_element('$product$ends',[],1,[]).

xml_attribute('$product$ends_att',[],A,[]).

xml_element('$product$author',[],1,[]).

xml_attribute('$product$author_att',[],A,[]).

xml_element('$product$high-bid',[],1,[]).

xml_attribute('$product$high-bid_att',[],A,[]).

xml_element('$product$payment',[],1,[]).

xml_attribute('$product$payment_att',[],A,[]).

xml_element('$product$shipping',[],1,[]).

xml_attribute('$product$shipping_att',[],A,[]).

xml_element('$product$negotiation',A,B,C) :-
        !,
        continue_index_querying(A,'$product$negotiation',B,C).

xml_element('$product$negotiation$preference',A,B,C) :-
        !,
        continue_index_querying(A,'$product$negotiation$preference',B,C).

xml_element('$product$negotiation$preference$constraint',[],1,[]).

xml_attribute('$product$negotiation$preference$constraint_att',[],A,[]).

xml_element('$product$negotiation$preference$price',[1000],1,[1000]).

xml_attribute('$product$negotiation$preference$price_att',[],A,[]).

xml_attribute('$product$negotiation$preference_att',[],A,[]).

xml_attribute('$product$negotiation_att',[],A,[]).

xml_element('$product$description',A,B,C) :-
        !,
        continue_index_querying(A,'$product$description',B,C).

xml_element('$product$description$Specie',[],1,[]).

xml_attribute('$product$description$Specie_att',[],A,[]).

xml_element('$product$description$Size',[],1,[]).

xml_attribute('$product$description$Size_att',[],A,[]).

xml_attribute('$product$description_att',[],A,[]).

xml_attribute('$product_att',[product_name=112],1,A) :-
        A=[product_name=[112,114,117,101,98,97]].

xml_attribute('$product_att',[xmlns=120],1,A) :-
        A=[xmlns=[120,45,115,99,104,101,109,97,58,46,46,47,102,105,115,104,46,120,100,114]].

xml_attribute('$product_att',[],1,[]).
