:- use_module(library(arithpreds)).

:- module ui_test {
    :- export(attach/1).
    attach(Container) :-
    	write_html(Container),
	NameDisplay = ~streams_div("nameoutput"),
	% TODO: it should be 'my_menu.install', but it does not work!
	%   "X = ~my_menu" is wrong
	%   "X = ~demo.my_menu" is correct
	demo.my_menu.install(@(~js_dom.document.fastfood_order.menu)),
	Text = @(~js_dom.document.fastfood_order.yourname),
	Text.onkeyup <- ~js_dom.event_handler(changed_text(NameDisplay, Text)).

    write_html(Container) :-
        Div = ~js_dom.createElement("div"),
	Div.className <- "fastfoodbox",
	Div.innerHTML <-
"<h2>Fast food order</h2>
<form name='fastfood_order'>
Your name: <input type='text' name='yourname'/><br/>
Your choice:
<select name='menu'>
<option value='0' selected>(select one)</option>
<option value='1'>cheese burger</option>
<option value='2'>burger without cheese</option>
<option value='3'>vegetarian burger</option>
</select>
<table><tr>
<td>
<input type='radio' name='mysize' value='big' onclick='changed_size(this);'/> Big <br />
<input type='radio' name='mysize' value='small'
       onclick='changed_size(this);' checked/> Small <br />
<input type='checkbox' name='supplement' value='bread' /> Bread
<br />
<input type='checkbox' name='supplement' value='softdrink' /> Soft drink
<br />
<input type='checkbox' name='supplement' value='dessert' /> Dessert
<br />
</td>
<td>
<div id='nameoutput' class='namebox'></div>
Total price: <span id='menuoutput' class='menubox'>...</span>
</td>
</tr></table>
</form>",
        Container.append_child(Div).

    changed_text(NameDisplay, Text) :-
        Value1 = @(~Text.value),
        SizeValue = ~selected_size,
        ( SizeValue = "big" ->
	    Value = ~Value1.uppercase
        ; SizeValue = "small" ->
	    Value = ~Value1.lowercase
	; Value = "unknown"
	),
        NameDisplay.set(Value).

% TODO: UI is not very responsive, use any set_timeout trick?
%	set_timeout0.
%
%    :- pred set_timeout0/0 + det.
%    set_timeout0 :- js_lang.stats([
%      setTimeout.[function([], []), 0]
%    ]).

    % TODO: this should be possible without js_lang package
    :- use_package(js_lang).

    :- pred selected_size/1 :: t_string + (detfun, argsbox([unbox])).
    selected_size := ~js_lang.expr('size_value').
}.

% TODO: very delicate; does not work if scope changes
:- js_native([
  vardecl('size_value', '\'small\''),
  function(call('changed_size', ['x']), ['size_value' <- 'x'.value])
]).

:- module my_menu {
    :- export(install/1).
    install(Element) :- 
        console_log("log-install-1"),
        MenuDisplay = ~streams_div("menuoutput"),
        console_log("log-install-2"),
        Element.onchange <- ~js_dom.event_handler(on_change(MenuDisplay, Element)).

    % TODO: Make mutables native, so that mutable arrays are also native
    %       and we do not need X[_] notation at all (just use X(_)).
    %       Of course, arrays need special operations, like length, etc.
    on_change(MenuDisplay, Element) :-
        SelectedIndex = @(~Element.selectedIndex),
        console.display("Menu changed" + SelectedIndex), console.nl,
        Value = ~((@(~(((~Element.options)[SelectedIndex]).value))).'$to_int'),
        MenuDisplay.set(~burger_price(Value).'$to_str').

    % TODO: implement indexing so that cuts are not necessary
    burger_price(0) := 0 :- !.
    burger_price(1) := 4 :- !.
    burger_price(2) := 5 :- !.
    burger_price(3) := 7.
}.

