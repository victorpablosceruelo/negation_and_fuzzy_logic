:- module(js_foreign, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Interface with Javascript objects").
:- doc(author, "Jose F. Morales").

% ---------------------------------------------------------------------------

:- class js_object {
    :- doc(title, "JavaScript objects").

    % Note: instances created from native 'box_js' code
    :- extends(simple_box('===')).

    % TODO: write a hierarchy of objects!
    % TODO: write automatically and/or allow dynamic methods
    :- export(innerHTML/1).
    innerHTML := ~js_object_field_mutable(~self, "innerHTML").
    :- export(title/1).
    title := ~js_object_field_mutable(~self, "title").
    :- export(style/1).
    style := ~js_object_field_mutable(~self, "style").
    :- export(verticalAlign/1).
    verticalAlign := ~js_object_field_mutable(~self, "verticalAlign").
    :- export(value/1).
    value := ~js_object_field_mutable(~self, "value").
    :- export(disabled/1).
    disabled := ~js_object_field_mutable(~self, "disabled").
    :- export(onclick/1).
    onclick := ~js_object_field_mutable(~self, "onclick").
    :- export(id/1).
    id := ~js_object_field_mutable(~self, "id").
    :- export(className/1).
    className := ~js_object_field_mutable(~self, "className").
    :- export(body/1).
    body := ~js_object_field_mutable(~self, "body").
    :- export(href/1).
    href := ~js_object_field_mutable(~self, "href").
    :- export(src/1).
    src := ~js_object_field_mutable(~self, "src").
    :- export(rel/1).
    rel := ~js_object_field_mutable(~self, "rel").
    :- export(type/1).
    type := ~js_object_field_mutable(~self, "type").
    :- export(fastfood_order/1).
    fastfood_order := ~js_object_field_mutable(~self, "fastfood_order").
    :- export(onchange/1).
    onchange := ~js_object_field_mutable(~self, "onchange").
    :- export(onkeyup/1).
    onkeyup := ~js_object_field_mutable(~self, "onkeyup").
    :- export(onreadystatechange/1).
    onreadystatechange := ~js_object_field_mutable(~self, "onreadystatechange").
    :- export(responseText/1).
    responseText := ~js_object_field_mutable(~self, "responseText").
    :- export(readyState/1).
    readyState := ~js_object_field_mutable(~self, "readyState").
    :- export(selectedIndex/1).
    selectedIndex := ~js_object_field_mutable(~self, "selectedIndex").
    :- export(options/1).
    options := ~js_object_field_mutable(~self, "options").
    :- export(parentNode/1).
    parentNode := ~js_object_field_mutable(~self, "parentNode").

    :- export('$get_field'/2).
    '$get_field'(ChildName) := ~js_object_field_mutable(~self, ChildName).

    % TODO: define a as/3 meta-function to do casting (on error throw exception)
    % TODO: introduce exceptions!

    :- use_package(js_lang).

    % TODO: create a class for htmlelement! add this as method
    :- export(append_child/1).
    :- pred append_child/1 :: js_object + (det, argsbox([unbox])).
    append_child(Child) :- js_lang.stats([(~self).appendChild.[Child]]).

    % TODO: create a class for htmlelement! add this as method
    :- export(remove_child/1).
    :- pred remove_child/1 :: js_object + (det, argsbox([unbox])).
    remove_child(Child) :- js_lang.stats([(~self).removeChild.[Child]]).
}.

:- class js_object_field_mutable {
    % TODO: write a hierarchy of objects!
    :- export(display/1).
    display := ~js_object_field_mutable(@(~self), "display").
    :- export(verticalAlign/1).
    verticalAlign := ~js_object_field_mutable(@(~self), "verticalAlign").
    :- export(menu/1).
    menu := ~js_object_field_mutable(@(~self), "menu").
    :- export(value/1).
    value := ~js_object_field_mutable(@(~self), "value").
    :- export(yourname/1).
    yourname := ~js_object_field_mutable(@(~self), "yourname").

    :- use_package(js_lang).

    % The mutable fields of js_object, which contain js_objects 
    % (or elements that can cast to js_objects)
    % TODO: ChildName can be t_string or int, this is why I am 
    %       unboxing it by hand... define a type for that!
    :- export(cons__/2).
    :- pred cons__/2 :: js_object * term + (det, argsbox([unbox, box])).
    cons__(Parent, ChildName) :- js_lang.stats([
      (~self).parent <- Parent,
      (~self).child_name <- ChildName.deref.[].unbox.[]
    ]).

    % TODO: why unboxing Value by hand?
    :- export('$mutset'/1).
    :- pred '$mutset'/1 + det.
    '$mutset'(Value) :- js_lang.stats([
      (~self).parent.elem((~self).child_name) <- Value.deref.[].unbox.[]
    ]).

    % TODO: why unboxing Value by hand?
    :- export('$mutinc'/1).
    :- pred '$mutinc'/1 + det.
    '$mutinc'(Value) :- js_lang.stats([
      '+='((~self).parent.elem((~self).child_name),
           Value.deref.[].unbox.[])
    ]).

    :- export('$mutget'/1).
    :- pred '$mutget'/1 + detfun.
%    '$mutget' := ~js_lang.expr(box_js.[(~self).parent.elem((~self).child_name)]).
    '$mutget' := ~js_lang.stats([
      vardecl('value', (~self).parent.elem((~self).child_name)),
      % TODO: move this code to the worker or to term (this should be shared)
      % (Box a JavaScript object inside a term (dynamically choosing the right box))
      % TODO: register castings in a table (a-la attribute hooks) that relates native types with term wrappers
      vardecl('value_type', typeof('value')),
      switch('value_type', [
        case('\'string\'', return(new(ctor_lookup(class, ':'(string_type_rt, t_string)), ['value']))),
        case('\'number\'', return(new(ctor_lookup(class, ':'(arithmetic, t_num)), ['value']))),
        default(return(new(ctor_lookup(class, ':'(js_foreign, js_object)), ['value'])))
      ])
    ]).

    :- export('$get_field'/2).
    '$get_field'(ChildName) := (@(~self))[ChildName].
}.

:- use_package(js_lang).

% % Box a JavaScript object inside a term (dynamically choosing the right box)
% % TODO: this is incomplete... what can we put here?
% % TODO: could we use attributed variables?
% :- js_native([
%   function(call('box_js', ['value']), [
%     vardecl('value_type', typeof('value')),
%     switch('value_type', [
%       case('\'string\'', return(new(ctor_lookup(class, ':'(string_type_rt, t_string)), ['value']))),
%       case('\'number\'', return(new(ctor_lookup(class, ':'(arithmetic, t_num)), ['value']))),
%       default(return(new(ctor_lookup(class, ':'(js_foreign, js_object)), ['value'])))
%     ])
%   ])
% ]).

% ---------------------------------------------------------------------------

% Some Javascript constants
% TODO: can I remove '__' from 'true' and 'false'? (there are predicates
%   with those names)
% TODO: boxjs may fail here...
% TODO: those, like constants, may be really functions (no ~ necessary)
:- module jscons {
    :- use_package(js_lang).

    :- export(true__/1).
    :- pred true__/1 :: js_object + (detfun, argsbox([unbox])).
    true__ := ~js_lang.expr('true').
    :- export(false__/1).
    :- pred false__/1 :: js_object + (detfun, argsbox([unbox])).
    false__ := ~js_lang.expr('false').
    :- export(null/1).
    :- pred null/1 :: js_object + (detfun, argsbox([unbox])).
    null := ~js_lang.expr('null').
}.

