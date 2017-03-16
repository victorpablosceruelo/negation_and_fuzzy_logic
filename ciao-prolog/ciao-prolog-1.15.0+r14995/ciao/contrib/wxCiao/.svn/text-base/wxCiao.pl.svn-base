:- module(wxCiao,_,[foreign_interface,assertions,hiord]).

:- extra_linker_opts(['-L.']).
:- use_foreign_library([wxCiao]).

:- doc(nodoc,assertions).

:- doc(title,"wxCiao GUI Toolkit").
:- doc(subtitle, "An interface to wxWidgets from Ciao").
:- doc(author,"Erik Lee").
:- doc(copyright,"Erik Lee, 2005").

:- doc(usage, "wxCiao is used in this way: use_module(wxCiao).
You must have the wxCiao shared library compiled and installed in a
location on your library search path (I recommend placing a symlink to
it in /usr/local/lib, and installing it with the rest of Ciao).").

:- doc(summary, "wxCiao is a library that provides bindings for
many of the GUI widgets of the wxWidgets GUI toolkit for use in Ciao.
I provides many of the more common widgets, as well as layout
primitives to make window sizing easy.  Events are handled using a
callback based mechanism, and widget access is provided through a
naming scheme implemented in the prolog portion of the library.").

:- doc(module, "
@subsection{Introduction}

wxCiao provides the following GUI components:

@begin{enumerate}
@item Toplevel frame  (use create_window)
@item Menu bar  (use create_menu_bar)
@item Popup Menu (use create_menu)
@item Status bar (use create_status_bar)
@item Button (use create_button)
@item Splitter pane (use create_splitter)
@item Listbox widget (use create_listbox)
@item Spinbox (use create_spinbox)
@item Line edit (use create_line_edit)
@item Text area (use create_text_area)
@item Box layout widgets (use create_hbox or create_vbox)
@end{enumerate}

@subsection{Widget Naming}

wxCiao provides two distinct ways to access the widgets in the GUI.
The native language interface provides an integer handle for all
widgets that can be used to uniquely identify any item in the program.

However, because event handling is done using a callback mechanism,
and because the widget identifiers are assigned by the system, it is
difficult for user code to keep track of the integers defined in this
way when handling events.  To solve this, wxCiao provides a naming
mechanism that permanently associates an atom (such as ok_button) with
a widget handle.  This allows the user to treat the name atoms as
global variables so that all of the named widgets created by the
application during initialization can be uniquely accessed at any
other point during the program's execution.

All of the widget functions in wxCiao have two forms: The first is a
direct mapping to the native integer handle mechanism and can be
recognized by the use of a camel-caps naming convention.  The second
is intended to be a more convenient interface using the symbolic
names, and can be recognized by the use of underscores in the naming
convention.

Both methods are provided for the case that the user wishes to
implement a different (possibly more efficient) widget resolution
mechanism.

@subsection{Event Handling} 

Events are processed by defining an event handler for a specific set
of events and windows.  Currently, the supported event types are

@begin{enumerate}
@item button_press to handle the user clicking on button widgets
@item menu  to handle the user selecting an item from a popup menu
@item spinbox  to handle the value of a spinbox changing
@end{enumerate}

When a GUI event occurs, wxCiao will call the user-defined callback
with an argument of the form event_type(widget_name).  For example, if
the button named 'ok_button' is clicked, the user's event handler will
be called with the structure button_click(ok_button).

").

:- true pred setEventHandler(in(WidgetName), in(EventFilter), in(Handle)) ::
atm * list(atm) * int + foreign # "This function sets the event
handler for the window specified by @var{Handle} to be the function
specified by @var{WidgetName}.  @var{EventFilter} is a prolog list of atoms
containing any subset of the defined event handler filters.  See the
event handling information for the allowed values.  A more convenient
interface to this function is provided by set_event_handler.".

:- true pred startEventLoop(in(InitHandler)) :: atm + foreign #
"startEventLoop passes control to the wxWidgets event loop.  This
function will not return until the GUI has exited.  Events are passed
to the application according to the event handlers that the
application has registered.  A more convenient interface to this
function is provided by start_event_loop.".

:- true pred createWindow(in(WidgetName), in(Width), in(Height), go(Handle))
:: string * int * int * int + (foreign, returns(Handle)) #
"createWindow creates a top level window for the GUI.  @var{Handle}
will have the window's handle in it on return.  Use create_window for
a more convenient interface that provides naming for the window".

:- true pred createButton(in(Parent), in(Text), go(Handle)) :: int *
string * int + (foreign, returns(Handle)) # "createButton creates a
button widget.  @var{Parent} must be a handle from a valid container
widget (such as a window or other frame).  @var{Text} is the text that
will appear on the button.  @var{Handle} returns the button's handle.
Use create_button for a better interface to prolog with naming
support.".

:- true pred showWindow(in(Handle)) :: int + (foreign) # "showWindow
displays a created window on the desktop.  Because of the structure of
wxWidgets, it is better to perform the initial setup of a window
before allowing it to be displayed.  This function is called after a
window is initialized.  Note that if you don't call this function you
won't see anything!  Use show_window for a nicer interface in prolog.".

:- true pred createListBox(in(Parent), go(Handle)) :: int * int +
(foreign, returns(Handle)) # "createListBox creates a list box widget.
@var{Parent} must be a container widget, and @var{Handle} returns the
new list box's handle.  Use create_listbox for a nicer interface.".

:- true pred createSpinBox(in(Parent), go(Handle)) :: int * int +
(foreign, returns(Handle)) # "createSpinBox creates a spinbox widget.
@var{Parent} must be a valid container widget (this will be the parent
of the spinbox).  @var{Handle} returns the new spinbox's handle.  Use
create_spinbox for a nicer interface.".

:- true pred createTextArea(in(Parent), go(Handle)) :: int * int +
(foreign, returns(Handle)) # "createTextArea creates a multiline text
edit widget as a child of @var{Parent}, which must be a valid
container widget.  @var{Handle} returns the new text area's handle.
Use create_text_area for a nicer interface.".

:- true pred createLineEdit(in(Parent), go(Handle)) :: int * int +
(foreign, returns(Handle)) # "createLineEdit creates a single line
text edit widget as a child of @var{Parent}, which must be a valid
container widget.  @var{Handle} returns the new line edit's handle.
Use create_line_edit for a nicer interface.".

:- true pred createHBox(in(Parent), go(Handle)) :: int * int +
(foreign, returns(Handle)) # "createHBox creates a horizontal layout
widget as a child of @var{Parent}, which must be a valid container
widget.  @var{Handle} returns the new widget's handle.  Use
create_hbox for a nicer interface.".

:- true pred createVBox(in(Parent), go(Handle)) :: int * int +
(foreign, returns(Handle)) # "createHBox creates a vertical layout
widget as a child of @var{Parent}, which must be a valid container
widget.  @var{Handle} returns the new widget's handle.  Use
create_vbox for a nicer interface.".

:- true pred layoutWidget(in(Box), in(Widget), in(Proportion),
in(Flags)) :: int * int * int * list(atm) + (foreign) # "layoutWidget
takes a layout box (@var{Box}) and a widget to be positioned
(@var{Widget}), and applies the layout to the new widget.
@var{Proportion} specifies the relative size of this widget compared
to others in the layout box. Use layout_widget for a better
interface.".

:- true pred setMainSizer(in(Window), in(Sizer)) :: int * int +
foreign # "setMainSizer sets the top level layout widget (@var{Sizer})
for the specified window (@var{Window}).  Order of operations is
important for this function, so read the documentation for
set_main_sizer for a discussion of how to make this work right.".

:- true pred createMenuBar(go(Handle)) :: int + (foreign,
returns(Handle)) # "createMenuBar creates a menu bar widget.
@var{Handle} returns the new widget's handle.  Use create_menu_bar for
a nicer interface.".

:- true pred createMenu(in(Menubar), in(WidgetName), go(Handle)) :: int *
string * int + (foreign, returns(Handle))# "createMeny creates a menu
widget and attaches it to the menubar specified by
@var{Menubar}. @var{Handle} returns the new widget's handle.  Use
create_menu for a nicer interface.".

:- true pred addMenuItem(in(Menu), in(WidgetName), go(Handle)) :: int *
string * int + (foreign, returns(Handle)) # "addMenuItem adds a menu
item named @var{WidgetName} to @var{Menu}, which is a previously created
menu.  @var{Handle} returns the new menu item's handle.  Use
create_menu_item for a nicer interface.".

:- true pred setMenuBar(in(Window), in(Menubar)) :: int * int +
foreign # "setMenuBar sets the main menu bar for @var{Window} to
@var{MenuBar}.  Both widgets must exist before the call.  Order is
important, so read the documentation for set_menu_bar for a better
idea of what to do here.".

:- true pred createStatusBar(in(Window), in(Fields)) :: int * int +
foreign# "createStatusBar creates a status bar widget for the toplevel
window specified by @var{Parent}.  @var{Handle} returns the new
widget's handle.  Use create_status_bar for a nicer interface.".

:- true pred setStatusText(in(Window), in(Field), in(Text)) :: int *
int * string + foreign # "setStatusText sets the text in field number
@var{Field} of @var{Window}'s status bar to be @var{Text}.  Use
set_status_text for a better interface.".

:- true pred createSplitter(in(Parent), go(Handle)) :: int * int +
(foreign, returns(Handle))# "createSplitter creates a splitter widget
as a child of @var{Parent}, which must be a valid container widget.
@var{Handle} returns the new widget's handle.  Use create_splitter for a
nicer interface.".

:- true pred splitVertically(in(Splitter), in(Left), in(Right)) :: int
* int * int + foreign # "splitVertically takes an existing splitter
pane (@var{Splitter}) and two existing child widgets of the splitter
(@var{Left} and @var{Right}) and splits the pane into a left and right
orientation, with the child widgets placed as their name indicates.
Order is important so read about split_vertically before using this
function.".

:- true pred splitHorizontally(in(Splitter), in(Top), in(Bottom)) ::
int * int * int + foreign # "splitHorizontally is identical to
splitVertically, except that it creates an over/under orientation for
the splitter pane.  See split_horizontally for a nicer interface.".

:- true pred dropPane(in(Splitter), in(ToDrop)) :: int * int + foreign
# "dropPane tells the splitter widget @var{Splitter} to show only one
pane by dropping the child widget @var{ToDrop} from its display.  See
drop_pane for a nicer interface.".

:- true pred getText(in(TextWidget), go(Text)) :: int * string +
(foreign, returns(Text)) # "getText retrieves the text from any text
entry widget (@var{TextWidget}) and returns it in @var{Text}.  See
get_text for a better interface.".

:- true pred getValue(in(NumWidget), go(Num)) :: int * int + (foreign,
returns(Num)) # "getValue retrieves the numeric value of any numeric
input widget (@var{NumWidget}) and returns it in @var{Num}.  See
get_value for a nicer interface.".

:- true pred messageBox(in(Title), in(Msg)) :: string * string +
(foreign) # "messageBox pops up a simple alert message box with a
short message specified by @var{Msg}.  The alert box's title bar will
display @var{Title}.  See message_box for a better interface.".

:- true pred getSaveFile(in(Filter), go(FileWidgetName)) :: string * string
+ (foreign, returns(FileWidgetName)) # "getSaveFile takes a filter string
@var{Filter} and returns a string in @var{FileWidgetName} that was selected
by the user.  This function will pop up a file selection dialog in
whatever style is native to the system the program is running on.  See
get_save_file for a better interface.".

:- true pred getOpenFile(in(Filter), go(FileWidgetName)) :: string * string
+ (foreign, returns(FileWidgetName)) # "getOpenFile is identical to
getSaveFile, except that it will display a read-oriented file dialog
on platforms that support a different style from the save dialog.  See
get_open_file for a  nicer interface.".

:- true pred setText(in(TextCtrl), in(Text)) :: int * string + foreign
# "setText sets the text of the text entry widget @var{TextCtrl} to be
@var{Text}.  See set_text for a better interface.".

:- true pred setValue(in(NumWidget), in(Value)) :: int * string +
foreign # "set_value sets the number in the numeric input widget
@var{NumWidget} to @var{Value}.  See set_value for a better
interface.".

:- true pred createLabel(in(Text), go(Handle)) :: string * int +
(foreign, returns(Handle))# "createLabel creates a text label widget
as a child of @var{Parent}, which must be a valid container widget.
@var{Handle} returns the new widget's handle.  Use create_label for a
nicer interface.".

:- true pred setLabelText(in(Handle), in(Text)) :: int * string +
foreign # "setLabelText allows the user to change the text displayed
by a static text label (@var{Handle}) to @var{Text}.  See
set_label_text for a nicer interface.".


:- meta_predicate set_event_handler(_,pred(1),_).
:- meta_predicate start_gui(pred(0)).

:- true pred handler(Id,Pred) :: int * atm # "handler is used to map
an event handler to the window it handles events for.".

:- data handler/2.

:- true pred named(WidgetName, Handle) :: atm * int # "named is used
to provide a mapping between widget names and handles.".

:- data named/2.

:- true pred initializer(Pred) :: atm # "initializer keeps the
applications initialization function for the time when it will be
called by wxWidgets".

:- data initializer/1.

:- true pred handle_event(in(WidgetName),in(Id),in(Window)) :: atm * int *
int # "handle_event is called by the C++ side of wxCiao when a user
interface event occurs that has a defined event handler.  It is
responsible for recasting the information given by the C++ code to a
form that is usable by the user's code (translating ids to names,
etc).".

handle_event(WidgetName,Id,Window) :- named(WidgetWidgetName,Id),
        handler(Window,Handler),
        Evt =.. [WidgetName,WidgetWidgetName],
        Handler(Evt).


:- true pred get_widget(go(Id), in(WidgetName)) :: int * atm +
returns(Id).  :- true pred get_widget(in(Id), go(WidgetName)) :: int *
atm + returns(WidgetName) # "get_widget provides the user with an
inteface to translage back and forth between widget names and
handles.".

get_widget(Id,WidgetName) :- named(WidgetName,Id).


:- true pred set_event_handler(in(Window),in(Pred),in(Filter)) :: atm
* atm * list(atm) # "set_event_handler sets the handler for the window
named @var{Window} to the user predicate @var{Pred}.  @var{Filter} is
a list of events that the handler is intended to handle.  For the full
set of known events, see the event handling documentation above.".

set_event_handler(Window,Pred,Filter) :- named(Window,Win),
        setEventHandler('wxCiao:handle_event',Filter,Win),
        asserta_fact(handler(Win,Pred)).

:- true pred create_window(in(WidgetName),in(Title),in(X),in(Y)) ::
atm * string * int * int # "create_window creates a new top level
window with title @var{Title} and size @var{X} by @var{Y}.  The name
@var{WidgetName} will be recorded so that the window can be reference
by this identifier in the future.".

create_window(WidgetName,Title,X,Y) :- createWindow(Title,X,Y,Handle),
        asserta_fact(named(WidgetName,Handle)).


:- true pred create_button(in(WidgetName), in(Text),
in(WindowWidgetName)) :: atm * string * atm # "create_button creates a
new push button as a child of the window named @var{WindowWidgetname}.
@var{Text} will be displayed on the button, and the button can be
referred to in the future using the name @var{WidgetName}." .

create_button(WidgetName,Text,WindowWidgetName) :- display(WidgetName),nl,display(WindowWidgetName),nl,
        named(WindowWidgetName,WinH),!,
        display(WinH),nl,
        createButton(WinH, Text, Button),
        asserta_fact(named(WidgetName,Button)).


:- true pred create_line_edit(in(WidgetName), in(WindowWidgetName)) ::
atm * atm # "create_line_edit creates a new single line text control
as a child of the window specified by @var{WindowWidgetName}.  The
line edit can be referred to in the future using @var{WidgetName}.".

create_line_edit(WidgetName,WindowWidgetName) :- named(WindowWidgetName, Win),!,
        createLineEdit(Win,Line),
        asserta_fact(named(WidgetName,Line)).

:- true pred create_spin_box(in(WidgetName), in(WindowWidgetName)) ::
atm * atm # "create_spin_box creates a new spinbox control as a child
of the window specified by @var{WindowWidgetName}.  The spinbox can be
referred to in the future using @var{WidgetName}.".

create_spin_box(WidgetName,WindowWidgetName) :- named(WindowWidgetName, Win),!,
        createSpinBox(Win,Sb),
        asserta_fact(named(WidgetName,Sb)).

:- true pred create_splitter(in(WidgetName), in(WindowWidgetName)) ::
atm * atm # "create_splitter creates a new splitter pane as a child of
the window specified by @var{WindowWidgetName}.  The splitter can be
referred to in the future using @var{WidgetName}.".

create_splitter(WidgetName,WindowWidgetName) :- named(WindowWidgetName, Win),!,
        createSplitter(Win,Splitter),
        asserta_fact(named(WidgetName,Splitter)).

:- true pred split_vertically(in(Splitter), in(Left), in(Right)) ::
atm * atm * atm # "split_vertically causes the splitter identified by
@var{Splitter} to divide itself into a left and right half.  @var{Left}
specifies the child widget that will be on the left, and @var{Right} is
the right.  @var{S1} and @var{S2} must be created with the splitter as
their parent, and must exist before calling this function.".

split_vertically(Splitter, S1, S2) :- named(Splitter, Spl),
        named(S1, Wid1),
        named(S2, Wid2),
        splitVertically(Spl,Wid1,Wid2).


:- true pred split_horizontally(in(Splitter), in(Top), in(Bottom)) ::
atm * atm * atm # "split_horizontally functions identically to
split_vertically, except that the widgets are in an over/under
configuration." .

split_horizontally(Splitter, S1, S2) :- named(Splitter, Spl),
        named(S1, Wid1),
        named(S2, Wid2),
        splitHorizontally(Spl,Wid1,Wid2).

:- true pred create_text_area(in(WidgetName), in(WindowWidgetName)) ::
atm * atm # "create_text_area creates a new multi-line text control as
a child of the window specified by @var{WindowWidgetName}.  The text
edit can be referred to in the future using @var{WidgetName}.".

create_text_area(WidgetName,WindowWidgetName) :- named(WindowWidgetName, Win),!,
        createTextArea(Win,Text),
        asserta_fact(named(WidgetName,Text)).

:- true pred create_vbox(in(WidgetName), in(ParentWidgetName)) :: atm
* atm # "create_vbox creates a new vertical layout widget as a child
of the widget specified by @var{ParentWidgetName}.  The parent widget
can be either a window or another layout widget.  The layout can be
referred to in the future using @var{WidgetName}.".

create_vbox(WidgetName, WindowWidgetName) :- named(WindowWidgetName, Win),!,
        createVBox(Win,VBox),
        asserta_fact(named(WidgetName,VBox)).

:- true pred create_hbox(in(WidgetName), in(ParentWidgetName)) :: atm
* atm # "create_hbox creates a new horizontal layout widget as a child
of the widget specified by @var{ParentWidgetName}.  The parent widget
can be either a window or another layout widget.  The layout can be
referred to in the future using @var{WidgetName}.".

create_hbox(WidgetName, WindowWidgetName) :- named(WindowWidgetName, Win),!,
        createHBox(Win,Widget),
        asserta_fact(named(WidgetName,Widget)).

:- true pred create_menu_bar(in(WidgetName)) :: atm # "create_menu_bar
creates a new menu bar widget.  The menu bar can be referred to in the
future using @var{WidgetName}.  Note that there is no parent specified
for this widget -- it must be created and initialized before inserting
it into a window using the set_menu_bar function.".

create_menu_bar(WidgetName) :- createMenuBar(Widget),
        asserta_fact(named(WidgetName,Widget)).

:- true pred create_menu(in(WidgetName), in(Label),
in(MenuBarWidgetName)) :: atm * string * atm # "create_menu creates a
new menu and attaches it to the menu bar referred to as
@var{MenuBarWidgetName}.  @var{Label} will be displayed as the text
for this menu entry in the menu bar.  The menu can be referred to in
the future using @var{WidgetName}.".

create_menu(WidgetName, Label, MenuBarWidgetName) :- named(MenuBarWidgetName, Bar),!,
        createMenu(Bar,Label,Widget),
        asserta_fact(named(WidgetName,Widget)).

:- true pred create_menu_item(in(WidgetName), in(Label),
in(MenuWidgetName)) :: atm * string * atm # "create_menu_item creates
a new menu item and appends it to the menu referred to as
@var{MenuWidgetName}.  @var{Label} will be displayed as the text for
this menu entry in the menu.  The menu entry can be referred to in the
future using @var{WidgetName}.".

create_menu_item(WidgetName, Label, MenuWidgetName) :- named(MenuWidgetName, Menu),!,
        addMenuItem(Menu,Label,Widget),
        asserta_fact(named(WidgetName,Widget)).

:- true pred create_status_bar(in(WindowWidgetName), in(Fields)) ::
atm * int # "create_status_bar adds a status bar to the window specified
by @var{WindowWidgetName}.  The status bar will be created with
@var{Fields} segments, each of which can have different text set using
set_status_text.".

create_status_bar(WindowWidgetName, Fields) :- named(WindowWidgetName, Win),!,
        createStatusBar(Win,Fields).

:- true pred set_status_text(in(WindowWidgetName), in(Field),
in(Text)) :: atm * int * string # "set_status_text displays the text
@var{Text} in the @var{Field} segment of the status bar for the window
identified by @var{WindowWidgetName}." .

set_status_text(WindowWidgetName, Field, Text) :- named(WindowWidgetName, Win),!,
        setStatusText(Win,Field,Text).

:- true pred layout(in(WidgetName), in(Parent), in(Proportion),
in(Flags)) :: atm * atm * int * list(atm) # "layout adds
@var{WidgetName} to the layout widget @var{Parent}. @var{Proportion}
specifies the relative size of @var{WidgetName} in comparison to the
other widgets in @var{Parent}.  For example, if there are three
widgets in a horizontal layout with proportion values of 1, 2, and 3,
then the left widget will have 1/6 of the total space, the middle
widget will have 1/3 of the space, and the right widget will have 1/2
of the space.  @var{Flags} gives hints to the window system as to how
to treat this widget.  Currently accepted flags are:

@begin{enumerate}
@item align_center
@item align_right
@item align_left
@item align_bottom
@item align_center_vertical
@item align_center_horizontal
@item expand
@item shaped (keep the aspect ratio of this widget when sizing)
@end{enumerate}" .

layout(WidgetName, Parent, Proportion, Flags) :- named(Parent, Par),!,
        named(WidgetName, Child),!,
        layoutWidget(Par, Child, Proportion, Flags).

:- true pred set_menu_bar(in(Window), in(Menu)) :: atm * atm #
"set_menu_bar sets the main window bar of the window identified by
@var{Window} to @var{Menu} (again an identifier).  The menu bar must
be fully created and filled before calling this function to ensure
correct behavior on all platforms." .

set_menu_bar(Window, Menu) :- named(Window, Win),!,
        named(Menu, Men),!,
        setMenuBar(Win,Men).


:- true pred set_main_sizer(in(Window), in(Sizer)) :: atm * atm #
"set_main_sizer sets the top level layout widget of @var{Window} to be
the layout widget identified by @var{Sizer}.  This must be done AFTER
the layout widget is filled with its child widgets, otherwise you will
probably get a tiny window with all of its widgets on top of each
other." .

set_main_sizer(Window, Sizer) :- named(Window,Win),!,
        named(Sizer, Size),!,
        setMainSizer(Win,Size).


:- true pred show(in(Window)) :: atm # "show simply takes the window
identified by @var{Window} and displays it on the desktop.  This is
done to make the interaction with wxWidgets work properly.  Make sure
the window is initialized before calling show on it.".

show(Window) :- named(Window,Win),!,
        showWindow(Win).

:- true pred start_gui(in(Pred)) :: atm # "start_gui takes a callback
@var{Pred} as an argument and calls the main event loop of wxWidgets.
@var{Pred} is used to perform the initial GUI setup when wxWidgets
requests it, so this function should build the main window, populate
it with widgets, and show it.  It should also register the main
window's event handler using set_event_handler." .

start_gui(Pred) :- asserta_fact(initializer(Pred)), startEventLoop('wxCiao:init').

:- true pred init # "init is used internally to redirect the
initialization call from wxWidgets to the user's code.  Do not call
this function from within prolog.".

init :- initializer(Init), Init.

:- true pred message_box(in(Title), in(Message)) :: string * string #
"message_box pops up an alert box in the GUI with the message given by
@var{Message}.  The string @var{Title} will be displayed in the title
bar of the dialog.  The user will be able to press OK to continue
execution.  This function will block until the user has pressed OK, so
execution will pause until the user acknowledges the message." .

message_box(Title,Message) :- messageBox(Title,Message).


:- true pred get_text(in(Widget), go(Result)) :: atm * string +
returns(Result) # "get_text gets the text from any text input widget
(identified by @var{Widget}) and returns it in @var{Result}.  " .

get_text(Widget,Result) :- named(Widget, Id),
        getText(Id,Result).

:- true pred get_value(in(Widget), go(Result)) :: atm * int +
returns(Result) # "get_value is just like get_text, except that it
works for integer input widgets.".

get_value(Widget,Value) :- named(Widget,Id),
        getValue(Id,Value).


:- true pred get_open_file(in(Filter), go(File)) :: string * string +
returns(File) # "get_open_file pops up a file open dialog in the
native style of the underlying platform for the user to select a file
from.  Files will be filtered according to the string @var{Filter}
(see wxWindows.org for details on this string's format).  The selected
file with full path is returned in @var{File}.  This function will not
return until the user has selected a file.".

get_open_file(Filter, File) :- getOpenFile(Filter,File).

:- true pred get_save_file(in(Filter), go(File)) :: string * string +
returns(File) # "get_save_file is identical to get_open_file except
that, on platforms that support a difference, it will use a dialog
targeted toward writing a file rather than reading one.".

get_save_file(Filter,File) :- getSaveFile(Filter,File).
