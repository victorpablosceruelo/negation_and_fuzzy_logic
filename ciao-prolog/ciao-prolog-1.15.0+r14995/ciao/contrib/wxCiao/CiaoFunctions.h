#ifndef CIAO_FUNCTIONS_H
#define CIAO_FUNCTIONS_H
#include"CiaoNames.h"
extern "C"{
#include"ciao_prolog.h"
}
#ifndef IN
#define IN
#endif
#ifndef OUT
#define OUT
#endif

extern "C" 
{
  /// startEventLoop starts the main wxWidgets event loop (thus
  /// running the GUI).  Until this function is called, no events will
  /// be processed, and there will be no windows visible on the
  /// screen.  The argument is the name of the initialization event
  /// handler, so that the CiaoApp::OnInit function can call back into
  /// Ciao to build the main GUI window.
  void startEventLoop(IN const char* init_handler);

  /// setEventHandler associates an event handler function with a
  /// particular set of events and a window.  The events are passed in
  /// as a ciao list of atoms.  The atoms should be of a form that
  /// converts to strings (either identifiers or strings will do).
  /// The currently accepted strings are "menu", "button_press", and
  /// "spinbox".
  void setEventHandler(const char* func, ciao_term filter, ciao::handle win);

  /// setInitHandler sets the init handler.  Note that this function
  /// does not need to be called explicitly except in unusual
  /// circumstances, because this is handled automatically by the
  /// startEventLoop function described above.
  void setInitHandler(IN const char* name);

  /// showWindow makes a created window visible on the screen.  The
  /// input is a handle as returned from a previous call to
  /// createWindow.
  void showWindow(IN ciao::handle hdl);

  /// createWindow creates a new top level window on the desktop.
  /// This window can have a menu bar and a status bar (using
  /// createMenuBar and createStatusBar).  The name parameter is the
  /// text that iwll show up as the window's title, w is the window's
  /// width, and h is the window's height.  The value returned is a
  /// handle to the newly created window.
  ciao::handle createWindow(IN const char* name, int w, int h);

  /// createButton creates a push button in the parent window
  /// specified by the handle parent.  parent MUST be a valid handle
  /// for this function to work (buttons are not top level widgets).
  /// The text parameter is the string that will appear on the button.
  /// This function returns the handle for the newly created button.
  ciao::handle createButton(IN ciao::handle parent,
                            IN const char* text);

  /// createTextArea creates a multiline text edit area in the parent
  /// window specified by the par parameter.  The return value is the
  /// handle for the newly created text area.
  ciao::handle createTextArea(ciao::handle par);
  
  /// createLineEdit create a single line text edit in the parent
  /// window.  The return value is the handle of the newly created
  /// line edit.
  ciao::handle createLineEdit(ciao::handle par);

  /// createListBox creates a dropdown list box in the parent window
  /// specified by the par argument.  The handle of the newly created
  /// list box is returned.
  ciao::handle createListBox(ciao::handle par);

  /// createSpinBox creates a spinbox widget in the parent window
  /// specified by the par argument.  The new spinbox's handle is
  /// returned.
  ciao::handle createSpinBox(ciao::handle par);

  /// createHBox is used to create a horizontal widget layout area.
  /// It's parent can be either a wxWidget, or another layout area
  /// (either hbox or vbox).  The new hbox's handle is returned.
  ciao::handle createHBox(ciao::handle par);

  /// createVBox is used to create a vertical widget layout area.
  /// It's parent can be either a wxWidget or another layout area.
  /// The new VBox's handle is returned.
  ciao::handle createVBox(ciao::handle par);

  /// createMenuBar creates a menu bar.  The parent window is not
  /// specified here, because wxWidgets requires that a menu bar be
  /// fully created and populated before being added to a window.
  /// After the menu bar is finished, call setMenuBar to put it in the
  /// appropriate window.  The upshot of this is that you can use the
  /// same menu bar for several windows if you so choose.
  ciao::handle createMenuBar();

  /// createMenu creates a new menu to be associated with the
  /// previously created menubar.  The name parameter is the header of
  /// the menu that will be displayed in the menu bar.  The handle of
  /// the new menu is returned.
  ciao::handle createMenu(ciao::handle menubar, const char* name);

  /// addMenuItem creates a new item in the menu (this is the actual
  /// active component of the menu system).  When a menu item is
  /// clicked in the GUI, the application will receive the menu(id)
  /// event, where id is the handle returned by this function.
  ciao::handle addMenuItem(ciao::handle menu, const char* text);

  /// setMenuBar sets the window's menu bar to the the previously
  /// created menu bar.  See createMenuBar for a discussion of the
  /// purpose for this function.
  void setMenuBar(ciao::handle window, ciao::handle menubar);
  
  /// createSplitter creates a new splitter pane.  The parent must be
  /// a valid window handle.  Creating a splitter is a complicated
  /// business because of the way it's implemented in wxWidgets, so
  /// here is a quick breakdown of the events that must transpire
  /// (order is important!).  First, create the splitter using this
  /// function.  Next, create the child widgets of the splitter, using
  /// the new handle as the parent pointer for any widgets you wish to
  /// have inside it.  You should set things up so that there are only
  /// two direct children of this widget, because when you go to
  /// actually split it you can only specify two.  I recommend using
  /// some sort of layout widget to hold each half of the splitter
  /// (you'll probably need that anyway).  After that, you can call
  /// either splitVertically or splitHorizontally to actually cause
  /// the splitter to work properly.
  ciao::handle createSplitter(ciao::handle parent);

  /// splitVertically takes a splitter window, its left side widget,
  /// and its right side widget, and places a vertical bar in the
  /// middle to split the widgets.  All of the involved widgets must
  /// be created before calling this function.
  void splitVertically(ciao::handle splitter,ciao::handle left, ciao::handle right);

  /// splitHorizontally is identical to splitVertically, except that
  /// it takes the top and bottom widgets respectively and places a
  /// horizontal splitter bar between them.
  void splitHorizontally(ciao::handle splitter, ciao::handle top, ciao::handle bottom);

  /// dropPane unsplits a splitter window.  todrop is the handle of
  /// the widget of the pane that should be discarded.
  void dropPane(ciao::handle splitter, ciao::handle todrop);

  /// getSaveFile is a shortcut function that takes a wxWidgets file
  /// specificaiton filter and returns a path to a file that the user
  /// selected.  This function will pop up a file selection dialog in
  /// the style of the underlying widget system.
  const char* getSaveFile(const char* filter);

  /// getOpenFile is identical to getSaveFile, except that it is
  /// geared toward reading files instead of writing them.
  const char* getOpenFile(const char* filter);

  /// getText retrives the text from any widget that has user editable
  /// text.  The result is returned as a string.
  const char* getText(ciao::handle tw);

  /// setText sets the text contents of any text edit widget.
  void setText(ciao::handle tw, const char* newtext);

  /// getValue retrieves the numeric value from any widget that
  /// supports number entry by the user.  The return value is the
  /// number.
  int getValue(ciao::handle sb);

  /// SetValue sets the numeric value of any widget that takes numeric
  /// input from the user.  
  void setValue(ciao::handle sb, int value);

  /// messageBox pops up a simple alert message, with the title bar
  /// set to title, and the message set to msg.  The dialog will have
  /// an "OK" button for the user to press.  This is a blocking
  /// function call, so it can be used to control the flow of
  /// execution of the program.
  void messageBox(const char* title, const char* msg);
  
  /// createStatusBar creates a new status bar in the specified
  /// window.  Status bars can have several text fields, so you can
  /// specify how many to create with the fields parameter.  Most
  /// applications use either three or four fields.
  void createStatusBar(ciao::handle window, int fields);

  /// setStatusText sets the text in a particular field of the status
  /// bar.  The field is specified by the field parameter, and the
  /// text by the text parameter.  Any old text will be lost.
  void setStatusText(ciao::handle window, int field, const char* text);

  /// setMainSizer is used to tell a window what it's main layout
  /// widget is.  This function should be called after all of the
  /// widgets have been created and positioned relative to each other
  /// using layoutWidget.  If this is done before the above conditions
  /// are met, the layout will probably show up as tiny window with
  /// gobs of overlapping widgets that are impossible to use.  This is
  /// an artifact of the layout system in wxWidgets.
  void setMainSizer(ciao::handle window, ciao::handle sizer);

  /// layoutWidget is used to add a widget to a sizer.  For horizontal
  /// sizers, the widgets will be added from left to right, and for
  /// vertical sizers they will be added from top to bottom.  prop
  /// specifies the relative size of this widget compared with the
  /// other widgets in this layout container.  This is calculated by
  /// adding the prop values from all of the widgets in the container
  /// and calculating the percentage that this one will use.  For
  /// example, if you have three widgets with props of 1,2,and 3
  /// respectively, the first widget will be half the size of the
  /// second and on third the size of the last.  The last widget will
  /// take 50% of the layout, the second will take 34%,and the first
  /// will take the remaining 16%.
  void layoutWidget(ciao::handle box, ciao::handle widget, int prop, ciao_term flags);
}

#endif
