#ifndef CIAO_FRAME_H
#define CIAO_FRAME_H

#include<wx/wx.h>
#include"CiaoNames.h"
#include<string>
#include<map>
#include<wx/spinctrl.h>


/// The CiaoFrame object exists primarily to provide event
/// demultiplexing for the widget system.  Each window can have a
/// separate set of event handlers mapped to different event types.
/// If the window receives an event for which there is no Ciao handler
/// specified, it skips it and uses whatever default behavior the
/// library specifies.  This was done to limit the calling between
/// Ciao and C++ as much as possible.  All callback functions must
/// have a fully qualified name, including the function's module name.
class CiaoFrame : public wxFrame
{
public:
  /// The constructor.  parent can be NULL if the window should be a
  /// toplevel window.  id should be a widget handle obtained from
  /// CiaoApp::nextHandle.  name, pos, and size are used internally by
  /// the wxWidgets library for setting the window title, position,
  /// and size, respectively.
  CiaoFrame(wxWindow* parent, ciao::handle id,
            const char* name, wxPoint pos, wxSize size);

  /// onButtonPress is called by wxWidgets whenever a button is
  /// pressed in this window.  If the application wishes to be
  /// notified of the event, it should set the event handler for
  /// "button_press".  If there is no handler set, the event is
  /// skipped and passed on to wxWidgets for further handling.
  void onButtonPress(wxCommandEvent& evt);

  /// onMenu handles menu selection events.  If the application wishes
  /// to be informed of menu selection events, it should register an
  /// event handler for the "menu" event.
  void onMenu(wxCommandEvent& evt);

  /// onSpin handles spinbox change events.  If the applicaiton wishes
  /// to be informed of spinbox changes, it should register an event
  /// handler for the "spinbox" event.
  void onSpin(wxSpinEvent& evt);

  /// addEventHandler registers Ciao functions to be called when
  /// certain events take place.  The currently supported events are
  /// button presses ("button_press"), menu selection ("menu"), and
  /// spinbox changes ("spinbox").  The function specified must have a
  /// fully qualified name (including module name).
  void addEventHandler(const std::string& func , const std::string& event);

private:
  void handleEvent(wxEvent&,const std::string&);
  std::map<std::string,std::string> _handlers;

  DECLARE_EVENT_TABLE()
};
#endif
