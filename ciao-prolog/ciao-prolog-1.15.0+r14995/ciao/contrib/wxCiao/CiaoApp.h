#ifndef CIAO_APP_H
#define CIAO_APP_H

#include<wx/wx.h>
#include<map>
#include<string>
#include"CiaoWidget.h"
#include"CiaoNames.h"

/// This class is intended to be used as a singleton.  It should be
/// accessed through the static instance() function, which will return
/// the valid pointer to the class.
class CiaoApp : public wxApp
{
public:
  CiaoApp();
  virtual ~CiaoApp();
  static CiaoApp* instance();
  
  /// Register the callback to use when initializing the application.
  /// This should be the name of a Ciao function that is arity 0, and
  /// will build the main GUI window.  This function is called only
  /// once, and does not receive GUI events.
  void registerInitCallback(const char* func);
  
  /// getWidget is a function to provide a uniform namespace for all
  /// of the widget types supported by wxCiao.  On creation, all
  /// widgets are cast to a CiaoWidget pointer, which is then inserted
  /// into the main widget map with a unique identifier.  Every widget
  /// that is accessible from within Ciao will have an identifier in
  /// this map.  When a function requires the actual widget pointer
  /// associated with an id, it should call CiaoApp::getWidget with a
  /// pointer to a pointer to the type of object it expects the widget
  /// to be, and the handle it wishes to resolve.  If the widget's
  /// type matches either the exact type that the calling function
  /// expects or a subclass of that type, getWidget will return the
  /// pointer by assigning it to the target argument.  On success,
  /// getWidget returns true.  Failure is indicated by a return value
  /// of false.  Note that the pointer is not modified in the case of
  /// failure, so testing the resulting pointer for NULL will NOT
  /// work.
  template<typename CLASS>
  bool getWidget(CLASS** target, ciao::handle hdl)
  {
    std::map<ciao::handle,CiaoWidget>::iterator i = _widgets.find(hdl);

    if(i != _widgets.end())
      {
        CiaoWidget w = i->second;
        *target = dynamic_cast<CLASS*>(w);
        return(*target != NULL);
      }
    return(false);
  }

  /// getHandle returns the uniform widget handle mapped to a named
  /// widget specified by name.  If the widget is not found (ie. it
  /// was not named), getHandle returns ciao::InvalidHandle.
  ciao::handle getHandle(const std::string& name);
    
  /// addWidget inserts an object into the uniform widget map.  This
  /// object can be any pointer to a class, so long as it is cast to a
  /// CiaoWidget.  Note that the pointer MUST be a pointer to a class,
  /// because the dynamic_cast function will be used on it later.  If
  /// the target of the pointer does not have a vtable (ie. it's not a
  /// class instance) this will cause a program crash.  If the object
  /// is added, this function returns true, false otherwise.
  bool addWidget(CiaoWidget widget, ciao::handle hdl);

  /// mapName provides yet another way to get access to the widgets
  /// through a naming system.  Calling mapName will insert a record
  /// that indicates that the widget whose handle is hdl can also be
  /// referred to by the string name.
  void mapName(const std::string& name, ciao::handle hdl);

  /// nextHandle is responsible for generating unique widget handles
  /// for the application to use in its uniform namespace.  This is
  /// done by incrementing a static variable, so multithreaded
  /// applications should provide some kind of mutual exclusion for
  /// this function call.
  ciao::handle nextHandle();
  
  /// This function is used by wxWidgets to initialize the main GUI
  /// window when the application starts.  In wxCiao, it simply calls
  /// the init callback specified using CiaoApp::registerInitCallback
  virtual bool OnInit();

    
private:

  static CiaoApp* _instance;
  static ciao::handle _next_handle;
  char* init_callback;
  char* event_callback;

  std::map<ciao::handle,CiaoWidget> _widgets;
  std::map<std::string,ciao::handle> _names;
};

#endif
