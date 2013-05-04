#include"CiaoApp.h"
#include"CiaoFunctions.h"
#include"CiaoFrame.h"
#include<wx/app.h>
#include<wx/spinctrl.h>
#include<wx/sizer.h>
#include<wx/splitter.h>
#include<wx/listctrl.h>

#include<string>
using namespace std;

#include"ciao_prolog.h"


IMPLEMENT_APP_NO_MAIN(CiaoApp)

#define EC extern "C" 


EC void startEventLoop(const char* name)
{
  CiaoApp* app = CiaoApp::instance();
  app->registerInitCallback(name);

  char* argv[] = {"wxCiao"};
  int argc=1;

  wxEntry(argc,argv);
}


EC void setEventHandler(const char* func, ciao_term filter, ciao::handle hdl)
{
  CiaoFrame* win;
  if(!CiaoApp::instance()->getWidget(&win, hdl))
    return;

  printf("setting filters\n");
  char* name;
  for(ciao_term ls = filter; ciao_is_list(ls); ls = ciao_list_tail(ls))
    {
      if(ciao_is_atom(ciao_list_head(ls)))
        win->addEventHandler(func,ciao_atom_name(ciao_list_head(ls)));
    }
}

EC void setInitHandler(const char* name)
{
  printf("setInitHandler(%s)\n", name);
  CiaoApp::instance()->registerInitCallback(name);
}

EC ciao::handle createWindow(const char* name, int width, int height)
{
  printf("createWindow(%s)\n", name);
  ciao::handle hdl = CiaoApp::instance()->nextHandle();

  CiaoFrame* frame = new CiaoFrame(NULL, hdl, name, wxDefaultPosition, wxSize(width,height));
  CiaoApp::instance()->mapName(name,hdl);
  CiaoApp::instance()->addWidget(frame,hdl);
  return(hdl);
}

EC void showWindow(ciao::handle win)
{
  wxWindow* window;
  if(!CiaoApp::instance()->getWidget(&window,win))
    return;

  window->Show();
}

EC ciao::handle createButton(ciao::handle parent, const char* text)
{
  printf("createButton(%s)\n", text);
  ciao::handle hdl = CiaoApp::instance()->nextHandle();
  wxWindow* par;
  if(!CiaoApp::instance()->getWidget(&par,parent))
    {
      return(ciao::InvalidHandle);
    }

  CiaoApp::instance()->mapName(text,hdl);
  wxButton* button = new wxButton(par,hdl,text);
  CiaoApp::instance()->addWidget(button,hdl);
  return(hdl);
}

EC ciao::handle createTextArea(ciao::handle parent)
{
  printf("createTextArea\n");
  ciao::handle hdl = CiaoApp::instance()->nextHandle();
  wxWindow* par;
  if(!CiaoApp::instance()->getWidget(&par,parent))
    {
      return(ciao::InvalidHandle);
    }
  wxTextCtrl * txt = new wxTextCtrl(par, hdl, "", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE);
  CiaoApp::instance()->addWidget(txt,hdl);
  return(hdl);
}

EC ciao::handle createLineEdit(ciao::handle parent)
{
  printf("createLineEdit\n");
  ciao::handle hdl = CiaoApp::instance()->nextHandle();
  wxWindow* par;
  if(!CiaoApp::instance()->getWidget(&par,parent))
    {
      return(ciao::InvalidHandle);
    }
  wxTextCtrl * txt = new wxTextCtrl(par, hdl);
  CiaoApp::instance()->addWidget(txt,hdl);
  return(hdl);
}

EC ciao::handle createListBox(ciao::handle parent)
{
  printf("createListBox\n");
  ciao::handle hdl = CiaoApp::instance()->nextHandle();
  wxWindow* par;
  if(!CiaoApp::instance()->getWidget(&par,parent))
    {
      return(ciao::InvalidHandle);
    }
  wxListCtrl* l = new wxListCtrl(par,hdl);
  CiaoApp::instance()->addWidget(l,hdl);
  return(hdl);
}

EC ciao::handle createSpinBox(ciao::handle parent)
{
  printf("createSpinBox\n");
  ciao::handle hdl = CiaoApp::instance()->nextHandle();
  wxWindow* par;
  if(!CiaoApp::instance()->getWidget(&par,parent))
    {
      return(ciao::InvalidHandle);
    }

  wxSpinCtrl* s = new wxSpinCtrl(par,hdl);
  CiaoApp::instance()->addWidget(s,hdl);
  return(hdl);
}

EC ciao::handle createHBox(ciao::handle parent)
{
  printf("createHBox\n");
  ciao::handle hdl = CiaoApp::instance()->nextHandle();
  wxWindow* par;
  if(!CiaoApp::instance()->getWidget(&par,parent))
    {
      return(ciao::InvalidHandle);
    }
 
  wxBoxSizer* box = new wxBoxSizer(wxHORIZONTAL);
  CiaoApp::instance()->addWidget(box,hdl);
  return(hdl);
}

EC ciao::handle createVBox(ciao::handle parent)
{
  printf("createHBox\n");
  ciao::handle hdl = CiaoApp::instance()->nextHandle();
  wxWindow* par;
  if(!CiaoApp::instance()->getWidget(&par,parent))
    {
      return(ciao::InvalidHandle);
    }
 
  wxBoxSizer* box = new wxBoxSizer(wxVERTICAL);
  CiaoApp::instance()->addWidget(box,hdl);
  return(hdl);
}

EC void layoutWidget(ciao::handle box, ciao::handle widget, int proportion, ciao_term layout_list)
{
  unsigned long flags = 0;
  
  wxBoxSizer* sizer;
  if(!CiaoApp::instance()->getWidget(&sizer, box))
    {
      return;
    }

  // parse the layout list flags
  for(ciao_term ls = layout_list; ciao_is_list(ls); ls = ciao_list_tail(ls))
    {
      string flag = ciao_atom_name(ciao_list_head(ls));
      printf("Flag: %s\n", flag.c_str());
      if(flag == "align_center")
        flags |= wxALIGN_CENTER;
      else if(flag == "align_left")
        flags |= wxALIGN_LEFT;
      else if(flag == "align_right")
        flags |= wxALIGN_RIGHT;
      else if(flag == "align_bottom")
        flags |= wxALIGN_BOTTOM;
      else if(flag == "align_center_vertical")
        flags |= wxALIGN_CENTER_VERTICAL;
      else if(flag == "align_center_horizontal")
        flags |= wxALIGN_CENTER_HORIZONTAL;
      else if(flag == "expand")
        flags |= wxEXPAND;
      else if(flag == "shaped")
        flags |= wxSHAPED;
      else
        printf("Unrecognized flag: %s\n", flag.c_str());
    }
  if(flags == 0)
    {
      flags = wxALIGN_LEFT | wxALIGN_TOP | wxEXPAND | wxALIGN_CENTER_VERTICAL | wxALIGN_CENTER_HORIZONTAL;
    } 

  wxWindow* win;
  wxSizer* bx;

  if(CiaoApp::instance()->getWidget(&win, widget))
    {
      sizer->Add(win, proportion, flags);
    }
  else if(CiaoApp::instance()->getWidget(&bx,widget))
    {
      sizer->Add(bx, proportion, flags);
    }
  else
    printf("Failed to layout widget\n");
}

EC void setMainSizer(ciao::handle window, ciao::handle bs)
{
  wxWindow* win;
  wxSizer* box;
  if(!CiaoApp::instance()->getWidget(&win,window) ||
     !CiaoApp::instance()->getWidget(&box,bs))
    return;

  win->SetSizer(box);
  box->SetSizeHints(win);
}

EC ciao::handle createMenuBar()
{
  printf("createMenuBar\n");
  ciao::handle hdl = CiaoApp::instance()->nextHandle();
  
  wxMenuBar* mb = new wxMenuBar;
  CiaoApp::instance()->addWidget(mb,hdl);
  return(hdl);
}

EC ciao::handle createMenu(ciao::handle mb, const char* name)
{
  printf("createMenu\n");
  ciao::handle hdl = CiaoApp::instance()->nextHandle();
  wxMenuBar* mbar;
  if(!CiaoApp::instance()->getWidget(&mbar,mb))
    return(ciao::InvalidHandle);
  
  wxMenu* menu = new wxMenu();
  mbar->Append(menu,name);
  CiaoApp::instance()->addWidget(menu, hdl);
  return(hdl);
}

EC ciao::handle addMenuItem(ciao::handle mb, const char* name)
{
  printf("addMenuItem\n");
  ciao::handle hdl = CiaoApp::instance()->nextHandle();
  wxMenu* menu;
  if(!CiaoApp::instance()->getWidget(&menu,mb))
    return(ciao::InvalidHandle);
  
  menu->Append(hdl,name);
  return(hdl);
}

EC void setMenuBar(ciao::handle window, ciao::handle menubar)
{
  printf("setMenuBar\n");
  ciao::handle hdl = CiaoApp::instance()->nextHandle();
  wxMenuBar* mbar;
  if(!CiaoApp::instance()->getWidget(&mbar,menubar))
    return;
  wxFrame* frame;
  if(!CiaoApp::instance()->getWidget(&frame,window))
    return;
 
  frame->SetMenuBar(mbar);
}

EC void createStatusBar(ciao::handle window, int fields)
{
  printf("createStatusBar\n");
  wxFrame* par;
  if(!CiaoApp::instance()->getWidget(&par,window))
    {
      return;
    }
 
  par->CreateStatusBar(fields);
}

EC void setStatusText(ciao::handle window, int field, const char* text)
{
  printf("setStatusText\n");
  wxFrame* par;
  if(!CiaoApp::instance()->getWidget(&par,window))
    {
      return;
    }
  par->SetStatusText(text,field);
}

EC ciao::handle createSplitter(ciao::handle parent)
{
  printf("createSplitter\n");
  ciao::handle hdl = CiaoApp::instance()->nextHandle();
  wxWindow* par;
  if(!CiaoApp::instance()->getWidget(&par,parent))
    {
      return(ciao::InvalidHandle);
    }

  wxSplitterWindow* sp = new wxSplitterWindow(par,hdl);
  CiaoApp::instance()->addWidget(sp,hdl);
  return(hdl);

}

EC void splitVertically(ciao::handle spl, ciao::handle left, ciao::handle right)
{
  printf("splitVertically\n");
  wxSplitterWindow* par;
  wxWindow* l,*r;
  if(!CiaoApp::instance()->getWidget(&par,spl) ||
     !CiaoApp::instance()->getWidget(&l,left) ||
     !CiaoApp::instance()->getWidget(&r,right))
    {
      return;
    }
 
  par->SplitVertically(l,r);
}

EC void splitHorizontally(ciao::handle spl, ciao::handle top, ciao::handle bottom)
{
  printf("splitHorizontally\n");
  wxSplitterWindow* par;
  wxWindow* t,*b;
  if(!CiaoApp::instance()->getWidget(&par,spl) ||
     !CiaoApp::instance()->getWidget(&t,top) ||
     !CiaoApp::instance()->getWidget(&b,bottom))
    {
      return;
    }
 
  par->SplitHorizontally(t,b);
}

EC void dropPane(ciao::handle spl,ciao::handle todrop)
{
  printf("dropPane\n");
  wxSplitterWindow* par;
  if(!CiaoApp::instance()->getWidget(&par,spl))
    {
      return;
    }
  wxWindow* drop;
  if(!CiaoApp::instance()->getWidget(&drop,todrop))
    return;
 
  par->Unsplit(drop);
}

EC const char* getText(ciao::handle tw)
{
  wxTextCtrl* text;
  if(!CiaoApp::instance()->getWidget(&text,tw))
    {
      return("");
    }
  char* res;
  if(text->GetValue().c_str())
    res = strdup(text->GetValue().c_str());
  else
    res = strdup("");
  return(res);
}

EC void setText(ciao::handle tw, const char* newtext)
{
  wxTextCtrl* text;
  if(!CiaoApp::instance()->getWidget(&text,tw))
    {
      return;
    }
  text->SetValue(newtext);
}

EC int getValue(ciao::handle box)
{
  wxSpinCtrl* sb;
  if(!CiaoApp::instance()->getWidget(&sb, box))
    {
      return(ciao_integer(0));
    }
  return(ciao_integer(sb->GetValue()));
}

EC void setValue(ciao::handle box, int newvalue)
{
  wxSpinCtrl* sb;
  if(!CiaoApp::instance()->getWidget(&sb, box))
    {
      return;
    }
  sb->SetValue(newvalue);
}

EC void messageBox(const char* title, const char* msg)
{
  printf("messageBox(%s,%s)\n",title,msg);
  wxMessageBox(msg,title);
}

EC const char* getSaveFile(const char* filter)
{
  wxString result;
  result = wxFileSelector("Select File to Save","","","",filter);
  char* res;
  if(result.c_str())
    res = strdup(result.c_str());
  else
    res = strdup("");
  return(res);
}

EC const char* getOpenFile(const char* filter)
{
  wxString result = wxFileSelector("Select File to Open","","","",filter);
  char* res;
  if(result.c_str())
    res = strdup(result.c_str());
  else
    res = strdup("");
  return(res);
}
