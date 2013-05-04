#include"CiaoFrame.h"
#include"CiaoFunctions.h"
using namespace std;

BEGIN_EVENT_TABLE(CiaoFrame, wxFrame)
  EVT_BUTTON(-1, CiaoFrame::onButtonPress)
  EVT_MENU(-1, CiaoFrame::onMenu)
  EVT_SPINCTRL(-1,CiaoFrame::onSpin)
  END_EVENT_TABLE()
  ;


CiaoFrame::CiaoFrame(wxWindow* parent, ciao::handle id, const char* name, 
                     wxPoint pos, wxSize size) : 
  wxFrame(parent,id,name,pos,size)
{
  printf("CiaoFrame::CiaoFrame\n");
}

void CiaoFrame::onButtonPress(wxCommandEvent& evt)
{
  handleEvent(evt, "button_press");
}

void CiaoFrame::onMenu(wxCommandEvent& evt)
{
  handleEvent(evt, "menu");
}

void CiaoFrame::onSpin(wxSpinEvent& evt)
{
  handleEvent(evt,"spinbox");
}

void CiaoFrame::handleEvent(wxEvent& evt, const string& name)
{
  map<string,string>::iterator item = _handlers.find(name);
  if(item == _handlers.end())
    {
      evt.Skip();
      return;
    }
  
  ciao_term id = ciao_integer(evt.GetId());
  ciao_term win = ciao_integer(GetId());
  ciao_term nam = ciao_atom(name.c_str());
  ciao_commit_call(item->second.c_str(), 3, nam, id, win);
}

void CiaoFrame::addEventHandler(const string& func, const string& event)
{
  printf("Add handler: %s handles %s\n", func.c_str(), event.c_str());
  _handlers[event] = func;
}
