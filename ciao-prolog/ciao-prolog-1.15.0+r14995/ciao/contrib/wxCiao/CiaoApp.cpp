#include"CiaoApp.h"
using namespace std;

CiaoApp* CiaoApp::_instance = NULL;
ciao::handle CiaoApp::_next_handle = 1;

#define STR(X) # X

CiaoApp* CiaoApp::instance()
{
  if(_instance == NULL)
    _instance = new CiaoApp;

  return(_instance);
}

CiaoApp::CiaoApp() 
{
  if(_instance != NULL)
    printf("Warning: two instances\n");
  _instance = this;
}

CiaoApp::~CiaoApp()
{
  _instance = NULL;
}

void CiaoApp::registerInitCallback(const char* cb)
{
  printf("Registered init callback (%s) this=%p\n", cb, this);
  init_callback = strdup(cb);
  printf("registerInitCallback return\n");
}

ciao::handle CiaoApp::getHandle(const string& name)
{
  printf("CiaoApp::getHandle\n");
  map<string,ciao::handle>::iterator i = _names.find(name);
  if(i == _names.end())
    return(ciao::InvalidHandle);
  return(i->second);
}

bool CiaoApp::addWidget(CiaoWidget widget, ciao::handle hdl)
{
  printf("CiaoApp::addWidget\n");
  map<ciao::handle,CiaoWidget>::iterator i = _widgets.find(hdl);
  if(i != _widgets.end())
    return(false);
  _widgets[hdl] = widget;
  return(true);
}

void CiaoApp::mapName(const string& name, ciao::handle hdl)
{
  _names[name] = hdl;
}


ciao::handle CiaoApp::nextHandle()
{
  printf("CiaoApp::nextHandle\n");
  return(_next_handle++);
}
 
bool CiaoApp::OnInit()
{
  printf("CiaoApp::OnInit\n");

    printf("pid: %d\n", getpid());

 ciao_commit_call(init_callback,0);
  return(true);
}

