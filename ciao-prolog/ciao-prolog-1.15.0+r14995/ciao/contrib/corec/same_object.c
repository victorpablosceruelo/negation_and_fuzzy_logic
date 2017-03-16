#include "datadefs.h"
#include "support.h"

bool_t 
same_object_2(Arg)
     Argdecl;
{
  DEREF(X(0), X(0));
  DEREF(X(1), X(1));

  return X(0) == X(1);
}

