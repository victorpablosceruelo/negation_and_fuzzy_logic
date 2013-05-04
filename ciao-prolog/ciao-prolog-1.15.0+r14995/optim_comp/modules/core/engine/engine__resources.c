#include <engine/basiccontrol.native.h>

#include <stdlib.h>

intmach_t resources__get_variable(char *name, intmach_t default_value) {
  char *value_string;
  value_string = getenv(name);
  if (value_string != NULL) {
    return atol(value_string);
  } else {
    return default_value;
  }
}

