#include <engine/basiccontrol.native.h>

extern char *library_directory;

CBOOL__PROTO(prolog_ciao_lib_dir) {
  CBOOL__LASTUNIFY(X(0), GET_ATOM(library_directory));
}

extern char *emulator_architecture;

CBOOL__PROTO(prolog_getarch) {
  CBOOL__LASTUNIFY(GET_ATOM(emulator_architecture), X(0));
}

extern char *emulator_os;

CBOOL__PROTO(prolog_getos) {
  CBOOL__LASTUNIFY(GET_ATOM(emulator_os), X(0));
}

extern char *emulator__so_cc;

CBOOL__PROTO(prolog_get_so_cc) {
  CBOOL__LASTUNIFY(GET_ATOM(emulator__so_cc), X(0));
}

extern char *emulator__so_ld;

CBOOL__PROTO(prolog_get_so_ld) {
  CBOOL__LASTUNIFY(GET_ATOM(emulator__so_ld), X(0));
}

extern char *emulator__so_cc_opts;

CBOOL__PROTO(prolog_get_so_cc_opts) {
  CBOOL__LASTUNIFY(GET_ATOM(emulator__so_cc_opts), X(0));
}

extern char *emulator__so_ld_opts;

CBOOL__PROTO(prolog_get_so_ld_opts) {
  CBOOL__LASTUNIFY(GET_ATOM(emulator__so_ld_opts), X(0));
}

extern char *emulator__so_libs;

CBOOL__PROTO(prolog_get_so_libs) {
  CBOOL__LASTUNIFY(GET_ATOM(emulator__so_libs), X(0));
}

