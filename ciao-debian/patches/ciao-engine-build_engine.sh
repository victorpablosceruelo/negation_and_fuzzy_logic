Index: ciao/engine/build_engine.sh
===================================================================
--- ciao/engine/build_engine.sh	(revision 14285)
+++ ciao/engine/build_engine.sh	(working copy)
@@ -2,6 +2,11 @@
 
 set -e 
 
+echo " "
+echo "CIAOBUILDDIR: ${CIAOBUILDDIR}"
+echo "CIAODESRC: ${CIAODESRC}"
+echo " "
+
 if test x"${CIAOBUILDDIR}" = x""; then
     echo "error: CIAOBUILDDIR is undefined" >&2
     echo "Please, do not call this script directly. Use 'ciaosetup' instead." >&2
