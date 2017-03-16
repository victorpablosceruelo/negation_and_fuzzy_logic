#!/bin/sh

echo "Please wait while CiaoDE is configured..."
echo "The file environment_ciao.log contains the output of this process."
echo
"$1" -C -b "build/bin/config_source_components"
echo "Please verify that no errors appear in this log file." > environment_ciao.log 2>&1
echo "Command executed:" >> environment_ciao.log 2>&1
echo $0 $* >> environment_ciao.log 2>&1
echo Output: >> environment_ciao.log 2>&1
cd ciao
"../$1" -d emacs_path="$2/emacs.exe" -d install_emacs_support=yes -d emacs_type=Win32 environment windows_bats -C -b ../build/bin/lpmake.sta >> ../environment_ciao.log 2>&1
# start wordpad ..\environment_ciao.log
cd ..
