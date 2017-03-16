#!/bin/sh
echo "If you see 'attr_debug_bug', there is a bug:"
ciao > /dev/null <<EOF
debug_module_source(k).
use_module(k).
trace.
( test(X) -> display(user_error, attr_debug_no_bug), nl(user_error) ; display(user_error, attr_debug_bug), nl(user_error) ).








EOF

