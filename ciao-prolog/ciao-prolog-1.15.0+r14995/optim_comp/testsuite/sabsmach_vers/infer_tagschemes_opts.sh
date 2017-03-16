#!/bin/bash

# ---------------------------------------------------------------------------
what=tagschemes
echo "Infer good options for ${what}"

machines="ciempies x86_64 clip huygens"

mkdir -p out_view/${what}
ciaotool tests sabsmach-vers summary infer out_view/${what} ${what}_results ${machines}



