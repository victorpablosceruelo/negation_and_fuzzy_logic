#!/bin/bash

echo "Aptitude useful commands from http://www.mepis.org/docs/en/index.php?title=Aptitude"

aptitude clean --purge-unused

aptitude search ~c

aptitude purge $(aptitude search ~c -F "%p")

