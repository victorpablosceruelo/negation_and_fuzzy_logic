# Helper functions (e.g. debug printing)
import sys

debugfile = open("pldebug.txt", "r+")

def printdebug(s): 
    debugfile.read() # read until the end of file
    debugfile.write(str(s)+"\n") # now write..

