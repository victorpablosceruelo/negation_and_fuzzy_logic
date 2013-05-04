"""
This file contains some classes that can be used to test prolog to python
"""
# Syntax errors are lost
import sys
class Sum:
    """Parses a text expression corresponding to a 
       sum and returns a value"""

    def __init__(self, value=0, text=""):
        self.text = ""
        if value == 0:
            self.text = text
        elif text == "":
            # fixme: value is supposed to be an integer, not string
            self.text = '+'.join([str(i) for i in range(value+1)])
        else:
            # test
            self.text = text
        # FIXME: prolog types not handled correctly for a list of args
        print >> sys.stderr, "text = %s, value = %s" % (self.text, str(value))
        self.nlist = None
        self.result = 0

    def compute(self):
        if len(self.text) > 0: self.result = reduce(lambda x, y: x + y, self.asList())
    
    def asList(self):
        if not self.nlist:
            import sys
            print >> sys.stderr, "%s." % self.text
            if len(self.text) > 0: self.nlist = [int(i) for i in self.text.split("+")]
            else: self.nlist = []
        return self.nlist

    def printme(self):
        try:  
            f = None
            try:
                f = open("testsumout.txt", "r+")
                f.read()
            except IOError:
                f = open("testsumout.txt", "w")
            f.write( "List = %s - " % self.nlist)
            f.write("Sum  = %d\n" % self.result)
            f.close()
        except Exception:
            import traceback
            traceback.print_exc()
