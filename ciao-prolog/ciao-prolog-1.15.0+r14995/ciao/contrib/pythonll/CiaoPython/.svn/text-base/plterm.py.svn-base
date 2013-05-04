import io # buffer stuff BufferedIOBase
import StringIO
from plexceptions import *
import copy
import traceback
from plhelper import printdebug

TERMINATE = "$terminate"
PYTHON_OBJECT = "$python_object"
PYTHON_MODULE = "$python_module"
PYTHON_EXCEPTION = "$python_exception"
PROLOG_EXCEPTION = "prolog_exception"
PROLOG_FAIL = "prolog_fail"
PROLOG_SUCCESS = "prolog_success"
PROLOG_QUERY_ID = "prolog_query_id"
AtomTable = []
VarNumber = VarCounter = 0
STRING_BUFFER_SIZE = 20
MAX_READ_BUFFER = 512
#constants for fast_read/fast_write version 'a'.
VERSION_A = 'a'
PFX_LIST_WITH_NIL_TAIL = 'l'
PFX_LIST_WITH_NONIL_TAIL = 'L'
PFX_SHORT_INT = 'i'
PFX_LONG_INT = 'I'
PFX_FLOAT = 'f'
PFX_MULTI_REF_VAR = 'V'
PFX_SINGLE_REF_VAR = 'v'
PFX_ATOM = 'a'
PFX_MULTI_REF_ATOM = 'A'
PFX_STRUCT = 's'
PFX_MULTI_REF_STRUCT = 'S'
VERSION_C = 'C'
PFXC_LIST = '['
PFXC_STRING = '\"'
PFXC_INTEGER = 'I'
PFXC_FLOAT = 'F'
PFXC_VARIABLE = '_'
PFXC_ATOM = 'A'
PFXC_STRUCT = 'S'
PFXC_NIL = ']'
# init to version C (what?)
currentVersion = VERSION_C

## --------------------------------------------------------
## Static methods for reading the socket pipe
## --------------------------------------------------------
def getChar(buff):
    """ buff -- A socket !"""
    try:
        c = buff.recv(1)
    except Exception: 
        traceback.print_exc()
        raise PLException("Cannot read from input socket")
    return c

def getString(buff):
    Subs = ""
    c = getChar(buff)
    while (c != '\0'): 
        Subs += c
        c = getChar(buff)
    return Subs

def getPrefix(buff):pass # only for VERSION_A

def getInt(buff):
    value = 0; number = ""; 
    c = getChar(buff)
    while (c != '\0'):
        number += c 
        c = getChar(buff)
    try: 
        value = int(number)
    except ValueError: 
        raise PLException("Cannot parse " + str(number) + " :(")
    return value

## --------------------------------------------------------
## Prolog term
## --------------------------------------------------------
class PLTerm:
    INTEGER = 1
    FLOAT = 2
    ATOM = 3
    LIST = 4
    STRUCTURE = 5
    VARIABLE = 6
    STRING = 7

    def __init__(self) :
        self.Type == -1
        self.varTable = { }
    
    # accessors
    def isVariable(self) : return self.Type == PLTerm.VARIABLE
    def isStructure(self) : return self.Type == PLTerm.STRUCTURE
    def isString(self) : return self.Type == PLTerm.STRING
    def isList(self) : return self.Type == PLTerm.LIST
    def isNil(self) : return self.equals(nil)
    def isException(self): return self.Type == PLTerm.STRUCTURE and self.getFunctor() == PROLOG_EXCEPTION

    # standards
    def equal(self) : raise NotImplementedError
    def copy(self) : raise NotImplementedError
    def pythonRepr(self, plinterpreter) : raise NotImplementedError
    def isRunnable(self) : raise NotImplementedError
    def equals(self): raise NotImplementedError
    def launchGoal(self, plinterpreter, plconnection) : 
        raise PLGoalException("Term cannot be a goal:" + str(self))
    def hashKey(self) : return str(self)
    def unify(self, term) : 
        if term.isVariable() and term.isFree():
            term.bind(self)
            return True
        elif term.isVariable():
            return self.unify(term.getBinding())
        else: return self.equals(term)
            
    def backtrack(self, term) :
        if not self.equals(term): 
            raise PLException("Cannot backtract" + str(term) + " avec " + str(self))

    @staticmethod
    def pythonException(s) :
        """ s -- a string, with exception info"""
        #printdebug("A python exception is thrown: %s" % s)
        return PLStructure(PYTHON_EXCEPTION, [ PLAtom(s) ])

    def isPrologFail(self) : return self.equals(PLAtom(PROLOG_FAIL))
    def isPrologSuccess(self) : return self.equals(PLAtom(PROLOG_SUCCESS))

    def isQueryId(self) : 
        return (self.Type == PLTerm.STRUCTURE and 
                self.getFunctor() == PROLOG_QUERY_ID and 
                self.getArity() == 1)

    def isSolution(self) :
        from pltypes import PLGoal
        return (self.Type == PLTerm.STRUCTURE and
                self.getFunctor() == PLGoal.SOLUTION and
                self.getArity() <= 2)

    @staticmethod
    def fastRead(socket):
        """ socket -- Socket
            asks to receive data from the given socket and returns a PLTerm"""
        at = []
        AtomTable = at
        VarNumber = VarCounter = 0
        v = getChar(socket)
        if (v != currentVersion) : 
            raise PLException("Wrong fast_read version : '%s'" % v)
        return getTerm(socket)
    
    def fastWrite(self):
        """return a StringIO repr. low-level repr of term
            not sure it is optimized in python"""
        self.varTable = {}
        s  = currentVersion # prefix
        s += self.genTerm(self).getvalue()
        self.genTerm(self).close()
        return s

    #private
    def genTerm(self, t):
        """t -- a PLTerm to be low-level translated
           return -- low-level repr. of t
        """
        sbuff = StringIO.StringIO()
        if t.Type == PLTerm.ATOM:
            if t == nil: 
                sbuff.write(PFXC_NIL)
            else:
                sbuff.write(PFXC_ATOM)
                sbuff.write(t.getName() + '\0')
        elif t.Type == PLTerm.INTEGER:
            sbuff.write(PFXC_INTEGER)
            sbuff.write(str(t) + '\0')
        elif t.Type == PLTerm.LIST:
            sbuff.write(PFXC_LIST)
            sbuff.write(self.genTerm(t.getHead()).getvalue())
            sbuff.write(self.genTerm(t.getTail()).getvalue())
        elif t.Type == PLTerm.STRUCTURE:
            args = t.Args # should work in python if t type is correctly infered
            sbuff.write(PFXC_STRUCT)
            sbuff.write(t.getFunctor())
            sbuff.write('\0')
            arity = len(args)
            sbuff.write("%c" % arity)
            for i in range(len(args)): 
                sbuff.write(self.genTerm(args[i]).getvalue())
        elif t.Type == PLTerm.VARIABLE:
            if t.isFree() : 
                if self.varTable.has_key(t):
                    varNumber = int(self.varTable[t])
                else:
                    varNumber = len(self.varTable)
                    self.varTable[t] = varNumber
                sbuff.write(PFXC_VARIABLE)
                sbuff.write(str(varNumber) + '\0')
            else:
                sbuff.write(t.getBinding() + '\0')
        elif t.Type == PLTerm.STRING:
            sbuff.write(PFXC_STRING)
            sbuff.write(t.getValue() + '\0')
            sbuff.write(PFXC_NIL)
        # CARE: print sbuff.getvalue() can block threads
        return sbuff

## --------------------------------------------------------
## Prolog Atom
## --------------------------------------------------------
class PLAtom(PLTerm):

    def __init__(self,name):
        self.Type = PLTerm.ATOM
        self.Value = name

    def pythonRepr(self, plinterpreter=None): return self.Value
    def __str__(self): return self.Value
    def getName(self) : return self.Value
    def isRunnable(self) : return True
    def equals(self, term): return Type == term.Type and Value.equals(term.Value)

    def launchGoal(self, plinterpreter, plconnection):
        try:
            g = PLGoal(plconnection, self)
            g.query()
            g.execute()
        except Exception: # FIXME: identify exception
            traceback.print_exc()

    def equals(self, t):
        return self.Type == t.Type and self.Value == (t.Value)

    def copy(self):
        return PLAtom(self.getName())
## --------------------------------------------------------
## Prolog Structure
## --------------------------------------------------------
class PLStructure(PLTerm):

    def __init__(self, name, arg=[], arity=None):
        """name -- String
           arg -- List of PLTerm
           arity -- Integer"""
        self.Type = PLTerm.STRUCTURE
        self.Name = name
        self.Arity= None
        if arity != None : 
            self.Arity = arity
        else:
            self.Arity = len(arg)
        self.Args = [a for a in arg] # this is a copy (otherwise, same reference)

    def __str__(self): 
        strArgs = [str(a) for a in self.Args]
        return (self.Name + "(" + (", ".join(strArgs)) + ")")

    def getFunctor(self): 
        return self.Name

    def getArity(self): 
        return self.Arity

    def getArgs(self): 
        return self.Args

    def getArg(self, argNumber) :
        if argNumber < len(self.Args): return self.Args[argNumber]
        else: return None

    def pythonRepr(self, plinterpreter):
        if self.Name in [PYTHON_OBJECT, PYTHON_MODULE] and self.Arity == 1:
            return plinterpreter.getObject(self.Args[0].pythonRepr(plinterpreter))
        elif plinterpreter.isInterpretable(self):
            return (plinterpreter.interpret(self)).pythonRepr(plinterpreter)
        else:
            return self # as generic type Object

    def isRunnable(self): return True

    def equals(self, t):
        if self.Type == t.Type and self.Arity == t.Arity:
            return all([self.Args[i] == t.Args[i] for i in range(self.Arity)])
        return False

    def copy(self) :
        argCopy = [self.Args[i].copy() for i in range(self.Arity)]
        s = PLStructure(self.Name, argCopy, self.Arity)
        return s
    
    def unify(self, term) :
        if term.isVariable() and term.isFree():
            term.bind(self)
            return True
        elif term.isVariable():
            return self.unify(term.getBinding())
        else:
            if self.Type == term.Type and self.Arity == term.Arity :
                for i in range(self.Arity):
                    if not self.Args[i].unify(term.Args[i]):
                        return False
                return True
        return False

    def backtrack(self,term):
        """term -- PLTerm"""
        if self.Type == term.Type and self.Arity == term.Arity:
            for i in range(self.Arity):
                self.Args[i].backtrack(term.Args[i])
        else:
          raise PLException("Object cannot be backtracked" + self.toString())

    def launchGoal(self, plinterpreter, plconnection) :
        args = []
        for i in range(self.Arity):
            if plinterpreter.isInterpretable(self.Args[i]):
                args.append (plinterpreter.interpret(self.Args[i]))
            else:
                args.append (self.Args[i])
        stGoal = PLStructure(self.Name, args, self.Arity)
        try:
            goal = PLGoal(plconnection, stGoal)
            goal.query()
            goal.execute()
        except Exception: # Fixme
            traceback.print_exc()

## --------------------------------------------------------
## Prolog Integer term
## --------------------------------------------------------
class PLInteger(PLTerm):

    def __init__(self, v):
        self.Type  = PLTerm.INTEGER
        self.Value = v

    def __str__(self):
        return str(self.Value)

    def pythonRepr(self, i=None):
        """i -- PLInterpreter"""
        return self.Value # FIXME:value may be a byte

    def getValue(self):
        return self.Value

    def isRunnable(self):
        return False

    def equals(self, t):
        return Type == t.Type and self.Value == t.Value
        
    def copy(self):
        return PLInteger(self.Value)


def getTerm(buff):
    inchar = getChar(buff)
    if inchar == PFXC_LIST:
        (head, tail) = getTerm(buff), getTerm(buff)
        # ununderstood patch
        if tail.isString(): tail = tail.toPLList()
        return PLList(head=head, tail=tail)
    elif inchar == PFXC_STRING:
        s = PLString(getString(buff))
        qdr = getTerm(buff)
        if qdr.isNil(): return s
        else:
            l = s.toPLList()
            l.append(qdr)
            return l
    elif inchar == PFXC_INTEGER:
        return PLInteger(getInt(buff))
    elif inchar == PFXC_VARIABLE:
        from pltypes import PLVariable # import here bcz of circular reference
        return PLVariable(getInt(buff))
    elif inchar == PFXC_ATOM:
        return PLAtom(getString(buff))
    elif inchar == PFXC_STRUCT:
        name = getString(buff)
        arity = ord(getChar(buff)) # in CiaoJava: getChar(buff) instead.
        args = [getTerm(buff) for i in range(arity)]
        return PLStructure(name, args, arity)
    elif inchar == PFXC_NIL:
        return PLAtom("[]")
## --------------------------------------------------------
## Prolog String term
## --------------------------------------------------------
class PLString(PLTerm):
    
    def __init__(self, s):
        self.Value = s
        self.Type = PLTerm.STRING
    
    def __str__(self): 
        return self.Value
    
    def getValue(self):
        return self.Value

    def pythonRepr(self, plinterpreter=None):
        return self.Value

    def isRunnable(self):
        return False

    def equals(self, t):
        return (self.Type == t.Type and self.Value == t.getValue())

    def copy(self):
        return copy.copy(self.Value)
    
    def __len__(self):
        return len(self.Value)

    def toPLList(self):
        il = [PLInteger(self.Value[i]) for i in range(len(self.Value))]
        return PLList(il)

## --------------------------------------------------------
## Prolog List term
## --------------------------------------------------------
class PLList(PLTerm):

    # dirty overloading of PLList constructor 
    def __init__(self, l=None, head=None, tail=None):
        """head -- PLTerm
           tail -- PLTerm
           l -- python list of PLTerms (if l is None, head and tail are not)"""
        self.Type = PLTerm.LIST
        if l == None and head != None and tail != None:
            if tail.equals(nil) or tail.Type == self.Type:
                self.Head = head
                self.Tail = tail
            else:
                raise PLException("Error: wrong tail type (" + str(tail.Type) + ")")
        elif l != None and head == None and tail == None:
            self.Head = l[0]
            if len(l) > 1:      self.Tail = PLList(l[1:])
            elif len(l) == 1:   self.Tail = nil
            else:               return PLException("Nil cannot be of type PLList")
        else: # other cases forbidden
            raise PLException("Wrong initialization of PLList")
            sys.exit(1)

    def __str__(self):
        s = "["
        t = self
        while (t.getTail().Type == self.Type):
            s = s + str(t.getHead()) + ", "
            t = t.getTail()
        return s + str(t.getHead()) + "]"

    def pythonRepr(self, plinterpreter=None):
        t = self
        v = []
        # t.getTail() is a PLAtom instance
        while (t.Type == self.Type):
            v.append(t.getHead().pythonRepr(plinterpreter))
            t = t.getTail()
        return v

    def isRunnable(self) : return False

    def getHead(self) : return self.Head
    def getTail(self) : return self.Tail
    
    def setTail(self, l):
        if l.isList() or l.isNil(): self.Tail = l
        else: raise PLException("The tail is not PLList nor nil")

    def add(self, t):
        tail = self
        while tail.getTail().isList(): tail = tail.getTail()
        tail.setTail(PLList(head=t, tail=nil))

    def append(self, l):
        tail = self
        if l.isNil() or l.isList():
            while (tail.getTail().isList()):
                tail = tail.getTail()
            tail.setTail(l)
        else:
            raise PLException("append argument is not a PLLList of nil : %s" % str(l))

    def equals(self, t):
        if (self.Type == t.Type):
            return self.Head.equals(t.Head) and self.Tail.equals(t.Tail)
        else:
            return False
        
    def copy(self):
        return PLList(head=self.Head.copy(), tail=self.Tail.copy())
    
    def unify(self, t):
        if t.isVariable():
            if t.isFree(): 
                t.bind(self)
                return True
            else:
                return self.unify(t.getBinding())
        elif self.Type == t.Type:
            return self.Head.unify(t.Head) and self.Tail.unify(t.getTail())
        return False

    def backtrack(self, term):
        """term -- PLTerm"""
        if self.Type == term.Type:
            self.getHead().backtrack(term.getHead())
            self.getTail().backtrack(term.getTail())
        else:
            raise PLException("Object cannot be backtracked" + str(self))

    def __len__(self):
        n = 1
        t = self
        while self.getTail().Type == Type:
            n+=1
            t = t.getTail()
        return n

# Nil is here.
nil = PLAtom("[]")
