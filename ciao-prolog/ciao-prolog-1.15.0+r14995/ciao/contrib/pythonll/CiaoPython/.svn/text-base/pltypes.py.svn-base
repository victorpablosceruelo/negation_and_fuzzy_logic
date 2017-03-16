from plpythonserver import PLConnection
from plexceptions import PLException
import plterm
from plterm import PLStructure, PLInteger, PLAtom
## --------------------------------------------------------
## Goal status
## --------------------------------------------------------
NOT_LAUNCHED = 0
RUNNING = 1
TERMINATED = -1
FINISHED = -2
LAUNCH_GOAL = "prolog_launch_query"

## --------------------------------------------------------
## Prolog Variable
## --------------------------------------------------------
class PLVariable(plterm.PLTerm) :

    lastRef = -1 # static

    def __init__(self, number = None):
        self.SINGLE_REF = -1 # constant
        self.VarNumber = number
        self.Binding = None
        self.Type = plterm.PLTerm.VARIABLE
        if self.VarNumber == None:
            if PLVariable.lastRef < 0: 
                self.VarNumber = 0
                PLVariable.lastRef = 0
            else: 
                self.VarNumber = ++PLVariable.lastRef
        elif PLVariable.lastRef < number: PLVariable.lastRef = number
        
    def isFree(self) : return self.Binding == None
    def bind(self, term) : self.Binding = term
    def unbind(self): self.Binding = None
    def getBinding(self) : return self.Binding

    def __str__(self):
        ret = ""
        if self.VarNumber == self.SINGLE_REF: ret = "_"
        else: ret = "_" + str(self.VarNumber)
        if not self.isFree(): ret = ret + "{" + str(self.Binding) + "}"
        return ret

    def isRunnable(self): return false

    def equals(self, t):
        return self.Type == t.Type and self.VarNumber == t.VarNumber

    def copy(self):
        v = PLVariable()
        if not self.isFree():
            content = self.Binding.copy()
            v.bind(content)
        return v

    def unify(self, term):
        if self.Binding == None:
            self.Binding = term
            return True
        else:
            return self.Binding.unify(term)

    def backtrack(self, term):
        if term.isVariable():
            self.bind(term.getBinding())
        else:
            raise PLException("Object is not a variable, cannot be backtracked:" + str(self))


def passFun() : pass
def raiseTerminated() : raise PLException("Query already terminated")
def raiseFinished() : raise PLException("Query already finished")
def raiseNotLaunched() : raise PLException("Query not launched")
## --------------------------------------------------------
## Prolog Goal
## --------------------------------------------------------
class PLGoal:
    # Static constants
    SOLUTION = "prolog_solution"
    FAIL = "prolog_fail"
    IS_RUNNING = "prolog_is_running"
    NEXT_SOLUTION = "prolog_next_solution"
    TERMINATE_QUERY = "prolog_terminate_query"
    USE_MODULE = "prolog_use_module"
    EXECUTE = "prolog_execute"

    @staticmethod
    def handleStatus(status): 
        statuserror_table = \
        { TERMINATED : raiseTerminated, 
          FINISHED : raiseFinished, 
          NOT_LAUNCHED : raiseNotLaunched }
        if (statuserror_table.has_key(status)):
            statuserror_table[status]()
        else:
            pass

    def __init__(self, where=None, term=None):
        """where -- a PLConnection
           term -- a plterm.PLTerm"""
        if where == None:
            print "Where (PLConnection) is not given"
            self.prologSpace = PLConnection.getPreviousConnection() # PLConnection
        else: 
            self.prologSpace = where
        import threading
        self.lock = threading.Lock()
        self.goalId = None # plterm.PLTerm
        self.status = NOT_LAUNCHED # int
        self.originalGoal = term
        self.actualGoal = term.copy()
        if self.prologSpace == None:
            raise PLException("No started connection")

    def query(self):
        """bla"""
        printdebug("Run query()")
        if self.status != NOT_LAUNCHED :
            raise PLException("already launched")

        if self.actualGoal.isRunnable() :
            arg = [ self.actualGoal ]
            result = None
            self.lock.acquire() # access to prologSpace
            pythonId = self.getPythonId()
            self.prologSpace.toPrologJP(pythonId, PLStructure(LAUNCH_GOAL, arg=arg))
            result = self.prologSpace.fromPrologJP(pythonId)
            self.lock.release()
            # if result is solution, its type is PLStructure
            # FIXME: not supposed to be none
            if result == None: raise PLException("Result is not supposed to be None")
            
            if result.isSolution() and result.getArg(0).isException():
                raise PLException("version prolog de l'exception?")
            if result.isException():
                raise PLException("version prolog de l'exception?")
            if result.isPrologFail():
                raise PLException("No solution")
            if result.isQueryId():
                self.goalId = result.getArg(0)
                self.status = RUNNING
            else:
                raise PLException("No id received at query creation: " + result)
        else:
            raise PLException("Invalid goal" + self.actualGoal)
    

    def getPythonId(self): #raise NotImplementedError
        return PLStructure("$pythonId", arg=[ hash(self) ])

    def getConnection(self): return self.prologSpace

    # Python 2 Prolog side
    def nextSolution(self): 
        """returns a plterm.PLTerm corresponding to the solution"""
        result = None
        PLGoal.handleStatus(self.status)
        
        # todo: embed following with synchronisation
        self.prologSpace.toPrologJP(self.goalId, PLAtom(PLGoal.NEXT_SOLUTION))
        result = self.prologSpace.fromPrologJP(self.goalId)

        if result.isPrologFail():
            result = None
            self.status = FINISHED
        elif result.isSolution() and result.getArg(0).isException():
            raise PLException.translateException(result.getArg(0))
        elif result.isException():
            raise PLException(result)
        else:
            result = result.getArg(0)
            self.actualGoal.backtrack(self.originalGoal)
            self.actualGoal.unify(result)
        return result

    def execute(self):
        result = None
        PLGoal.handleStatus(self.status)
        self.lock.acquire()
        self.prologSpace.toPrologJP(self.goalId, PLAtom(PLGoal.NEXT_SOLUTION))
        print "result computed"
        result = self.prologSpace.fromPrologJP(self.goalId)
        print "sending result fails"
        self.lock.release()

        return (not result.isPrologFail())

    def isStillRunning(self):
        if   status in [TERMINATED, FINISHED]: return false
        elif status == NOT_LAUNCHED: raise PLException("Query not launched")
        else:
            self.lock.acquire()
            self.prologSpace.toPrologJP(self.goalId, PLAtom(PLGoal.NEXT_SOLUTION))
            result = self.prologSpace.fromPrologJP(self.goalId)
            self.lock.release()
            print "Something computed"
            if result == None: raise PLException("Result is not supposed to be None")
            if   result.isPrologSuccess(): return True
            elif result.isPrologFail():    return False
            elif result.isSolution() and result.getArg(0).isException():
                raise PLException.translateException(result.getArg(0))
            elif result.isException():
                raise PLException.translateException(result)
            else: #???
                raise PLException("Unexpected data returned from prolog:"+ result)
    
    def terminate(self):
        if   self.status == NOT_LAUNCHED: raise PLException("Query not launched")
        elif self.status == TERMINATED: raise PLException("Query already terminated")
        else:
            self.__terminate()

    def __terminate(self):
        # todo: synchronized the two lines
        self.prologSpace.toPrologJP(self.goalId, PLAtom(TERMINATE_QUERY))
        result = self.prologSpace.fromPrologJP(self.goalId)
        if result.isSolution() and result.getArg(0).isException():
            raise PLException.translateException(result.getArg(0))
        elif result.isException():
            raise PLException.translateException(result)
        elif not result.isPrologSuccess(self):
            raise PLException("Termination not accepted by the server")

    # Destructor - not destructor in python.
    #def finalize(self):
    #    if status not in [NOT_LAUNCHED, TERMINATED, FINISHED]:
    #        __terminate()

    def parseTerm(self, termString):
        v = PLVariable()
        p = PLStructure("prolog_parse", arg=[PLString(termString),v])
        g = PLGoal(self.prologSpace, p)
        g.query()
        r = g.nextSolution()
        if r == None: raise PLException("None returned from Prolog socket")
        g.terminate()
        return v.getBinding()

    def __str__(self):
        return "goal {" + str(self.actualGoal) + "}"

    def getPythonId(self):
        return PLStructure("$pythonId", arg=[PLInteger(hash(self))])

