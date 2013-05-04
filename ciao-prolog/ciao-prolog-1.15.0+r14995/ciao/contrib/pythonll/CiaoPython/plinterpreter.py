# NOTE: when import fail, they do not appear on stderr (through prolog)

import inspect, types
from plexceptions import PLException
from plterm import PLAtom, PLTerm, PLInteger, PLString, PLStructure, PYTHON_OBJECT, PYTHON_MODULE, PLList, nil
from plhelper import printdebug
import traceback

# Constants for the prolog-python communication
STARTING_CAPACITY = 16
FACTOR = .2
INTERPRETER_ERROR = "No"
INTERPRETER_SUCCESS = "Yes"
# Interface strings from prolog to python
CREATE_OBJECT = "$python_create_object"
DELETE_OBJECT = "$python_delete_object"
GET_VALUE = "$python_get_value"
SET_VALUE = "$python_set_value"
INVOKE_METHOD = "$python_invoke_method"
IMPORT_MODULE = "$python_import_module"

TERMINATE = "$terminate"
DISCONNECT = "$disconnect"
WAIT_FOR_EVENTS = "$python_wait_for_events"

class PLInterpreter:
    """Interprete the terms received from the Prolog side and performs the actions requested from that side
   Contains the object table needed to look up the Python objects referred by the Prolog side.
    """
    success = PLAtom(INTERPRETER_SUCCESS)
    fail = PLAtom(INTERPRETER_ERROR)
    terminate = PLAtom(TERMINATE)

    def __init__(self, plconnection=None):
        self.objTable = {}
        self.objKey = 0

    def interpret(self, t):
        """t -- a PLTerm, corresponding to a request $bidule"""
        if t.Type == PLTerm.STRUCTURE:
            if t.Name == CREATE_OBJECT:     return self.createObject(t)
            if t.Name == DELETE_OBJECT:     return self.deleteObject(t)
            if t.Name == GET_VALUE:         return self.getValue(t)
            if t.Name == SET_VALUE:         return self.setValue(t)
            if t.Name == INVOKE_METHOD:     return self.invokeMethod(t)
            if t.Name == IMPORT_MODULE:     return self.importModule(t)
        elif t.Type == PLTerm.ATOM:
            if t.getName() in [TERMINATE, DISCONNECT]: 
                return PLAtom(t.getName())
        # any other case
        return PLTerm.pythonException("Unexpected atom received")

    def getValue(self, st):
        """st -- PLStructure"""
        # Check that structure is well-formed
        if len(st.Args) != 2:
            return PLTerm.pythonException("getvalue: number of arguments")
        if st.Args[0].Type not in [PLTerm.STRUCTURE, PLTerm.ATOM]:
            return PLTerm.pythonException("getvalue: object reference")
        if st.Args[1] != PLTerm.ATOM:
            return PLTerm.pythonException("getvalue: Field name reference")
        obj = None
        attrname = str(st.Args[1].pythonRepr(self))
        # Get the value of the structure (whether Class.Attr, or instance.Attr)
        if st.Args[0] == PLTerm.ATOM:
            if globals().has_key(str(st.Args(0))):
                cl = globals()[str(st.Args[0])]
                fieldval = getattr(cl, attrname)
            else:
                return PLTerm.pythonException(\
                "getvalue: class or attr not found -" + str(st.Args[0]))
        # Get the instance attribute
        else :
            struct = st.Args[0] # it is a PLStructure
            if not struct.Name == PYTHON_OBJECT or struct.Arity != 1:
                return PLTerm.pythonException("getvalue: object structure")
            obj = struct.pythonRepr(self)
            if obj != None:
                fieldval = getattr(obj, attrname)
            else:
                return PLTerm.pythonException("getvalue: object not found")
        return prologRepr(fieldval)

    def setValue(self, st):
        """st -- PLStructure """
        # Check well-formedness of the arguments given in PLStructure
        if len(st.Args) != 3:
            return PLTerm.pythonException("setvalue: number of arguments")
        if st.Args[0].Type != PLTerm.STRUCTURE:
            return PLTerm.pythonException("setvalue: object reference")
        if st.Args[1].Type != PLTerm.ATOM:
            return PLTerm.pythonException("setvalue: field name reference")
        # Check and get the reffered instance (i.e. 1st argument)
        plst = st.Args[0]
        if plst.Name != PYTHON_OBJECT or plst.Arity != 1:
            return PLTerm.pythonException("setvalue: object structure")
        obj = plst.pythonRepr(self)
        if obj == None:
            return PLTerm.pythonException("setvalue: object not found")
        # Check and get the value to set
        val = st.Args[2].pythonRepr(self)
        try:
            attrname = str(st.Args[1].pythonRepr(self))
            setattr(obj, attrname, val)
        except Exception: # FIXME: more precise exception(s)
            return PLTerm.pythonException("setvalue: unset field %s=%s on %s (%s)" %  (attrn, obj, val, traceback.format_exc())) 

    def deleteObject(self, st):
        """st -- PLStructure"""
        # Check well-formedness of the arguments given in PLStructure
        if len(st.Args) != 1:
            return PLTerm.pythonException("deleteobject: number of arguments")
        if st.Args[0].Type != PLTerm.STRUCTURE:
            return PLTerm.pythonException("deleteobject: object reference")
        # Check and get the reffered instance (i.e. 1st argument)
        plst = st.Args[0]
        if plst.Name != PYTHON_OBJECT or plst.Arity != 1:
            return PLTerm.pythonException("deleteobject: object structure")
        objid = int(plst.Args[0].pythonRepr(self))
        value = None
        # Remove instance
        try:
            value = self.objTable.remove(obj)
        except Exception:
            return PLTerm.pythonException("deleteobject: object not found")
        if value == None:
            return PLTerm.pythonException("deleteobject: object not found")
        else:
            return PLInterpreter.success

    def importModule(self, st):
        """ st -- PLStructure """
        printdebug('ImportModule called')
        if len(st.Args) != 1: # one argument for the import name, one for the assignment
            return PLTerm.pythonException("importModule : number of arguments")
        elif st.Args[0].Type != PLTerm.ATOM:
            return PLTerm.pythonException("importModule : module name")
        try:
            newmod = __import__(st.Args[0].Value)
            self.objTable[hash(newmod)] = newmod
            return self.python_module(hash(newmod))
        except ImportError: 
            return PLTerm.pythonException("import error")
        except TypeError:
            return PLTerm.pythonException("type error: %s\n" % traceback.format_exc())
        except Exception:
            return PLTerm.pythonException("unexpected error: %s\n" % traceback.format_exc())

    def createObject(self, st):
        """st -- PLStructure"""
        # Check structure well-formedness (ClassName | ConstructorParams)
        if len(st.Args) != 2: 
            return PLTerm.pythonException("createObject: number of arguments")
        elif st.Args[0].Type != PLTerm.ATOM:
            return PLTerm.pythonException("createObject: class name")
        elif not (st.Args[1].isList() or st.Args[1].isNil() or st.Args[1].isString()):
            return PLTerm.pythonException("createobject: no list argument")
        # Get class, arg0 is a term, convert to string
        try: 
            cl = self._getClassForName(str(st.Args[0])) 
        except KeyError:
            return PLTerm.pythonException("createobject: class not found")
        # Create object and add it to objTable
        newobj = None
        try:
            arg = st.Args[1]                            # arguments for obj construction
            arg2str = []
            if arg.Type in [PLTerm.LIST, PLTerm.STRING]:# in python, only 1 constructor
                pyargs = arg.pythonRepr(self)
                arg2str = self.translatebytes(pyargs)
            newobj = cl(*arg2str)
            self.objTable[hash(newobj)] = newobj
        except Exception:
            return PLTerm.pythonException("createObject: %s" % traceback.format_exc())
        return self.python_object(hash(newobj))

    def translatebytes(self, b):
        # Translate b into int (length=1), or string, or a list of things
        if type(b) == str :
            if len(b) == 1: return self._getArgAsArray(b) # it is a byte
            else: return b                                # it is a string
        elif type(b) == list:
            return [ self.translatebytes(arg) for arg in b]
        else:
            raise PLException("Elements of type %s not handled " % str(b.Type) ) #fixme: number

    def python_object(self, pyid):
        """Prolog representation of a Python object in the object table
        pyid -- a python object identifier"""
        return PLStructure(PYTHON_OBJECT, arg=[PLInteger(pyid)])

    def python_module(self, pyid):
        """ pyid -- a python module identifier """
        return PLStructure(PYTHON_MODULE, arg=[PLInteger(pyid)])

    def getMethod(self, classtype, methname, classtypetable=[]):
        """ classtype -- a Class
        methname -- a string name of the looked-up method
        classtypetable -- arguments with which call the method"""
        mt = getattr(classtype, methname)
        # I do not care yet about the parameters in python 
        return mt

    
    def invokeMethod(self, st):
        """st -- a PLStructure"""
        printdebug("Call to invokeMethod ($python_invoke_method)")
        # Check arguments
        if len(st.Args) != 3 :
            return PLTerm.pythonException("invokemethod: number of arguments")
        if st.Args[0].Type not in [PLTerm.STRUCTURE, PLTerm.ATOM]:
            return PLTerm.pythonException("invokemethod: object/class reference")
        if st.Args[1].Type != PLTerm.ATOM:
            return PLTerm.pythonException("invokemethod: method name")
        if not (st.Args[2].isList() or st.Args[2].isNil() or st.Args[2].isString()):
            return PLTerm.pythonException("invokemethod: no list argument")
        # Get class name of method
        (cl, obj) = None, None
        if st.Args[0].Type == PLTerm.ATOM:
            try:
                self._getClassForName(str(st.Args[0]))
            except:
                return PLTerm.pythonException("invokemethod: class not found")
        else:
            plst = st.Args[0]
            if plst.Name not in [PYTHON_OBJECT, PYTHON_MODULE] or plst.Arity != 1:
                return PLTerm.pythonException("invokemethod: object structure")
            obj = plst.pythonRepr(self)
            if obj == None:
                return PLTerm.pythonException("invokemethod: python object not found")
            # python-specific
            if type(obj) == types.ModuleType: 
                cl = obj
            else:
                cl = obj.__class__
        # Find and invoke method
        try:
            mtName = st.Args[1].pythonRepr(self)
            mt = self.getMethod(cl, mtName)
            result = None
            arg2str = []
            # Check and get arguments if they exist (otherwise a PLAtom '_' is given in Arg.)
            if st.Args[2].isList() or st.Args[2].isString():
                pyargs = st.Args[2].pythonRepr(self)
                arg2str = self.translatebytes(pyargs)
            result = self.invoke(mt, obj, arg2str)
            # Check result
            if result == None:
                return PLInterpreter.success
            else:
                return self.prologRepr(result)
        except:
            return PLTerm.pythonException("invokemethod: method not found -- %s" % traceback.format_exc())

    
    def prologRepr(self, v):
        """Get the prolog representation of the object argument
        v - an Object
       returns a PLTerm representing the python object v
        """
        if type(v) == list:
            nlist = nil
            if len(v) > 0:
                pva = [self.prologRepr(vi) for vi in v]
                try:
                    nlist = PLList(pva)
                except PLException:
                    return PLTerm.pythonException("prologRepr: ill-formed python object")
        if type(v) == int: return PLInteger(v)
        if type(v) == str: return PLString(v)
        if type(v) == bool:return PLString(str(v))
        import pltypes
        if v.__class__ in [PLTerm, PLAtom, PLInteger, PLList, PLStructure, pltypes.PLVariable]: 
            return v
        # 'list' is an unhashable type --> cheating with str
        if type(v) == list: 
            hashkeyv = hash(str(v))
        else: 
            hashkeyv = hash(v)
        self.objTable[hashkeyv] = v
        return self.python_object(hashkeyv)

    def invoke(self, mt, obj, args):
        """Invoke method mt on object obj, with arguments args"""
        if type(obj) == types.ModuleType: 
            return mt(*args)
        else: 
            return mt(obj, *args)
    
    def getObject(self, hashcode): return self.objTable[hashcode]
    
    def isInterpretable(self, t):
        """ t - PLTerm"""
        if t.Type == PLTerm.STRUCTURE:
            return st.Name in [CREATE_OBJECT, DELETE_OBJECT, GET_VALUE, SET_VALUE, INVOKE_METHOD]
        elif t.Type == PLTerm.ATOM:
            return t.getName() in [TERMINATE, DISCONNECT]
        else: 
            return False

    @staticmethod
    def _getArgAsArray(strarg):
        """ TODO - doc ."""
        res = [i for i in bytearray(str(strarg))]
        return res

    @staticmethod
    def _getModuleForName(m): return _getClassForName(m)

    @staticmethod
    def _getClassForName(classname):
        if classname.find('.') == -1:
            return globals()[classname]
        else:
            return reduce(getattr, classname.split('.')[1:], __import__(classname.split('.')[0]))
    # ignored in Python
    # def getConstructor
    # def getDistance
    # def getPrimitiveClass
