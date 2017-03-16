from threading import Thread
import threading
import sys, traceback
from plterm import PLTerm
import Queue
from plhelper import printdebug

## ----------------------------------------------------------------
#  PLSocketWriter
## ----------------------------------------------------------------
class PLSocketWriter(Thread):

    def __init__(self, socket, event=None, queue=None):
        Thread.__init__(self)
        self.socket = socket
        self.queue = Queue.Queue()
        self.event = event
        self.start()

    def run(self):
        from plinterpreter import PLInterpreter
        try:
            s = None
            while not s or not s.getArg(1).equals(PLInterpreter.terminate):
                s = self.getMsg()
                self.socket.send(s.fastWrite())
        except Exception: 
            traceback.print_exc()
            sys.exit(1)
        
    def write(self, msg):
        """msg -- PLTerm"""
        self.queue.put(msg)

    def getMsg(self):
        """synchronized method"""
        msg = self.queue.get()
        return msg

## ----------------------------------------------------------------
#  PLSocketReader
## ----------------------------------------------------------------
class PLSocketReader(Thread):

    def __init__(self, socket, writer, event=None, queue=None):
        import Queue
        Thread.__init__(self)
        self.socket = socket
        self.writer = writer
        self.event = event
        self.queue = Queue.Queue()
        self.start()

    def run(self):
        from plinterpreter import PLInterpreter
        try:
            s = None
            while not s or not s.getArg(1).equals(PLInterpreter.terminate):
                s = PLTerm.fastRead(self.socket)
                self.addMsg(s)
            self.writer.write(s)
        except Exception:
            traceback.print_exc()
            sys.exit(1)
        
    def read(self):
        msg = self.queue.get()
        return msg

    def addMsg(self, msg):
        """msg -- a PLTerm """
        self.queue.put(msg)

## ----------------------------------------------------------------
#  PLMultithreadSocketReader probably not working
## ----------------------------------------------------------------
class PLMultithreadSocketReader(Thread):

    def __init__(self, socket, writer):
        Thread.__init__(self)
        self.socket = socket
        self.writer = writer
        self.msgTable = {} 
        self.event = threading.Event()
        self.lock = threading.Lock()
        self.start()

    def run(self):
        from plinterpreter import PLInterpreter
        s = None 
        while not s or not s.getArg(1).equals(PLInterpreter.terminate):
            try:
                s = PLTerm.fastRead(self.socket)
            except Exception:
                printdebug ("PLMultithreadSocketReader: Socket broken")
                traceback.print_exc()
                sys.exit(1)
            try:
                jId = s.getArg(0).hashKey()
                self.addMsg(jId, s.getArg(1))
            except Exception:
                printdebug ("PLMultithreadSocketReader error")
                traceback.print_exc()
                sys.exit(1)
        self.writer.write(s)
        printdebug ("Terminating PLMultithreadSocketReader")

    def read(self, plid):
        """plid -- PLTerm. Element requested by called"""
        # If None, wait...
        jId = plid.hashKey()
        msg = self.getMsg(jId)
        if msg == None: 
            self.event.wait()
            msg = self.getMsg(jId)
        return msg

    def getMsg(self, oid):
        """oid -- Object identifier"""

        if self.msgTable.has_key(oid):
            msgList = self.msgTable[oid] # A list of PLTerm elements
            if msgList.not_empty:
                msg = msgList.get()
                if msgList.empty: # is there still something to wait for?
                    self.msgTable.pop(oid)
                return msg
            else: # javacommentcopy: case that should not occur
                self.msgTable.pop(oid)
                return None
        else:
            return None # when is oid not in msgTable?

    def addMsg(self, oid, t):
        """oid -- an Object
           t   -- a PLTerm
           Adds the object t with id oid to the msgTable"""
        if not self.msgTable.has_key(oid):
            self.msgTable[oid] = Queue.Queue()
        msgList = self.msgTable[oid]
        msgList.put(t)
        self.event.set()
        self.event.clear()

    def clear(self):
        """Clears the internal data structures"""
        self.msgTable.clear()


