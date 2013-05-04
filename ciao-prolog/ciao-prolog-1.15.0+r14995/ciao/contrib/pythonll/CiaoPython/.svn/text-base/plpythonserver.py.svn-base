import traceback, sys, socket, sys, subprocess
import threading
from threading import Thread
from plterm import PLAtom, PLInteger, PLStructure
from plinterpreter import PLInterpreter
from plsocket import PLSocketWriter, PLSocketReader, PLMultithreadSocketReader
from plhelper import printdebug

## --------------------------------------------------------
## Prolog Server, reads prolog requests, interprets them and do actions
## --------------------------------------------------------
class PLPythonObjServer(Thread):
    """Lit les requetes prolog, les interprete, et fait les actions qu'il faut"""

    def __init__(self, plconnection):
        printdebug( "Start PLPythonObjServer")
        Thread.__init__(self)
        self.pl = plconnection
        self.start()

    def run(self):
        i = self.pl.getInterpreter()
        printdebug("Read a command from prolog: %s" % str(i))
        cmd = self.pl.fromPrologPJ()
        printdebug("Command received: %s" % str(cmd))
        try:
            while not cmd.getArg(1).equals(PLInterpreter.terminate):
                response = i.interpret(cmd.getArg(1))
                if not response == PLInterpreter.terminate:
                    self.pl.toPrologPJ(cmd.getArg(0), response)
                cmd = self.pl.fromPrologPJ()
            self.pl.joinSocketHandlers()
            self.pl.closeSocketStreams()
        except Exception:
            printdebug( "PLJavaObjServer error")
            sys.exit(1)

        printdebug ("Terminating PLJavaObjServer")


JP_SYNC =  PLAtom("event")
PJ_SYNC =  PLAtom("data")
ID_INTERFACE =  PLInteger(0)
## --------------------------------------------------------
## Prolog Connection
## --------------------------------------------------------
class PLConnection:

    previousConnection = None
    debugging = True
    HOST = 'localhost'
    PORT = 5005
    def __init__(self, sock=None, where=None):
        """sock -- server socket
           where -- prolog server command (as an array)"""
        self.plInterpreter = None
        self.pjSocket = None
        self.pjSocket = None ## socket client 
        self.jpSocket = None
        self.jpReader = None
        self.jpWriter = None
        self.pjReader = None
        self.pjWriter = None
        self.jServer = None # PLJavaObjServer

        if sock == None:
            printdebug( "Creating a socket server")
            self.ss = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            #self.ss.bind((PLConnection.HOST, PLConnection.PORT)) # random argument values
            self.ss.bind(('', 0))
            self.ss.listen(5) # listen capacity
        else:
            self.ss = sock
        self.start(where)
        PLConnection.previousConnection = self

    def start(self, where=None):
        """where -- Array containing the prolog server command """
        import sys
        self.plInterpreter = PLInterpreter(self)
        (addr, port) = self.ss.getsockname()
        printdebug ("Port number: %d" % port)
        print "%s." % str(port)
        sys.stdout.flush()
        if where == None:
            self.bindSockets()
        else:
            proc = subprocess.Popen(where, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
            proc.stdin.write(str(port)+".") # pass the socket port to plserver
            proc.stdin.close()
            self.bindSockets()

    def getInterpreter(self):
        return self.plInterpreter

    def bindSockets(self):
        event = threading.Event()
        # prolog-to-java socket
        printdebug( "PJSocket: Accepting connection...")
        (self.pjSocket, address) = self.ss.accept()
        printdebug( "PJSocket: Connection accepted.")
        import Queue
        q = Queue.Queue()
        self.pjWriter = PLSocketWriter(self.pjSocket)
        self.pjReader = PLSocketReader(self.pjSocket, self.pjWriter)
        # synchronizing p2j socket
        self.fromPrologPJ()
        self.toPrologPJ(ID_INTERFACE, PJ_SYNC)

        # java-to-prolog socket: connect prolog to java, with java being now the client
        if PLConnection.debugging: printdebug ("JPSocket: Accepting connection...")
        (self.jpSocket, address) = self.ss.accept()
        if PLConnection.debugging: printdebug ("JPSocket: Connection accepted.")
        self.jpWriter = PLSocketWriter(self.jpSocket)
        self.jpReader = PLMultithreadSocketReader(self.jpSocket, self.jpWriter)
        # synchronizing j2p socket
        self.fromPrologJP(ID_INTERFACE)
        self.toPrologJP(ID_INTERFACE, JP_SYNC)

        self.jServer = PLPythonObjServer(self)

    def connectSockets(self, host, port):
        self.pjSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.pjSocket.connect(port)
        self.toPrologPJ(ID_INTERFACE, PJ_SYNC)
        self.fromPrologPJ()
        
        self.jpSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.jpSocket.connect(port)
        self.toPrologJP(ID_INTERFACE, JP_SYNC)
        self.fromPrologJP(ID_INTERFACE)
        
        self.jServer = PLPythonObjServer(self)


    def query(self, term):
        """ term -- PLTerm
        returns a PLGoal"""
        goal = PLGoal(self,term)
        goal.query()
        return goal

    def fromPrologJP(self, idterm):
        return self.jpReader.read(idterm)

    def fromPrologPJ(self): 
        msg = self.pjReader.read()
        return msg

    def toPrologJP(self, idterm, term):
        arg = [idterm, term]
        msg = PLStructure("jp", arg);
        self.jpWriter.write(msg)

    def toPrologPJ(self, idterm, term):
        arg = [idterm, term]
        msg = PLStructure("pj", arg)
        self.pjWriter.write(msg)

    def stop(self):
        self.toPrologJP(ID_INTERFACE,PLInterpreter.terminate)
        self.toPrologPJ(ID_INTERFACE,PLInterpreter.terminate)
        self.join() # this method also closes socket streams.

    # Socket closing
    def closeSocketStreams(self):
        self.pjSocket.close()
        self.jpSocket.close()
        if (self.ss != None):
            self.ss.close()

    def join(self):
        self.joinSocketHandlers()
        jServer.join()

    def joinSocketHandlers(self):
       self.jpReader.join()
       self.jpWriter.join()
       self.pjReader.join()
       self.pjWriter.join()

    @staticmethod
    def getPreviousConnection(): 
        return PLConnection.previousConnection
    
    def getPrologProcess(self): return self.plProc



def usage() :
    print "Wrong usage. Options are : -server, -port P, -max M, ou -help"

if __name__ == "__main__" :
    from sys import argv
    port = 0
    max = 2
    server = "-server" in argv
    # Check command line arguments
    if "-port" in argv:
        try: port = int(argv[argv.index("-port")+1])
        except: usage(); sys.exit(1)
    if "-max" in argv:
        try: max = int(argv[argv.index("-max")+1]) *2
        except: usage(); sys.exit(1)
    if argv[0] == "-help": usage(); sys.exit(0)

    pysocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    # Bind socket with (HOST,PORT)
    pysocket.bind(('', port))
    pysocket.listen(5) # 5 = random num
    try:
        if server:
            pl = None
            while True: # data : data sent by cl. client : client ip
                pl = PLConnection(sock=pysocket)
        else:
            pl = PLConnection(sock=pysocket)
            pl.join()
        pysocket.close()
    except Exception:
        printdebug ("Problem starting python server: ")
        traceback.print_exc()
