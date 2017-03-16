import sys
sys.path.insert(0, "..")

from CiaoPython.plterm import *
from CiaoPython.pltypes import *
import traceback

if __name__ == '__main__':
    plconnection = None
    try:
        if len(sys.argv) == 1:
            plconnection = PLConnection()
        else:
            plconnection = PLConnection(where=sys.argv[1:])
    except Exception:
        print "Could not create a PLConnection"
        traceback.print_exc()
    plList = PLList([PLAtom('a'), PLAtom('b')])
    plX = PLVariable()
    plY = PLVariable()
    plArgs = [plX, plY, plList]
    strGoal = PLStructure('append', plArgs)
    goal = PLGoal(plconnection, strGoal)
    try:
        print ("Query: " + str(strGoal))
        goal.query()
        #s = goal.execute()
        #print "A solution was found:", s
        while (goal.nextSolution() != None):
            print ("Solution: " + str(strGoal))
    except Exception:
        print "Error while finding solutions"
        traceback.print_exc()


    try:
        plconnection.stop()
        sys.exit(0)
    except Exception:
        print "Could not stop prolog server"
        sys.exit(1)

