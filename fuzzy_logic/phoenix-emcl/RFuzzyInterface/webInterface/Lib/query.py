from subprocess import *
import re
from webInterface.models import *
from copy import deepcopy
from django.contrib import admin
from django.core.management import call_command
from django.db.models import loading
from django.db import models

def query(program):
    path = "webInterface/IOFile/"
    call(["ciaoc",path+program])
    f=Popen(["./"+path+program], stdout=PIPE)
    return f.stdout
       
def parseAllKeywords():
    frame = {}
    result = {}
    kws = query("crisp_QA").readlines()[0][1:-1]
    for i in range(SimpleQuery.objects.all().count()):
        key = i
        result.update({key:[]})
    keywords = re.split(",",kws)
    for e in keywords:
        frame.update({e:deepcopy(result)})
#   print frame
    return frame

def parseAllAnswers():
    frame = parseAllKeywords()
    print "initial frame"
    print frame
    results = deepcopy(query("fuzzy_QA").readlines())
    for i in range(SimpleQuery.objects.all().count()):
        qindex = i
        pos = results.index("\n")
        qresult = [e.strip() for e in results[:pos]]
        results = results[pos+1:]
        #deal with two lines
        qresults = [qresult[2*n:2*n+2] for n in range(len(qresult)/2)]
        #print qresults
        for e in qresults:
            frame[e[0]][qindex].append(e[1])
    print frame
    return frame




