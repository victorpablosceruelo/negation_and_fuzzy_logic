# Create your views here.
from django.http import HttpResponseRedirect

from webInterface.forms import *
from webInterface.models import *
from annoying.decorators import render_to
from annoying.decorators import render_to, ajax_request
from webInterface.Lib.configurationLib import *
from webInterface.Lib.validation import *
from webInterface.Lib.metaDBLib import *
from webInterface.Lib.fileLib import *


def defaultDomain():
    class InnerClass:
        domain_type = "C"
        domain_range = "[[0,1]]"
    return InnerClass()

def formalizeFunc(data):
    """ input is the request.GET, output is a list of functions with"""
    in_dict = True
    index = 0
    funcs = []
    while in_dict:
        key_type = "funcs[%d][type]" % index
        key_range = "funcs[%d][range]" % index        
        key_expr = "funcs[%d][expression]" % index
        if key_type in data and key_range in data and key_expr in data:
            #get the values by keys
            funcs.append({"type" : data.get(key_type), "range" : data.get(key_range), "expression" : data.get(key_expr)})
        else:
            in_dict = False
        index = index + 1
    return funcs

@render_to("base_add.html")
def createConcept(request):
    name = "concept"
    
    function_form = ConceptFunctionForm()
    if request.method == 'POST':
        print "-----------HERE IS POST DATA-----------"
        print request.POST
        conceptName = request.POST.get("name")
        attrpk=request.POST.get("attribute")
        if conceptName!="" and attrpk!="":
            saveConcept(conceptName,attrpk)
        # Functions are passed as a big string separated by \n each
        for func in request.POST.get("funcs", "").split("\v")[:-1]:
            # Values are separated by \t
            vals = func.split("\t")
            dt = vals[0]
            dr = vals[1]
            expr = vals[2]
            print "<%s, %s, %s >" % (dt, dr, expr)
            if conceptName!="" and attrpk!="":
                # if all validation is fine
                saveConceptFunc(dt,dr,expr,conceptName)
                data = {"conceptfunctions" : ConceptFunction.objects.all(),
                        "concepts" : Concept.objects.all()}
            else:
                form = ConceptForm(request.POST)
                error_message = "Some error happened"
        return HttpResponseRedirect('/success/')
    else:
        form = ConceptForm()
    return  locals()

@ajax_request
def checkConceptFunction(request):
    status = True
    message = "Everything is fine"
    funcsInfo = formalizeFunc(request.GET)
    conceptInfo = Attribute.objects.get(pk=request.GET.get("attribute_id"))
    print "------------HERE IS GET DATA-----------"
    print request.GET
    print funcsInfo
    print conceptInfo
    try:
        validation(conceptInfo,funcsInfo)
    except Exception as e:
        status = False
        message = str(e)
    return {'status': status,'message':message}


@render_to("base_add.html")
def createNegation(request):
    name = "negation"
    if request.method == 'POST':
        print "-----------HERE IS POST DATA-----------"
        print request.POST
        negationName = request.POST.get("name")
        if negationName!="":
            saveNegation(negationName)
        # Functions are passed as a big string separated by \n each
        for func in request.POST.get("funcs", "").split("\v")[:-1]:
            # Values are separated by \t
            vals = func.split("\t")
            dt = vals[0]
            dr = vals[1]
            expr = vals[2]
            print "<%s, %s, %s >" % (dt, dr, expr)
            if negationName!="":
                # if all validation is fine
                saveNegationFunc(dt,dr,expr,negationName)
                data = {"conceptfunctions" : NegationFunction.objects.all(),
                        "concepts" : Negation.objects.all()}
            else:
                form = NegationForm(request.POST)
                error_message = "Some error happened"
        return HttpResponseRedirect('/success/')

    else:
        form = NegationForm()
        function_form = NegationFunctionForm()
    return  locals()

@ajax_request
def checkNegationFunction(request):
    status = True
    message = "Everything is fine"
    funcsInfo = formalizeFunc(request.GET)
    print "------------HERE IS GET DATA-----------"
    print request.GET
    print funcsInfo
    try:
        validation(defaultDomain(),funcsInfo)
    except Exception as e:
        status = False
        message = str(e)
    return {'status': status,
            'message':message}

@render_to("base_add.html")
def createQuantification(request):
    name = "quantification"
    if request.method == 'POST':
        print "-----------HERE IS POST DATA-----------"
        print request.POST
        qName = request.POST.get("name")
        if qName!="":
            saveQuantification(qName)
        # Functions are passed as a big string separated by \n each
        for func in request.POST.get("funcs", "").split("\v")[:-1]:
            # Values are separated by \t
            vals = func.split("\t")
            dt = vals[0]
            dr = vals[1]
            expr = vals[2]
            print "<%s, %s, %s >" % (dt, dr, expr)
            if qName!="":
                # if all validation is fine
                saveQuantificationFunc(dt,dr,expr,qName)
                data = {"conceptfunctions" : QuantificationFunction.objects.all(),
                        "concepts" : Quantification.objects.all()}

            else:
                form = QuantificationForm(request.POST)
                error_message = "Some error happened"
        return HttpResponseRedirect('/success/')
    else:
        form = QuantificationForm()
        function_form = QuantificationFunctionForm()
        
    return  locals()

@ajax_request
def checkQuantificationFunction(request):
    status = True
    message = "Everything is fine"
    funcsInfo = formalizeFunc(request.GET)
    print "------------HERE IS GET DATA-----------"
    print request.GET
    try:
        validation(defaultDomain(),funcsInfo)
    except Exception as e:
        status = False
        message = str(e)
    return {'status': status,
            'message':message}

    
@render_to("loadfile.html")
def loadFile(request):
    if request.method=='POST':
        #print "---------request.POST-------------"
        #print request.POST 
        xmlFile =  request.POST.get("xml")
        dbFile =  request.POST.get("db")
        #print xmlFile
        #print dbFile
        #save into FileInfo
        saveInputFile(xmlFile,dbFile)
        #parsing xml file
        storeMetaDB(xmlFile)
    else:
        form = InputFileForm()
    return locals()

@render_to("query.html")
def query(request):
    
    form=SimpleQueryForm()
    return locals()

@render_to("results.html")
def results(request):
    SimpleQuery.objects.all().delete()
    if request.method == "POST":
        print "---------HERE IS POST DATA-----------"
        print request.POST
        for query in request.POST.get("funcs", "").split("\v")[:-1]:
            vals = query.split("\t")
            n = vals[0]
            q = vals[1]
            c = vals[2]
            print "<%s, %s, %s >" % (n, q, c)
            saveSimpleQuery(n,q,c)
        generateFile()
        fileCombination()
        # excute query over ciaoProlog
       
    else:
        raise "Can't happen"

    return locals()
