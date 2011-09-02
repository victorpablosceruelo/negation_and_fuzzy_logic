from webInterface.models import *
from webInterface.forms import *
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

@render_to("base_edit.html")
def editConcept(request):
    name = "concept"
    entity = Concept.objects.get(pk=request.GET.get("pk"))
    function_form = ConceptFunctionForm()
    functions = []
    for func in entity.conceptfunction_set.all():
        d_type = "Discrete" if func.domain_type == "D" else "Continuous"
        val = "%s %s %s" % (d_type, func.domain_range, func.expression)
        encoded_val = "%s\t%s\t%s" % (func.domain_type, func.domain_range, func.expression)
        functions.append((func.id, val, encoded_val))

    return locals()

@ajax_request
def deleteFunction(request):
    id = request.GET.get("id","")
    if id:
        func = ConceptFunction.objects.get(id=id)
        func.delete()
        status = True
    else:
        status = False
    return {"status": status}


@ajax_request
def checkAndAddConceptFunction(request):
    status = True
    message = "Everything is fine"
    funcsInfo = formalizeFunc(request.GET)
    conceptInfo = Attribute.objects.get(pk=request.GET.get("attribute_id"))
    conceptName = request.GET.get("entity_name")
    print "------------HERE IS GET DATA-----------"
    print request.GET
    print funcsInfo
    print conceptInfo
    print conceptName
    try:
        validation(conceptInfo,funcsInfo)
    except Exception as e:
        status = False
        message = str(e)
    else:
        Concept.objects.get(pk=conceptName).conceptfunction_set.all().delete()
        for func in funcsInfo:
            dt = func["type"]
            dr = func["range"]
            expr = func["expression"]
            print "<%s, %s, %s >" % (dt, dr, expr)
            saveConceptFunc(dt,dr,expr,conceptName)
    return {"status":status, "message":message}

@render_to("base_edit.html")
def editNegation(request):
    name = "negation"
    entity = Negation.objects.get(pk=request.GET.get("pk"))
    function_form = NegationFunctionForm()
    functions = []
    for func in entity.negationfunction_set.all():
        d_type = "Discrete" if func.domain_type == "D" else "Continuous"
        val = "%s %s %s" % (d_type, func.domain_range, func.expression)
        encoded_val = "%s\t%s\t%s" % (func.domain_type, func.domain_range, func.expression)
        functions.append((func.id, val, encoded_val))

    return locals()

@ajax_request
def deleteNegation(request):
    id = request.GET.get("id","")
    if id:
        func = NegationFunction.objects.get(id=id)
        func.delete()
        status = True
    else:
        status = False
    return {"status": status}

@render_to("base_edit.html")
def editQuantification(request):
    name = "quantification"
    entity = Quantification.objects.get(pk=request.GET.get("pk"))
    function_form = QuantificationFunctionForm()
    functions = []
    for func in entity.quantificationfunction_set.all():
        d_type = "Discrete" if func.domain_type == "D" else "Continuous"
        val = "%s %s %s" % (d_type, func.domain_range, func.expression)
        encoded_val = "%s\t%s\t%s" % (func.domain_type, func.domain_range, func.expression)
        functions.append((func.id, val, encoded_val))

    return locals()

@ajax_request
def deleteQuantification(request):
    id = request.GET.get("id","")
    if id:
        func = QuantificationFunction.objects.get(id=id)
        func.delete()
        status = True
    else:
        status = False
    return {"status": status}


@ajax_request
def checkAndAddNegationFunction(request):
    status = True
    message = "Everything is fine"
    funcsInfo = formalizeFunc(request.GET)
    negationName = request.GET.get("entity_name")
    print "------------HERE IS GET DATA-----------"
    print request.GET
    print funcsInfo
    try:
        validation(defaultDomain(),funcsInfo)
    except Exception as e:
        status = False
        message = str(e)
    else:
        Negation.objects.get(pk=negationName).negationfunction_set.all().delete()
        for func in funcsInfo:
            dt = func["type"]
            dr = func["range"]
            expr = func["expression"]
            print "<%s, %s, %s >" % (dt, dr, expr)
            saveNegationFunc(dt,dr,expr,negationName)
    return {'status': status,
            'message':message}

@ajax_request
def checkAndAddQuantificationFunction(request):
    status = True
    message = "Everything is fine"
    funcsInfo = formalizeFunc(request.GET)
    quantificationName = request.GET.get("entity_name")
    print "------------HERE IS GET DATA-----------"
    print request.GET
    print funcsInfo
    try:
        validation(defaultDomain(),funcsInfo)
    except Exception as e:
        status = False
        message = str(e)
    else:
        Quantification.objects.get(pk=quantificationName).quantificationfunction_set.all().delete()
        for func in funcsInfo:
            dt = func["type"]
            dr = func["range"]
            expr = func["expression"]
            print "<%s, %s, %s >" % (dt, dr, expr)
            saveQuantificationFunc(dt,dr,expr,quantificationName)
    return {'status': status,
            'message':message}
