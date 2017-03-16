from webInterface.models import *

def generateFuzzyFunction():
    """generate the file including functions from models ConceptFunction, NegationFunction, and QuantificationFunction"""
    allFunctions = []
    print "-------------Fuzzy Concept and its Functions------------"
    allFunctions.append("% Fuzzy Concept Functions \n")
    for c in Concept.objects.all():
        #print ">>>>>>>>>>>>"+ c.name + "<<<<<<<<<<<<<<<<<<<"
        #print "\n".join(str(x) for x in c.conceptfunction_set.all())
        cf = getRfuzzyFuncs(c, "concept")
        print cf
        if cf!=[]:
            allFunctions.extend(cf)
    print "-------------Negation and its Functions-----------------"
    allFunctions.append("% Neagtion Functions \n")
    for n in Negation.objects.all():
        #print ">>>>>>>>>>>>"+ n.name + "<<<<<<<<<<<<<<<<<<<"
        #print "\n".join(str(x) for x in e.negationfunction_set.all())
        nf = getRfuzzyFuncs(n, "negation")
        print nf
        if nf!=[]:
            allFunctions.extend(nf)
    print "-------------Quantification and its Functions-----------"
    allFunctions.append("% Quantification Functions \n")
    for q in Quantification.objects.all():
        #print ">>>>>>>>>>>>"+ q.name + "<<<<<<<<<<<<<<<<<<<"
        #print "\n".join(str(x) for x in e.quantificationfunction_set.all())
        qf = getRfuzzyFuncs(q, "quantification")
        print qf
        if qf!=[]:
            allFunctions.extend(qf)
    return allFunctions
    
def getRfuzzyFuncs(e, oType):
    eFunctions = eval("e."+oType+"function_set")
    functions = []
    discreteFunctions = eFunctions.filter(domain_type="D")
    if discreteFunctions.count()!=0:
        drfs = getDRfuzzyFuncs(str(e.name)+"_func", discreteFunctions)
        functions.extend(drfs)
    continuousFunctions = eFunctions.filter(domain_type="C")
    #print continuousFunctions.count()
    if continuousFunctions.count()!=0:
        crfs = getCRfuzzyFuncs(str(e.name)+"_func", continuousFunctions)
        functions.extend(crfs)        
    return functions
# getFunction in models have to be changed
def getDRfuzzyFuncs(name, discreteFunctions):
    funcs = []
    for df in discreteFunctions:
        funcs.extend(df.getFunction())
    rfuzzyFunc = [name+"("+str(individual) + "," + str(value) + ").\n" for (individual,value) in funcs]
    return rfuzzyFunc

def getCRfuzzyFuncs(name, continuousFunctions):
    rfuzzyFunc = []
    for cf in continuousFunctions:
        for e in cf.getFunction():
            rfuzzyFunc.append(name + "(X,Y) :- " + e)
    return rfuzzyFunc

def generateMiddleWareFunction():
    #expensive(X,Y):- house(X,_,_,_,P,_,_),expensive_func(P,Y).
    functions = [getClause(str(e.name)) for e in Concept.objects.all()]
    functions.insert(0,"% Fuzzy concept generated from database \n")
    return functions
    
def getClause(conceptName):
    return getHead(conceptName) + " :- " + getBody(conceptName) + ".\n"

def getBody(conceptName):
    return getDBAtom(conceptName)+","+getFuzzyConceptAtom(conceptName)

def getHead(conceptName):  
    return conceptName + "("+Table.objects.all()[0].getPKterm()+",V)"

def getFuzzyConceptAtom(conceptName):
    attributeName =  str(Concept.objects.get(pk=conceptName).attribute.name).upper()
    return conceptName+"_func("+attributeName+",V)"

def getDBAtom(conceptName):
    attribute =  Concept.objects.get(pk=conceptName).attribute
    attributeName = str(attribute.name).upper()
    position = attribute.id
    return getDBAtomPattern(position) %attributeName

def getDBAtomPattern(position):
    predicate = str(Table.objects.all()[0].name).lower()
    prefix = list("_"*(position-1))
    postfix = list("_"*(Attribute.objects.count()-position))
    keyword = Table.objects.all()[0].getPKterm()
    term = []
    term.append(keyword)
    term.extend(prefix)
    term.append("%s")
    term.extend(postfix)
    atom_pattern = predicate + "(" + ",".join(term) + ")"
    return atom_pattern
    
def generateQuery():
    queries = [sq.getQuery() for sq in SimpleQuery.objects.all()]
    queries.insert(0,"% Query List \n")
    return queries

def generateFuzzyInterfaceToCiao():
#q1 :- findall((Y,[X]),not_very_expensive(Y,X),Ans),show(Ans).
#main :- q1,nl,q2.
    print "test Query" 
    print SimpleQuery.objects.all()
    complexQuery = []
    interface = []
    for sq in SimpleQuery.objects.all():
        head = "q" + str(sq.id)
        pkTerm = Table.objects.all()[0].getPKterm()
        body = "findall((%s,[V]),%s,Ans),show(Ans)" % (pkTerm,sq.getHead())
        query = head + " :- " + body + ".\n"
        interface.append(query)
        complexQuery.append(head)
    mainbody = ",nl,".join(complexQuery)
    mainbody = mainbody + ",nl."
    interface.append("main :- " + mainbody + "\n")
    return interface

def getCrispAtom():
     predicate = str(Table.objects.all()[0].name).lower()
     keyword = Table.objects.all()[0].getPKterm()
     fix = list("_"*Attribute.objects.count())
     term = []
     term.append(keyword)
     term.extend(fix)
     atom_pattern = predicate + "(" + ",".join(term) + ")"
     return atom_pattern

def generateCrispInterfaceToCiao():
#    main :- findall(FLAT_NUMBER,house(FLAT_NUMBER,_,_,_,_,_,_),L),write(L).
    pattern = "main :- findall(%s,%s,L),write(L)."
    keyword = Table.objects.all()[0].getPKterm()
    atom = getCrispAtom()
    return pattern % (keyword, atom)
    
