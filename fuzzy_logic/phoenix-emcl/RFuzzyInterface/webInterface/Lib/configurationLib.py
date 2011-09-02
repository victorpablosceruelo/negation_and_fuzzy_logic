from webInterface.models import *

def saveConcept(conceptName, attrpk):
    attrdom = Attribute.objects.get(pk=attrpk)
    concept = Concept(name=conceptName, attribute=attrdom)
    concept.save()

def saveConceptFunc(dt, dr, expr, conceptName):
    con = Concept.objects.get(pk=conceptName) #not solved
    conceptFunc = ConceptFunction(domain_type=dt, domain_range=dr, expression=expr, concept=con)
    conceptFunc.save()

def saveNegation(negationName):
    negation = Negation(pk=negationName)
    negation.save()

def saveNegationFunc(dt, dr, expr, negationName):
    neg = Negation.objects.get(pk=negationName) # not solved
    negationFunc = NegationFunction(domain_type=dt, domain_range=dr, expression=expr, negation=neg)
    negationFunc.save()

def saveQuantification(qName):
    quantification = Quantification(pk=qName)
    quantification.save()

def saveQuantificationFunc(dt, dr, expr, qName):
    quan = Quantification.objects.get(pk=qName) # not solved
    quantificationFunc = QuantificationFunction(domain_type=dt, domain_range=dr, expression=expr, quantification=quan)
    quantificationFunc.save()

def saveSimpleQuery(negator,quantifier,con):
    n = Negation.objects.get(pk=negator)
    q = Quantification.objects.get(pk=quantifier)
    c = Concept.objects.get(pk=con)
    sq = SimpleQuery(negation=n, quantification=q, concept=c)
    sq.save()
