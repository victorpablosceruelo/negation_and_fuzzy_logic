from django.db import models
from webInterface.Lib.domainLib import *
from settings import PROJECT_DIR
import os

DOMAIN_CHOICES = (
    ('C','Continuous'),
    ('D','Discrete'),
    )

class InputFile(models.Model):
    
    db = models.FilePathField(path=os.path.join(PROJECT_DIR, "webInterface/IOFile/"))
    xml = models.FilePathField(path=os.path.join(PROJECT_DIR, "webInterface/IOFile/"))

    def __unicode__(self):
        return "dbFile: %s \n xmlFile: %s \n" % (self.db, self.xml)
 
class Table(models.Model):

    name = models.CharField(max_length=20)
    p_key = models.CharField(max_length=100)

    def __unicode__(self):
        return "Table %s, pk %s" % (self.name, self.p_key)

    def getPKterm(self):
        pkterm = ",".join(eval(self.p_key)).upper()
        return pkterm

class Attribute(models.Model):

    name = models.CharField(max_length=20)
    domain_type = models.CharField(max_length=1, choices=DOMAIN_CHOICES)
    domain_range = models.CharField(max_length=100)
    #table = models.ForeignKey(Table)

    def __unicode__(self):
        #return "Attribute %s, type %s, range %s" % (self.name, self.domain_type, self.domain_range)
        return self.name

   
class Concept(models.Model):

    name = models.CharField(max_length=20, primary_key=True)
    attribute = models.ForeignKey(Attribute)

    def __unicode__(self):
        #return "Concept %s, attribute %s" % (self.name, self.attribute)
        return self.name

class Negation(models.Model):

    name = models.CharField(max_length=20,primary_key=True)

    def __unicode__(self):
        #return "Negation %s" % (self.name)
        return self.name

class Quantification(models.Model):

    name = models.CharField(max_length=20,primary_key=True)

    def __unicode__(self):
        #return "Quantifier %s" % (self.name)
        return self.name

class Function(models.Model):

    """Abstract class, never used directly"""


    domain_type = models.CharField(max_length=1, choices=DOMAIN_CHOICES)
    domain_range = models.CharField(max_length=100)
    expression = models.CharField(max_length=50)

    def getFunction(self):
        if self.domain_type=="C":
            domain = Continuous(self.domain_range)
        elif self.domain_type=="D":
            domain = Discrete(self.domain_range)
        function = domain.getFunction(self.expression)
        return function

    class Meta:
        abstract = True


class ConceptFunction(Function):

    concept = models.ForeignKey(Concept)

    def __unicode__(self):
        return "ConceptFunc: expression %s, range %s" % (self.expression, self.domain_range)
        
class NegationFunction(Function):

    negation = models.ForeignKey(Negation)

    def __unicode__(self):
        return "NegationFunc: expression %s, range %s" % (self.expression, self.domain_range)


class QuantificationFunction(Function):

    quantification = models.ForeignKey(Quantification)

    def __unicode__(self):
        return "QuantificationFunc: expression %s, range %s" % (self.expression, self.domain_range)

    
class SimpleQuery(models.Model):

    negation = models.ForeignKey(Negation)
    quantification = models.ForeignKey(Quantification)
    concept = models.ForeignKey(Concept)

    def __unicode__(self):
        return " SimpleQuery: (%s, %s, %s) " % (self.negation, self.quantification, self.concept)
   
    def getQuery(self):
        pkTerm = Table.objects.all()[0].getPKterm()
        head = self.negation.name + "_" + self.quantification.name + "_" + self.concept.name + "(" + pkTerm + ", V)"
        nAtom = self.negation.name + "_func(V2, V)"
        qAtom = self.quantification.name + "_func(V1, V2)"
        cAtom = self.concept.name + "(" + pkTerm +", V1)"
        #body = nAtom + ", " + qAtom + ", " + cAtom
        body = cAtom + ", " + qAtom + ", " + nAtom
        query = head + " :- " + body + ".\n"
        return query

    def getHead(self):
        pkTerm = Table.objects.all()[0].getPKterm()
        head = self.negation.name + "_" + self.quantification.name + "_" + self.concept.name + "(" + pkTerm + ", V)"
        return head
#not_very_expensive(FLAT_NUMBER,V):-not_func(V2,V),very_func(V1,V2),expensive(FLAT_NUMBER,V1).



