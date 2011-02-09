from webInterface.Lib.fileLib import *
from webInterface.Lib.metaDBLib import *
from webInterface.Lib.configurationLib import *
from webInterface.models import *

xmlfile = "/Users/jingweilu/workspace/PythonVirtualEnv/fuzzyquery/src/RfuzzyWeb/webInterface/IOFile/house.xml"
dbfile = "/Users/jingweilu/workspace/PythonVirtualEnv/fuzzyquery/src/RfuzzyWeb/webInterface/IOFile/house.xml"


def storeMetaDB():
    Table.objects.all().delete()
    Attribute.objects.all().delete()
    #xml = InputFile.objects.all()[0].xmlFile
    readFromXML(xmlfile)
    # display the table DBMeta and AttrDom
    dbMeta = Table.objects.all()[0]
    print dbMeta
    for e in Attribute.objects.all():
        print e

def configure():
    """ interactive configure the fuzzy concept, negation, quantification"""
    pass
    
    
def query():
    n = Negation.objects.get(pk="not")
    q = Quantification.objects.get(pk="very")
    c = Concept.objects.get(pk="far")
    sq = SimpleQuery(negation=n,quantification=q,concept=c)
    sq.save()
    print sq.getQuery()
    
    
