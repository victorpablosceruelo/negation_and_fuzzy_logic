from webInterface.models import *
from xml.dom.minidom import parse


def readFromXML(xmlFile):
    dom = parse(xmlFile)
    saveDBMeta(readTableName(dom), readPrimaryKey(dom))
    saveSchema(dom)

def saveDBMeta(tableName,primaryKey):
    dbMeta = Table(name=tableName, p_key=primaryKey)
    dbMeta.save()

def saveAttrDom(attrdom):
    attrDom = Attribute(name=attrdom[0],domain_type=attrdom[1],domain_range=attrdom[2])
    attrDom.save()

def readTableName(dom):
    return str(dom.documentElement.tagName)

def readPrimaryKey(dom):
    es = dom.getElementsByTagName("PrimaryKey")
    keys = str([readText(e) for e in es])
    return keys

def readText(leafNode):
    return str(leafNode.childNodes[0].data).strip()

def saveSchema(dom):
    es = dom.getElementsByTagName("AttrDom")
    for e in es:
        saveAttrDom(readAttrDom(e))

def readAttrDom(adNode):
    attr = readAttribute(adNode)
    dom = readDomain(adNode)
    attrdom = tuple([attr,dom[0],dom[1]])
    return attrdom

def readAttribute(adNode):
    return readText(adNode.getElementsByTagName("Attribute")[0])

def readDomain(adNode):
    d = adNode.getElementsByTagName("Domain")[0]
    domainType = readText(d.getElementsByTagName("Type")[0])
    domainRange = readText(d.getElementsByTagName("Range")[0])
    return [domainType,domainRange]


def storeMetaDB(xml):
    Table.objects.all().delete()
    Attribute.objects.all().delete()
    #xml = InputFile.objects.all()[0].xmlFile
    readFromXML(xml)
    # display the table DBMeta and AttrDom
    #dbMeta = Table.objects.all()[0]
    #print dbMeta
    #for e in Attribute.objects.all():
    #    print e
