from webInterface.models import *
from webInterface.Lib.translationLib import *
#path = '/Users/jingweilu/workspace/PythonVirtualEnv/fuzzyquery/src/RfuzzyWeb/webInterface/IOFile/'

path=os.path.join(PROJECT_DIR, "webInterface/IOFile/")

head = [":- module(query,_,[clpr,rfuzzy,debugger_pkg]).\n",
        ":- use_module(engine(hiord_rt)).\n"]
files = [path+"fuzzy_head.pl",InputFile.objects.all()[0].db,path+"fuzzy_db.pl",path+"fuzzy_query.pl",path+"fuzzy_ciao.pl"]
def saveInputFile(xmlFile, dbFile):
    """store xmlFile and dbFile into table FileInfo"""
    InputFile.objects.all().delete()
    inputFile = InputFile(xml=xmlFile, db=dbFile)
    inputFile.save()
    print InputFile.objects.all()


def writeInRFuzzy(filename,content):
    funFile = open(path+filename,"a")
    funFile.writelines(content)
    funFile.close()

def generateFuzzyFile(filename):
    writeInRFuzzy(filename,generateMiddleWareFunction())
    writeInRFuzzy(filename,generateFuzzyFunction())

def generateQueryFile(filename):
    writeInRFuzzy(filename,generateQuery())

def generateFileHead(filename):
    writeInRFuzzy(filename,head)

def generateCiaoInterface(filename):
    writeInRFuzzy(filename,generateInterfaceToCiao())

def generateFile(fdb="fuzzy_db.pl",fquery="fuzzy_query.pl",fhead="fuzzy_head.pl", fCiao="fuzzy_ciao.pl"):
    removeFiles(fdb,fquery,fhead)
    generateFileHead(fhead)
    generateFuzzyFile(fdb)
    generateQueryFile(fquery)
    generateCiaoInterface(fCiao)

def removeFiles(fdb="fuzzy_db.pl",fquery="fuzzy_query.pl",fhead="fuzzy_head.pl",fCiao="fuzzy_ciao.pl"):
    os.path.exists(path+fdb) and os.remove(path+fdb)
    os.path.exists(path+fquery) and os.remove(path+fquery)
    os.path.exists(path+fhead) and os.remove(path+fhead)
    os.path.exists(path+fCiao) and os.remove(path+fCiao)

def fileCombination(listOfFiles=files,file_name="fuzzy_QA.pl"):
	"""input : listOfFile is a list
	output: return a new file named final_name
	append all the contents from each files in the listOfFile into 
	a new file"""
        os.path.exists(path+file_name) and os.remove(path+file_name)
	final_file = open(path + file_name,"a")
	for f in listOfFiles:
            fObj = open(f)
            content = fObj.read()
            final_file.write(content)
            fObj.close()
	final_file.close()
	print file(path+file_name).read()	
