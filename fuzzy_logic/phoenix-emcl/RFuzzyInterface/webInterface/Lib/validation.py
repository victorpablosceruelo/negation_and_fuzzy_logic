from webInterface.Lib.domainLib import *

def valDomain(dt, dr):
    if dt=="C":
        try:
            is_cdomain(dr)
        except:
            raise Exception(" ErrorMessage : Continuous Domain can not be built ! ")
        else:
            return True
    elif dt=="D":
        try:
            is_ddomain(dr)
        except:
            raise Exception(" ErrorMessage : Discrete Domain can not be built ! ")
        else:
            return True
    else:
        raise Exception(" ErrorMessage : Domain Type is Illegal ! ")

def valDomainTypes(cdt, fdt):
    correct = cdt=="D" and fdt=="D" or cdt=="C"
    if correct==False:
        raise Exception(" Domain type of function is not available ! ")
    else:
        return correct

def valFunctions(funcsInfo, conceptInfo):
    for funcInfo in funcsInfo:
        valFunction(funcInfo, conceptInfo)
    if len(funcsInfo)>1: 
        disjoin(funcsInfo)

def valDomainsSubsumption(conceptInfo, funcInfo):
    cdt = conceptInfo.domain_type
    cdr = conceptInfo.domain_range
    fdt = funcInfo["type"]
    fdr = funcInfo["range"]
    result = False
    if cdt=="C" and fdt=="C":
        cdom = Continuous(cdr)
        fdom = Continuous(fdr)
        result = fdom<=cdom
    elif cdt=="D" and fdt=="D":
        cdom = Discrete(cdr)
        fdom = Discrete(fdr)
        result = fdom<=cdom
    elif cdt=="C" and fdt=="D":
        cdom = Continuous(cdr)
        fdom = Discrete(fdr).intoContinuous()
        result = fdom<=cdom
    else:
        raise Exception(" ErrorMessage : Domain type of function is not available ! ")
    if result == False:
        raise Exception(" ErrorMessage : " + str(fdr) + "is not a subdomain of " + str(cdr) + " ! ")
    else:
        return True


def valDomainsDisjoin(f1, f2):
    f1t=f1["type"]
    f1r=f1["range"]
    f2t=f2["type"]
    f2r=f2["range"]
    if f1t=="C" and f2t=="C":
        if Continuous(f1r).disjoin(Continuous(f2r)):
            return True
        else:
            raise Exception(" ErrorMessage : overlapped domains of \n" + str(f1) + "\n" + str(f2))
    elif f1t=="D" and f2t=="D":
        if Discrete(f1r)&Discrete(f2r)!=[]:
            raise Exception(" ErrorMessage : overlapped domains of \n" + str(f1) + "\n" + str(f2))
        else:
            return True
    elif f1t=="C" and f2t=="D":
        if Continuous(f1r).disjoin(Discrete(f2r).intoContinuous()):
            return True
        else:
            raise Exception(" ErrorMessage : overlapped domains of \n" + str(f1) + "\n" + str(f2))
    elif f1t=="D" and f2t=="C":
        if Continuous(f2r).disjoin(Discrete(f1r).intoContinuous()):
            return True
        else:
            raise Exception(" ErrorMessage : overlapped domains of \n" + str(f1) + "\n" + str(f2))
    else:
        raise Exception(" ErrorMessage : Domain type of function is not available ! ")

def disjoinDomains(f, fs):
     #correct = all([valDomainsDisjoin(f,x) for x in fs])
    for x in fs:
        try:
            valDomainsDisjoin(f,x)     
        except:
            raise
    return True

def disjoin(funcsInfo):
    size = len(funcsInfo)
    #dis = [x==size-1 or disjoinDomains(funcsInfo[x],funcsInfo[x+1:]) for x in range(size)]
    for x in range(size):
        if x!=size-1:
            try:
                disjoinDomains(funcsInfo[x],funcsInfo[x+1:])
            except Exception as e:
                print e
                raise
    return True

def valExpression(funcInfo):
    dt = funcInfo["type"]
    dr = funcInfo["range"]
    if dt=="C":
        d = Continuous(dr)
    elif dt=="D":
        d = Discrete(dr)
    else:
        raise Exception(" Domain type is not validated ! ")
    try:
        function = d.valueFunction(funcInfo["expression"])
    except:
        raise
    else:
        return function

def valFunction(funcInfo, conceptInfo):
    try:
        # function domain type and concept domain type
        valDomainTypes(conceptInfo.domain_type, funcInfo["type"])
        print "No Problems with Types"
        # function domain range
        valDomain(funcInfo["type"],funcInfo["range"])
        print "No Problems with Function Domain"
        # function domain range is in concept domain range
        valDomainsSubsumption(conceptInfo, funcInfo)
        print "No problems with relation between Function Domain and Concept Domain"
        # eval function is in [0,1]
        function = valExpression(funcInfo)
        print "No problem with Function value"
    except Exception as e:
        print e
        raise
    else:
        #print function
        return True
                

def validation(conceptInfo, funcsInfo):
    cDT = conceptInfo.domain_type # concept domain type
    cDR = conceptInfo.domain_range # concept domain range
    try:
        valDomain(cDT, cDR) # validate concept domain
        print "Concept Domain is fine"
        print "----------funcsInfo------------"
        print funcsInfo
    except Exception as e:
        print e
    else:
        try:
            valFunctions(funcsInfo, conceptInfo) # validate all the functions
        except:
            raise
            print " ErrorMessage : Function is not validate ! "
    
