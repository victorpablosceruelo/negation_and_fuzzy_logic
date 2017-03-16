import re
from webInterface.Lib.intervalLib import *
from copy import copy, deepcopy

def is_ddomain(domainStr):#need to improve
    evaled = None
    try:
        evaled = eval(domainStr)
    except:
        raise Exception(" Error Message : The Discrete Domain" + domainStr + "is Illegal! ")
    else:
        return evaled

class Discrete(list):
    def __init__(self, domainStr):
        """domainStr: represents Discrete Domain, in a form [el1, el2, ...] functionality: create a list of elements according to domainStr"""
        list.__init__(self)
        try:
            evaled = is_ddomain(domainStr)
        except Exception as e:
            print e
        else:
            self.extend(evaled)
        
    def __and__(self,domain):
        return Discrete(str(list(set(self)&set(domain))))

    def __sub__(self, domain):
        """functionality: return self-domain"""
        return self>=domain and Discrete(str(list(set(self)-set(domain)))) or str(domain)+" can not be subtracted from \n"+str(self)
	
    def __eq__(self, domain):
        """functionality: check whether self==domain"""
        return set(self)==set(domain)
	
    def __lt__(self, domain):
        """functionality: check whether self<domain, all the elements in self are in domain, but not vice versa"""
        return set(self)<set(domain)

    def __gt__(self, domain):
        """functionality: check whether self>domain, all the elements in domain are in self, but not vice versa"""
        return set(self)>set(domain)

    def __le__(self, domain):
        """functionality: check whether self<=domain, all the elements in self are in domain"""
        return set(self)<=set(domain)
	
    def __ge__(self, domain):
        """functionality: check whether self>=domain, all the elements in domain are in self"""
        return set(self)>=set(domain)

    def __str__(self):
        """functionality: presents Discrete Domain as a string"""
        return ",".join([str(x) for x in self])

    def intoContinuous(self):
        domStr = str([[x,x] for x in self])
        return Continuous(domStr)

   
    def getFunction(self,expression):
        f = eval("lambda x:"+expression.replace("x","float(x)"))
        values = map(f,self)
        if all(map(lambda x:x<=1,values)):
            return zip(self,values)
        else:
            raise Exception(" ErrorMessage : The value of function is out of bound ! ")

    def valueFunction(self,expression):
        f = eval("lambda x:"+expression.replace("x","float(x)"))
        values = map(f,self)
        if all(map(lambda x:x<=1,values)):
            return zip(self,values)
        else:
            raise Exception(" ErrorMessage : The value of function is out of bound ! ")


def legal_continuous(cdomain):
    """functionality: check whether Continuous Domain is legal"""
    if not all(map(lambda x: x==len(cdomain)-1 or cdomain[x].dis(cdomain[x+1:]),range(len(cdomain)))):
        raise Exception(" ErrorMessage : Overlapped domain ! ")

def parse_continuous(domainStr):
    """functionality: parse domainStr into a list of intervals"""
   # p = "\[\s*(\s*[\(|\[]\s*\d+\s*,\s*\d+\s*[\]|\)]\s*,\s*)*[\[|\(]\s*\d+\s*,\s*\d+\s*[\]|\)]\s*\]"
   # pattern = """([\(|\[])			# begin with ( or [
#	       \s*(\d+)\s*,                     # begin with numbers possibly with space around, followed by ,
#	       \s*(\d+)\s*			# begin with numbers possibly with space around, 
#	       ([\)|\]])			# begin with ) or ]
#	       """
    p = "\[\s*(\s*[\(|\[]\s*(\d+\.)?\d+\s*,\s*(\d+\.)?\d+\s*[\]|\)]\s*,\s*)*[\[|\(]\s*(\d+\.)?\d+\s*,\s*(\d+\.)?\d+\s*[\]|\)]\s*\]"
    pattern = """([\(|\[])			# begin with ( or [
	       \s*([\d\.]+)\s*,                     # begin with numbers possibly with space around, followed by ,
	       \s*([\d\.]+)\s*			# begin with numbers possibly with space around, 
	       ([\)|\]])			# begin with ) or ]
	       """
    cdomain = []
    if re.match(p, domainStr)!=None:
        intervals = re.findall(pattern, domainStr, re.VERBOSE)
        for e in intervals:
            try:
                interval = Interval(e)
            except:
                raise
            else:
                cdomain.append(interval)
        return cdomain
    else:
        raise Exception( " ErrorMessage : Continuous Domain " + domainStr + " is syntatic wrong! ")
    

def is_cdomain(domainStr):
    try:
        domain = parse_continuous(domainStr)
    except Exception as e:
        print e
        raise
    else:
        if domain!=[]:
            try:
                legal_continuous(domain)
            except Exception as e:
                print e
                raise
        else:
            raise Exception(" ErrorMessage : Empty Domain! ")
    return  domain

class Continuous(list):
    def __init__(self, domainStr):
        """domainStr: represents Continuous Domain, in a form [[lower1,upper1], [lower2,upper2], ...] functionality: create a list of intervals according to domainStr"""
        list.__init__(self)
        try:
            domain = is_cdomain(domainStr)
        except:
            print " ErrorMessage : Continuous Domain can not be built ! "
        else:
            self.extend(domain)

    def __str__(self):
        """functionality: presents Continuous Domain as a string"""
        return "\n".join([str(x) for x in self])

    def disjoin(self, domain):
        return all([x.dis(domain) for x in self])

    def __eq__(self, domain):
        """functionality: check whether self==domain"""
        return deepcopy(self).__eqRecursive(deepcopy(domain))
		
    def __eqRecursive(self, domain):
        """functionality: check whether self==domain in a recursive way"""
        if len(self)!=len(domain):
            return False		
        elif list(self).__eq__([]) and list(domain).__eq__([]):
            return True
        else:
            e = domain[0].isEqInDomain(self)
            if not e:
                return False
            else:
                self.remove(e)
                domain.remove(domain[0])
                return self.__eqRecursive(domain)		

    def __lt__(self, domain):
        """functionality: check whether self<domain, all the intervals in self are subintervals of some intervals in domain, but not vice versa"""
        return all([x.isSubInDomain(domain) for x in self])
	
    def __gt__(self, domain):
        """functionality: check whether self>domain, all the intervals in domain are subintervals of some intervals in self, but not vice versa"""
        return domain.__lt__(self)

    def __le__(self, domain):
        """functionality: check whether self<domain, all the intervals in self are subintervals of some intervals in domain"""
        return self<domain or self==domain

    def __ge__(self, domain):
        """functionality: check whether self<domain, all the intervals in domain are subintervals of some intervals in self"""
        return self>domain or self==domain

    def __sub__(self, domain):
        """functionality: list of intervals "self" minus list of intervals "domain" """
        if self>=domain:
            return deepcopy(self).__subRecursive(deepcopy(domain))
        else:
            print str(domain) + "can not be subtracted from \n" + str(self)

    def __subRecursive(self, domain):
        if domain==[]:
            return self
        else:
            subtrahend = domain[0]
            minuend = subtrahend.isSubInDomain(self)
            intervals = minuend - subtrahend
            self.remove(minuend)
            domain.remove(subtrahend)
            self.extend(intervals)
            return self.__subRecursive(domain)
    
    def valueFunction(self,expression):
        function=[]
        for interval in self:
            try:
                value = interval.getValue(expression)
                print value
            except:
                raise
            else:
                function.extend(value)
        return function
    
    def getFunction(self,expression):
        function=[]
        for interval in self:
            try:
                subFunc = str(interval)+ ", " + "Y .=. " + expression.upper() + ".\n"
            except:
                raise
            else:
                function.append(subFunc)
        return function
