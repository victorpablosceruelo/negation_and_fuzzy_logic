
def is_interval(it):
	sure = eval(it[1])<eval(it[2]) or (eval(it[1])==eval(it[2]) and it[0]=="[" and it[3]=="]" )
	if not sure:
		raise Exception(" ErrorMessage : Illegal Interval ! ")
	else:
		return sure

class Interval:
	def __init__(self, intervalTuple):
		try:
			is_interval(intervalTuple)
		except Exception as e:
			print e
			raise
		else:	
			try:
				self.lower = LowerBound(intervalTuple[0],intervalTuple[1])
			except Exception as e:
				print e
				raise
			else:
				try:
					self.upper = UpperBound(intervalTuple[3],intervalTuple[2])
				except Exception as e:
					print e
					raise
	
	def __str__(self):
		return self.lower.__str__() + ", " + self.upper.__str__() 

	def isSubInDomain(self,intervals):
		result = filter(lambda x:self<=x and x, intervals)
		return len(result)!=0 and result[0]

	def isEqInDomain(self, intervals):
		result = filter(lambda x:self==x and x, intervals)
		return len(result)!=0 and result[0]

	def dis(self, intervals):
		return all(map(lambda x: self.__disjoin(x), intervals))

 	def __disjoin(self, interval):
		return self.lower.disjoint(interval.upper) or self.upper.disjoint(interval.lower)

	def __eq__(self, interval):
		return self.lower==interval.lower and self.upper==interval.upper

	def __lt__(self, interval):
		return self!=interval and self<=interval
	
	def __gt__(self, interval):
		return self!=interval and self>=interval
	
	def __le__(self, interval):
		return self.lower<=interval.lower and self.upper<=interval.upper
	
	def __ge__(self, interval):
		return self.lower>=interval.lower and self.upper>=interval.upper

	def __sub__(self, interval):
		if self>=interval:
			return filter(None,[self.lower-interval.lower,self.upper-interval.upper])
		else:
			print str(interval) + " can not be subtract from " + str(self)
			return None

	def getValue(self,expression):
		f = eval("lambda x:"+expression.replace("x","float(x)"))
		try:
			l=self.lower.getValue(f)
			print l
		except:
			raise
		else:
			try:
				u=self.upper.getValue(f)
				print u
			except:
				raise
			else:
				return l>=0 and u>=0 and [(self.lower.value,l),(self.upper.value,u)]

mark = {"[":" .>=. ",
	"(":" .>. ",
	")":" .<. ",
	"]":" .=<. "}
	
class IntervalPoint:
	def __init__(self, edge, value):	
		self.value = eval(value)
		self.edge = edge

	def __str__(self):
		return self.edge+str(self.value)
		
	@property
	def include(self):
		return self.edge=="[" or self.edge=="]"
	
	def __eq__(self, p):
		return self.value==p.value and self.edge==p.edge

class LowerBound(IntervalPoint):

	def __init__(self,edge,value):
		if edge=="[" or edge=="(":
			IntervalPoint.__init__(self,edge,value)
		else:
			raise Exception(" ErrorMessage : Illegal LowerBound ! ")

	def disjoint(self, upperPt):
		return ~self>=upperPt

	def __lt__(self, lowerPt):
		return not self.include and lowerPt.include and self.value>=lowerPt.value or self.value>lowerPt.value

	def __gt__(self, lowerPt):
		return self.include and not lowerPt.include and self.value<=lowerPt.value or self.value<lowerPt.value
	
	def __le__(self, lowerPt):
		return self<lowerPt or self==lowerPt

	def __ge__(self, lowerPt):
		return self>lowerPt or self==lowerPt

	def __str__(self):
		return "X" + mark[self.edge] + str(self.value)
		
	def __sub__(self, lowerPt):
		if self>lowerPt:
			tmp = ~lowerPt
			return Interval((self.edge,str(self.value),str(tmp.value),tmp.edge))
		elif self==lowerPt:
			return None
		else:
			print str(lowerPt) + " can not be subtracted from " + str(self) 

	def __invert__(self):
		newEdge = self.edge=="[" and ")" or "]"
		return UpperBound(newEdge,str(self.value))

	def getValue(self,f):
		tmp = round(f(self.value))
		print tmp
		if tmp<=1 and tmp>=0:
			return tmp
		else:
			raise Exception(" ErrorMessage : The value of " + str(self) + " is out of bound ! ")
	
class UpperBound(IntervalPoint):

	def __init__(self, edge, value):
		if edge=="]" or edge==")":
			IntervalPoint.__init__(self,edge,value)
		else:
			raise Exception(" ErrorMessage : Illegal UpperBound ! ")

	def disjoint(self, lowerPt):
		return ~self>=lowerPt

	def __lt__(self, upperPt):
		return not self.include and upperPt.include and self.value<=upperPt.value or self.value<upperPt.value

	def __gt__(self, upperPt):
		return self.include and not upperPt.include and self.value>=upperPt.value or self.value>upperPt.value

	def __le__(self, upperPt):
		return self<upperPt or self==upperPt
	
	def __ge__(self, upperPt):
		return self>upperPt or self==upperPt

	def __str__(self):
		return "X" + mark[self.edge] + str(self.value)
	#	return "Upper Bound is " + str(self.value) + self.edge
	

	def __sub__(self, upperPt):
		if self>upperPt:
			tmp = ~upperPt
			return Interval((tmp.edge,str(tmp.value),str(self.value),self.edge))
		elif self==upperPt:
			return None
		else:
			print str(upperPt) + " can not be subtracted from " + str(self)

	def __invert__(self):
		newEdge = self.edge=="]" and "(" or "["
		return LowerBound(newEdge,str(self.value))

	def getValue(self,f):
		tmp = round(f(self.value))
		print tmp
		if tmp<=1 and tmp>=0:
			return tmp
		else:
			raise Exception(" ErrorMessage : The value of " + str(self) + " is out of bound ! ")
