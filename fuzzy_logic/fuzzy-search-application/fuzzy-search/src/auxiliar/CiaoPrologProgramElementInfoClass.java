package auxiliar;

public class CiaoPrologProgramElementInfoClass {
	
	String type = null;
	String name = null;
	String arity = null;
		
	void setPredicateType(String typeIn) {
		type = typeIn;
	}
	void setPredicateName(String nameIn) {
		name = nameIn;
	}
	void setPredicateArity(String arityIn) {
		arity = arityIn;
	}
	
	String getPredicateType() {
		return(type);
	}
	String getPredicateName() {
		return(name);
	}
	String getPredicateArity() {
		return (arity);
	}	
}
