package auxiliar;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class CiaoPrologProgramElementInfoClass {
	private static final Log LOG = LogFactory.getLog(CiaoPrologProgramElementInfoClass.class);
	
	String type = null;
	String name = null;
	String arity = null;
		
	public void setPredicateType(String typeIn) { type = typeIn; }
	public void setPredicateName(String nameIn) { name = nameIn; }
	public void setPredicateArity(String arityIn) { arity = arityIn; }
	
	public String getPredicateType() {	return(type); }
	public String getPredicateName() {	return(name); }
	public String getPredicateArity() { return (arity); }
	
	public void log_info() {
		LOG.info("DataBaseInfoClass: Predicate: Type: " + type + " Name: " + name + " Arity: " + arity);
	}
}
