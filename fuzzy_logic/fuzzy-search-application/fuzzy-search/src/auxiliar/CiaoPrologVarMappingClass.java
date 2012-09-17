package auxiliar;

import CiaoJava.PLVariable;

public class CiaoPrologVarMappingClass {

	public String name = null;
	public PLVariable variable = null;
	public PLVariable realVariable = null;
	public PLVariable condition = null;
	public Boolean isTemporalVariable = false;
	
	CiaoPrologVarMappingClass(String varName) {
		name = varName;
		variable = new PLVariable();
		realVariable = new PLVariable();
		condition = new PLVariable();
		isTemporalVariable = false;
	}
	
}
