package auxiliar;

import CiaoJava.PLVariable;

public class CiaoPrologVarsMappingClass {

	private CiaoPrologVarMappingClass [] arrayOfMappings = null;
	private int size = 0;
	private int realSize = 0;
	
	public CiaoPrologVarsMappingClass(int size) {
		this.size = size;
		arrayOfMappings = new CiaoPrologVarMappingClass [size];
		for (int i=0; i<size; i++){
			arrayOfMappings[i].name = null;
			arrayOfMappings[i].variable = null;
		}
	}
	
	public PLVariable returnMapping(String varName) {
		int i = 0;
		while ((i < size) && (arrayOfMappings[i] != null) && (! arrayOfMappings[i].equals(varName))) {
			i++;
		}
		if (i == size) return null; // In case we get more vars than we can manage.
		else {
			if (arrayOfMappings[i] == null) {
				arrayOfMappings[i].name = varName;
				arrayOfMappings[i].variable = new PLVariable();
				realSize ++;
			}
			return arrayOfMappings[i].variable;
		}
	}
	
	public PLVariable [] returnVariablesArray() {
		PLVariable [] variables = new PLVariable [realSize];
		int i = 0;
		while ((i < size) && (arrayOfMappings[i] != null)) {
			variables[i] = arrayOfMappings[i].variable;
			i++;
		}
		return variables;
	}
	
}
