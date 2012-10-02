package auxiliar;

import CiaoJava.PLVariable;

public class CiaoPrologVarsMappingClass {

	private CiaoPrologVarMappingClass [] arrayOfMappings = null;
	private int size = 0;
	private int realSize = 0;
	private int index = 0;
	
	public CiaoPrologVarsMappingClass(int size) {
		this.size = size * 3; // 2 should be enough, but we have quantifiers.
		arrayOfMappings = new CiaoPrologVarMappingClass [this.size];
		for (index=0; index<this.size; index++) arrayOfMappings[index] = null;
		index = 0;
	}
	
	public PLVariable returnMapping(String varName) {
		index = 0;
		while ((index < size) && (arrayOfMappings[index] != null) && (! arrayOfMappings[index].equals(varName))) {
			index++;
		}
		if (index == size) return null; // In case we get more vars than we can manage.
		if ((index < size) && (index == realSize) && (arrayOfMappings[index] == null)) {
			arrayOfMappings[index] = new CiaoPrologVarMappingClass(varName);
			realSize ++;
		}
		return arrayOfMappings[index].variable;
	}
	
	public void markIndexedVariableAsTemporal () {
		if (arrayOfMappings[index] != null) {
			arrayOfMappings[index].isTemporalVariable = true;
		}
	}
	
	public CiaoPrologVarMappingClass [] returnVariablesArray() { return arrayOfMappings; }
	
	public int getRealSize() { return realSize; }
	public CiaoPrologVarMappingClass getMappingWithIndex(int localIndex) {
		return arrayOfMappings[localIndex];
	}
}
