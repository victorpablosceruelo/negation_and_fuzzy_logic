package auxiliar;

import CiaoJava.PLStructure;

public class QueryConversorClass {

	CiaoPrologVarsMappingClass varsMapping = null; 
	PLStructure [] subqueries = null;
	int subQueryCounter = 0;
	
	public QueryConversorClass(int queryLinesCounter, int fuzzyVarsCounter) {
		varsMapping = new CiaoPrologVarsMappingClass(fuzzyVarsCounter);
		subqueries = new PLStructure [queryLinesCounter+1];
		subQueryCounter = 0;
	}
	
	public void addSubQuery (String quantifier1, String quantifier2, String fuzzyPredicate, String [] [] arguments) {
		
	}
	
	public PLStructure returnQuery () {
		return null;
	}
	public CiaoPrologVarsMappingClass getVarsMapping () { return varsMapping; }
	
}
