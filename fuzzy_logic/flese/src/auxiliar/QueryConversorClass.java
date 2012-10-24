package auxiliar;

import CiaoJava.PLAtom;
import CiaoJava.PLStructure;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;

public class QueryConversorClass {

	CiaoPrologVarsMappingClass varsMapping = null; 
	PLStructure [] subqueries = null;
	PLVariable [] finalQueryVariables = null;
	int finalQueryVariablesCounter = 0;
	int subQueryCounter = 0;
	
	public QueryConversorClass(int queryLinesCounter) {
		varsMapping = new CiaoPrologVarsMappingClass(fuzzyVarsCounter);
		subqueries = new PLStructure [queryLinesCounter+1];
		subQueryCounter = 0;
		for (int i=0; i<=queryLinesCounter; i++) {
			subqueries[i] = null; // Initialization.
		}
	}
	
	public void addSubQuery (String quantifier1, String quantifier2, String fuzzyPredicate, String [] [] arguments) 
			throws QueryConversorExceptionClass {
		
		if ((fuzzyPredicate == null) || ("".equals(fuzzyPredicate)) || ("none".equals(fuzzyPredicate))) {
			// We understood the user has decided to not use this line.
			return;
		}
		else {
			// First the variables.
			PLTerm [] plArguments = new PLTerm [arguments.length];
			for (int i=0; i<arguments.length; i++) {
				if (arguments[i][0] == "constant") {
					plArguments[i] = new PLAtom(arguments[i][1]);
				}
				else {
					if (arguments[i][0] == "variable") {
						plArguments[i] = varsMapping.returnMapping(arguments[i][1]);
					}
					else {
						throw new QueryConversorExceptionClass("Not a variable nor a constant.");
					}

				}
			}
			// Now the main functor, the fuzzy predicate.
			PLStructure mainGoal = new PLStructure(fuzzyPredicate, plArguments);
			
			// Now the second quantifier.
			mainGoal = addQuantifierToSubQuery(quantifier2, "varQuantifier2", mainGoal);
			
			// Now the first quantifier.
			mainGoal = addQuantifierToSubQuery(quantifier1, "varQuantifier1", mainGoal);

			// Now we save it.
			subqueries[subQueryCounter] = mainGoal;
			subQueryCounter++;
		}
	}
	
	private PLStructure addQuantifierToSubQuery (String quantifier, String varName, PLStructure inputGoal) {
		if ((quantifier != null) && (! "".equals(quantifier)) && (! "none".equals(quantifier))) {
			varsMapping.markIndexedVariableAsTemporal();
			PLTerm [] plArguments = new PLTerm [2];
			plArguments[0] = inputGoal;
			plArguments[1] = varsMapping.returnMapping(varName);
			PLStructure goal = new PLStructure(quantifier, plArguments);
			return goal;
		}	
		else {
			return inputGoal;
		}
	}
	
	public PLStructure getFinalQuery () {
		PLStructure accumulator = variablesConversionSubQuery();
		
		for (int i=(subQueryCounter -1); i >=0; i--) {
			
			accumulator = getConjunctiveQuery(subqueries[i], accumulator);
		}
		
		return accumulator;
	}
	
	private PLStructure variablesConversionSubQuery () {
		PLStructure subquery = null;
		PLStructure accumulator = null;
		
		finalQueryVariables = new PLVariable[varsMapping.getRealSize()];
		for (int i=0; i<varsMapping.getRealSize(); i++) finalQueryVariables[i] = null;
		finalQueryVariablesCounter=0;
		
		for (int i=(varsMapping.getRealSize() -1); i>=0; i--) {
			CiaoPrologVarMappingClass varMapping = varsMapping.getMappingWithIndex(i);
			if (! varMapping.isTemporalVariable) {
				PLTerm [] plArguments = new PLTerm [3];
				plArguments[0] = varMapping.variable;
				plArguments[1] = varMapping.condition;
				plArguments[2] = varMapping.realVariable;
				subquery = new PLStructure("rfuzzy_var_truth_value", plArguments);
				
				finalQueryVariables[finalQueryVariablesCounter] = varMapping.realVariable;
				finalQueryVariablesCounter++;
			}
			accumulator = getConjunctiveQuery(subquery, accumulator);
			subquery = null;
		}
		return accumulator;
	}
	
	private PLStructure getConjunctiveQuery(PLStructure subQuery1, PLStructure subQuery2) {
		if (subQuery1 == null) return subQuery2;
		if (subQuery2 == null) return subQuery1;
		
		PLTerm [] plArguments = {subQuery1, subQuery2};
		PLStructure result = new PLStructure(",", plArguments);
		return result;
	}
	
	public PLVariable [] getFinalQueryVariables () { 
		PLVariable [] filteredFinalQueryVariables = new PLVariable [finalQueryVariablesCounter];
		for (int i=0; i<finalQueryVariablesCounter; i++) {
			if (finalQueryVariables[i] != null) {
				filteredFinalQueryVariables[i] = finalQueryVariables[i];
			}
		}
		return filteredFinalQueryVariables;
	}
	
}





//////