package auxiliar;

import java.util.ArrayList;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import servlets.FilesMgmtServlet;

import CiaoJava.PLAtom;
import CiaoJava.PLStructure;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;

public class QueryConversorClass {
	
	final Log LOG = LogFactory.getLog(QueryConversorClass.class);
	
    // Parameters to be retrieved, converted and saved:
    // quantifier0, quantifier1, predicate, rfuzzyComputeOperator, rfuzzyComputeValue, aggregator;
	
	public static final int quantifier0 = 0;
	public static final int quantifier1 = 1;
	public static final int predicate = 2;
	public static final int rfuzzyComputeOperator = 3;
	public static final int rfuzzyComputeValue = 4;
	public static final int aggregator = 5;
	
	private String tmpQuantifier0 = null;
	private String tmpQuantifier1 = null;
	private String tmpPredicate = null;
	private String tmpRfuzzyComputeOperator = null;
	private String tmpRfuzzyComputeValue = null;
	private String tmpAggregator = null;

	private CiaoPrologConnectionClass connection = null;
	private ArrayList<CiaoPrologVarMappingClass> varsMappings = null; 
	PLStructure [] subqueries = null;
	PLVariable [] finalQueryVariables = null;
	int finalQueryVariablesCounter = 0;
	int subQueriesCounter = 0;
	
	public QueryConversorClass(int queryLinesCounter, CiaoPrologConnectionClass connection) {
		varsMappings = new ArrayList<CiaoPrologVarMappingClass>();
		this.connection = connection;
		
		subqueries = new PLStructure [queryLinesCounter+1];
		subQueriesCounter = 0;
		for (int i=0; i<=queryLinesCounter; i++) {
			subqueries[i] = null; // Initialization.
		}
	}
	
	public void retrieveAndSave(String paramName, HttpServletRequest request, int type) throws QueryConversorExceptionClass {
		boolean error=false;
		if ((paramName == null) || ("".equals(paramName))) {
			throw new QueryConversorExceptionClass("paramName is null or empty string.");
		}
		String retrieved = request.getParameter(paramName);
		if ((retrieved != null) && (retrieved != "----")){
			LOG.info("type: "+type+" for paramName: "+paramName+" -> "+retrieved + " ");
			switch (type) {
			case quantifier0: tmpQuantifier0 = retrieved;
				break; 
			case quantifier1: tmpQuantifier1 = retrieved;
				break;
			case predicate: tmpPredicate = retrieved;
				break;
			case rfuzzyComputeOperator: tmpRfuzzyComputeOperator = retrieved;
				break;
			case rfuzzyComputeValue: tmpRfuzzyComputeValue= retrieved;
				break;
			case aggregator: tmpAggregator= retrieved;
				break;
			default: error=true;
				break;
			}
			if (error) throw new QueryConversorExceptionClass("Unknown type.");
		}
	}
	
	private  getPredicateInfo (String predicateName) {
		connection.getProgramIntrospectionIterator()
	}
	
	public void newSubquery() throws QueryConversorExceptionClass {
		if ((tmpQuantifier0 != null) || (tmpQuantifier1 != null) ||
			(tmpPredicate != null) || 
			(tmpRfuzzyComputeOperator != null) || (tmpRfuzzyComputeValue != null)) {
			if (tmpPredicate == null) {
				throw new QueryConversorExceptionClass("Cannot build a query without a predicate.");
			}
			if (((tmpRfuzzyComputeOperator != null) || (tmpRfuzzyComputeValue != null)) &&
				((tmpQuantifier0 != null) || (tmpQuantifier1 != null))) {
				throw new QueryConversorExceptionClass("Cannot build a so complex query.");
			}
			
			
			
		}
		
		// Re-initialize the values for the next subQuery.
		tmpQuantifier0 = null;
		tmpQuantifier1 = null;
		tmpPredicate = null;
		tmpRfuzzyComputeOperator = null;
		tmpRfuzzyComputeValue = null;
		
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