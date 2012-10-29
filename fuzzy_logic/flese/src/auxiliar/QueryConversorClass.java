package auxiliar;

import java.util.ArrayList;
import java.util.Iterator;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import CiaoJava.PLAtom;
import CiaoJava.PLStructure;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;

public class QueryConversorClass {
	
	final Log LOG = LogFactory.getLog(QueryConversorClass.class);
	
    // Parameters to be retrieved, converted and saved:
    // quantifier0, quantifier1, predicate, rfuzzyComputeOperator, rfuzzyComputeValue, aggregator;
	
	public static final int initialPredicate = 0;
	public static final int quantifier0 = 1;
	public static final int quantifier1 = 2;
	public static final int predicate = 3;
	public static final int rfuzzyComputeOperator = 4;
	public static final int rfuzzyComputeValue = 5;
	public static final int aggregator = 6;
	
	private String tmpInitialPredicate = null;
	private String tmpQuantifier0 = null;
	private String tmpQuantifier1 = null;
	private String tmpPredicate = null;
	private String tmpRfuzzyComputeOperator = null;
	private String tmpRfuzzyComputeValue = null;
	private String tmpAggregator = null;

	private CiaoPrologConnectionClass connection = null; 
	private ArrayList<SubQueryConversionClass> subQueries = null;
	private SubQueryConversionClass initialSubQuery = null;
	private PLVariable inputVariable = null;
	private String inputVariableName = null;
	private PLVariable outputVariable = null;
	private String outputVariableName = null;
	private PLVariable [] showVariables = null;
	private String [] showVariablesNames = null;
	
	private String queryComplexInfoString = null;
	private String querySimpleInfoString = null;
	
	public QueryConversorClass(int queryLinesCounter, CiaoPrologConnectionClass connection) {
		this.connection = connection;
	}
	
	public String subqueryRetrieveAndSaveSubpart(String paramName, HttpServletRequest request, int type) throws QueryConversorExceptionClass {
		boolean error=false;
		String msg = ""; 
		if ((paramName == null) || ("".equals(paramName))) {
			throw new QueryConversorExceptionClass("paramName is null or empty string.");
		}
		String retrieved = request.getParameter(paramName);
		msg += "\n  retrieved for paramName " + paramName + " value " + retrieved;
		if ((retrieved != null) && (! ("----".equals(retrieved))) && (! ("".equals(retrieved)))){
			LOG.info("type: "+type+" for paramName: "+paramName+" -> "+retrieved + " ");
			switch (type) {
			case initialPredicate: tmpInitialPredicate = retrieved;
				break;
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
		return msg;
	}
	
	public void subqueryEndTestAndSave() 
			throws QueryConversorExceptionClass, AnswerTermInJavaClassException {
		String msg = "";
		msg += ("\n  fp: " + tmpQuantifier0 + "(" + tmpQuantifier1 + "(" + tmpPredicate + "))");
		msg += ("\n  cp: " + tmpPredicate + " " + tmpRfuzzyComputeOperator + " " + tmpRfuzzyComputeValue);
		msg += ("\n  aggregator: " + tmpAggregator);
		msg += ("\n  tmpInitialPredicate: " + tmpInitialPredicate);
		LOG.info(msg);
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
			
			if ((tmpRfuzzyComputeOperator != null) && (tmpRfuzzyComputeValue != null)) {
				subqueryRfuzzyComputeOperatorEndTestAndSave();
			}
			else {
				subqueryFuzzyEndTestAndSave();
			}
		}
		else {
			if ((tmpInitialPredicate != null) && (inputVariable == null)) {
				subQueryInitialEndTestAndSave();
			}
		}
		
		// Re-initialize the values for the next subQuery.
		tmpQuantifier0 = null;
		tmpQuantifier1 = null;
		tmpPredicate = null;
		tmpRfuzzyComputeOperator = null;
		tmpRfuzzyComputeValue = null;
		
	}
	
	private void subQueryInitialEndTestAndSave() throws QueryConversorExceptionClass, AnswerTermInJavaClassException {
		if (tmpInitialPredicate == null) {
			throw new QueryConversorExceptionClass("No initial predicate for the query.");
		}
		if (inputVariable != null) {
			throw new QueryConversorExceptionClass("You cannot configure twice the initial subquery.");
		}
		AnswerTermInJavaClass [] PredInfo = connection.getPredicateInfo(tmpInitialPredicate);
		if (PredInfo == null) throw new QueryConversorExceptionClass("No possible conversion for the initial predicate.");
		if (PredInfo[1] == null) throw new QueryConversorExceptionClass("No defined arity for the initial predicate.");
		if (PredInfo[1].toString() == null) throw new QueryConversorExceptionClass("No defined arity for the initial predicate.");
		
		LOG.info("PredInfo[1].toString(): " + PredInfo[1].toString());
		
		// SubGoal1: call the typing predicate.
		int PredArity = Integer.parseInt(PredInfo[1].toString());
		PLVariable [] plArgsSubGoal1 = new PLVariable [PredArity];
		for (int i=0; i<PredArity; i++) {
			plArgsSubGoal1[i] = new PLVariable();
		}
		showVariables = plArgsSubGoal1;
		copyVariablesNamesToo(PredInfo[3]);

		PLStructure subGoal1 = new PLStructure(tmpInitialPredicate, plArgsSubGoal1);
		
		// SubGoal2: ensure the input variable always has values from the typing predicate.
		inputVariable = new PLVariable();
		inputVariableName =  tmpInitialPredicate;
		PLTerm [] plArgsSubGoal2 = new PLTerm [2];
		plArgsSubGoal2[0] = inputVariable;
		plArgsSubGoal2[1] = subGoal1;
		
		PLStructure subGoal2 = new PLStructure("=", plArgsSubGoal2);
		
		initialSubQuery = new SubQueryConversionClass();
		initialSubQuery.subQuery = new PLStructure(",", new PLTerm[]{subGoal1, subGoal2});
		initialSubQuery.SubQuerySimpleInfoString = "";
		AnswerTermInJavaClass tmpQuery = new AnswerTermInJavaClass(initialSubQuery.subQuery, null);
		initialSubQuery.SubQueryComplexInfoString = tmpQuery.toString();
		initialSubQuery.resultVariable = inputVariable;
	}
	
	private void copyVariablesNamesToo(AnswerTermInJavaClass predInfoMoreInfo) throws QueryConversorExceptionClass {
		
		if (predInfoMoreInfo == null) {
			throw new QueryConversorExceptionClass("predInfoMoreInfo is null.");
		}
		int i = 0;
		int j = 0;
		boolean found = false;
		AnswerTermInJavaClass aux1 = null;
		AnswerTermInJavaClass aux2 = null;
		
		if (predInfoMoreInfo.isList()) {
			while (i < predInfoMoreInfo.length() && (! found)) {
				aux1 =  predInfoMoreInfo.atPosition(i);
				
				if ((aux1 != null) && (aux1.isList()) && (aux1.length() > 1)) {
					if ((aux1.atPosition(0) != null) && (aux1.atPosition(0).toString() != null) && 
						("database".equals(aux1.atPosition(0).toString()))) {
						found = true;
						aux2 = aux1.atPosition(1);

						if (aux2.isList()) {
							showVariablesNames = new String [aux2.length()];
							for (j=0; j < aux2.length(); j++) {
								showVariablesNames[j] = aux2.atPosition(j).toString();
							}
						}
					}
				}
				if (! found) i++;
			}
		}
	}
	
	private void subqueryRfuzzyComputeOperatorEndTestAndSave() 
			throws QueryConversorExceptionClass, AnswerTermInJavaClassException {
		AnswerTermInJavaClass [] PredInfo = connection.getPredicateInfo(tmpPredicate);
		if (PredInfo[1].toString() == null) {
			throw new QueryConversorExceptionClass("No defined arity for the predicate " + tmpPredicate);
		}
		else {
			int PredArity = Integer.parseInt(PredInfo[1].toString());
			if (PredArity != 2) 
				throw new QueryConversorExceptionClass("Arity of predicate is not 2. Predicate " + tmpPredicate);
		}

		PLTerm tmpVar = new PLVariable();
		PLVariable resultVar = new PLVariable();

		PLStructure subGoal1 = new PLStructure(tmpPredicate, new PLTerm[]{inputVariable, tmpVar});
		
		PLAtom operator = new PLAtom(tmpRfuzzyComputeOperator);
		
		PLTerm value = new PLAtom(tmpRfuzzyComputeValue);

		if ("=~=".equals(tmpRfuzzyComputeOperator)) {
			tmpVar = new PLStructure(tmpPredicate, new PLTerm[]{tmpVar});
			value = new PLStructure(tmpPredicate, new PLTerm[]{value});
		}
		PLTerm database = new PLAtom(tmpInitialPredicate);
		PLStructure subGoal2 = new PLStructure("rfuzzy_compute", new PLTerm[]{operator, database, tmpVar, value, resultVar});

		SubQueryConversionClass subQuery = new SubQueryConversionClass();
		subQuery.subQuery = new PLStructure(",", new PLTerm[]{subGoal1, subGoal2});
		subQuery.SubQuerySimpleInfoString = " " + tmpPredicate + " " + tmpRfuzzyComputeOperator + " " + tmpRfuzzyComputeValue;
		AnswerTermInJavaClass tmpQuery = new AnswerTermInJavaClass(subQuery.subQuery, null);
		subQuery.SubQueryComplexInfoString = tmpQuery.toString();
		subQuery.resultVariable = resultVar;

		// We only initialize the list if we really need it. 
		if (subQueries == null) subQueries = new ArrayList<SubQueryConversionClass>();
		subQueries.add(subQuery);
		
	}
	
	private void subqueryFuzzyEndTestAndSave() 
			throws QueryConversorExceptionClass, AnswerTermInJavaClassException {
		AnswerTermInJavaClass [] PredInfo = connection.getPredicateInfo(tmpPredicate);
		if (PredInfo == null) 
			throw new QueryConversorExceptionClass("Predicate is not in database. Predicate: " + tmpPredicate);
		if (PredInfo[1].toString() == null) {
			throw new QueryConversorExceptionClass("No defined arity for the predicate " + tmpPredicate);
		}
		else {
			int PredArity = Integer.parseInt(PredInfo[1].toString());
			if (PredArity != 2) 
				throw new QueryConversorExceptionClass("Arity of predicate is not 2. Predicate " + tmpPredicate);
		}
		
		PLVariable tmpVar1 = new PLVariable();
		PLVariable tmpVar2 = null;
		PLVariable tmpVar3 = null;
		
		PLStructure subGoal1 = new PLStructure(tmpPredicate, new PLTerm[]{inputVariable, tmpVar1});
		PLStructure subGoal2 = null;
		PLStructure subGoal3 = null;
		
		if (tmpQuantifier1 != null) {
			tmpVar2 = new PLVariable();
			subGoal2 = new PLStructure(tmpQuantifier1, new PLTerm[]{subGoal1, tmpVar2});
		}
		else {
			subGoal2 = subGoal1;
			tmpVar2 = tmpVar1;
		}
		
		if (tmpQuantifier0 != null) {
			tmpVar3 = new PLVariable();
			subGoal3 = new PLStructure(tmpQuantifier0, new PLTerm[]{subGoal2, tmpVar3});
		}
		else {
			subGoal3 = subGoal2;
			tmpVar3 = tmpVar2;
		}
		
		SubQueryConversionClass subQuery = new SubQueryConversionClass();
		subQuery.subQuery = subGoal3;
		subQuery.SubQuerySimpleInfoString = " ";
		if (tmpQuantifier0 != null) subQuery.SubQuerySimpleInfoString += tmpQuantifier0 + "(";
		if (tmpQuantifier1 != null) subQuery.SubQuerySimpleInfoString += tmpQuantifier1 + "(";
		subQuery.SubQuerySimpleInfoString += tmpPredicate;
		if (tmpQuantifier0 != null) subQuery.SubQuerySimpleInfoString += ")";
		if (tmpQuantifier0 != null) subQuery.SubQuerySimpleInfoString += ")";
		
		AnswerTermInJavaClass tmpQuery = new AnswerTermInJavaClass(subQuery.subQuery, null);
		subQuery.SubQueryComplexInfoString = tmpQuery.toString();
		subQuery.resultVariable = tmpVar3;

		// We only initialize the list if we really need it.
		if (subQueries == null) subQueries = new ArrayList<SubQueryConversionClass>();
		subQueries.add(subQuery);
	}
	
	public PLStructure queryConvert () 
			throws QueryConversorExceptionClass, AnswerTermInJavaClassException {
		
		if (initialSubQuery == null) {
			throw new QueryConversorExceptionClass("No initial subQuery to convert.");
		}
		
		querySimpleInfoString = "";
		queryComplexInfoString = "";

		PLStructure finalQuery = null;
		PLVariable currentVar = null;
		PLVariable tmpVar = null;
		PLStructure aggrSubQuery = null;
		AnswerTermInJavaClass tmpQuery = null;
		
		if (subQueries != null) {
			Iterator<SubQueryConversionClass> subQueriesIterator = subQueries.iterator();
			while (subQueriesIterator.hasNext()) {
				SubQueryConversionClass currentSubQuery = subQueriesIterator.next();

				if (finalQuery == null) {
					finalQuery = currentSubQuery.subQuery;
					currentVar = currentSubQuery.resultVariable;
					querySimpleInfoString += currentSubQuery.SubQuerySimpleInfoString;
					queryComplexInfoString += currentSubQuery.SubQueryComplexInfoString;
				}
				else {
					if (tmpAggregator == null) {
						throw new QueryConversorExceptionClass("No aggregator to combine the truth values.");
					}
					
					finalQuery = new PLStructure(",", new PLTerm[]{finalQuery, currentSubQuery.subQuery});
					tmpVar = new PLVariable();
					aggrSubQuery = new PLStructure(tmpAggregator, new PLTerm[]{currentVar, currentSubQuery.resultVariable, tmpVar});
					currentVar = tmpVar;
					tmpVar = null;
					finalQuery = new PLStructure(",", new PLTerm[]{finalQuery, aggrSubQuery});

					querySimpleInfoString += ", " + currentSubQuery.SubQuerySimpleInfoString;
					queryComplexInfoString += ", " + currentSubQuery.SubQueryComplexInfoString;
					tmpQuery = new AnswerTermInJavaClass(currentSubQuery.subQuery, null);
					queryComplexInfoString += ", " +  tmpQuery.toString();


				}
			}
			
			// currentVar, after all subQueries, is our output variable !!!
			outputVariable = currentVar;
			
			
			if (tmpAggregator != null) {
				querySimpleInfoString = tmpAggregator + "(" + querySimpleInfoString + ")";
			}
		}
		
		if (finalQuery == null) {
			finalQuery = initialSubQuery.subQuery;
			querySimpleInfoString = initialSubQuery.SubQuerySimpleInfoString;
			queryComplexInfoString = initialSubQuery.SubQueryComplexInfoString;
		}
		else {
			finalQuery = new PLStructure(",", new PLTerm[]{initialSubQuery.subQuery, finalQuery});
			querySimpleInfoString = initialSubQuery.SubQuerySimpleInfoString + querySimpleInfoString;
			queryComplexInfoString = initialSubQuery.SubQueryComplexInfoString + queryComplexInfoString;
			
		}
		
		return finalQuery;
	}
	
	public PLVariable [] getListOfVariables() {
		PLVariable [] variables = null;
		int lengthShowVariables = 0;
		if (showVariables != null) lengthShowVariables = showVariables.length;
		if (subQueries == null) {
			variables = new PLVariable[1 + lengthShowVariables];
			variables[0] = inputVariable;
			for (int i=0; i<lengthShowVariables; i++) variables[i+1] = showVariables[i];
		}
		else {
			variables = new PLVariable[2 + lengthShowVariables];
		    variables[0] = inputVariable;
		    for (int i=0; i<lengthShowVariables; i++) variables[i+1] = showVariables[i];
		    variables[lengthShowVariables + 1] = outputVariable;
		}
		return variables;
	}
	
	public String [] getListOfNamesForVariables() {
		String [] variablesNames = null;
		int lengthShowVariablesNames = 0;
		if (showVariablesNames != null) lengthShowVariablesNames = showVariablesNames.length;
		if (subQueries == null) {
			variablesNames = new String[1 + lengthShowVariablesNames];
			variablesNames[0] = inputVariableName;
			for (int i=0; i<lengthShowVariablesNames; i++) variablesNames[i+1] = showVariablesNames[i];
		}
		else {
			variablesNames = new String[2 + lengthShowVariablesNames];
			variablesNames[0] = inputVariableName;
			for (int i=0; i<lengthShowVariablesNames; i++) variablesNames[i+1] = showVariablesNames[i];
			variablesNames[lengthShowVariablesNames + 1] = outputVariableName;
		}
		return variablesNames;
	}
		
	public String getQuerySimpleInfoString() {return querySimpleInfoString; };
	public String getQueryComplexInfoString() { return queryComplexInfoString; };
	
}





//////