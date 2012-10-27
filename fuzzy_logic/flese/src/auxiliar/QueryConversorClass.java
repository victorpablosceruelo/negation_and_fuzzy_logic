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
	
	private String queryComplexInfoString = null;
	private String querySimpleInfoString = null;
	
	public QueryConversorClass(int queryLinesCounter, CiaoPrologConnectionClass connection) {
		this.connection = connection;
		subQueries = new ArrayList<SubQueryConversionClass>();
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
	
	public void subqueryEndTestAndSave() throws QueryConversorExceptionClass {
		String msg = "";
		msg += ("\n  fp: " + tmpQuantifier0 + "(" + tmpQuantifier1 + "(" + tmpPredicate + "))");
		msg += ("\n  cp: " + tmpPredicate + " " + tmpRfuzzyComputeOperator + " " + tmpRfuzzyComputeValue);
		msg += ("\n  aggregator: " + tmpAggregator);
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
	
	private void subQueryInitialEndTestAndSave() throws QueryConversorExceptionClass {
		if (tmpInitialPredicate == null) {
			throw new QueryConversorExceptionClass("No initial predicate for the query.");
		}
		if (inputVariable != null) {
			throw new QueryConversorExceptionClass("You cannot configure twice the initial subquery.");
		}
		AnswerTermInJava [] PredInfo = connection.getPredicateInfo(tmpInitialPredicate);
		if (PredInfo[1].toString() == null) {
			throw new QueryConversorExceptionClass("No defined arity for the initial predicate.");
		}

		// SubGoal1: call the typing predicate.
		int PredArity = Integer.getInteger(PredInfo[1].toString());
		PLTerm [] plArgsSubGoal1 = new PLTerm [PredArity];
		for (int i=0; i<PredArity; i++) {
			plArgsSubGoal1[i] = new PLVariable();
		}
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
		AnswerTermInJava tmpQuery = new AnswerTermInJava(initialSubQuery.subQuery, null);
		initialSubQuery.SubQueryComplexInfoString = tmpQuery.toString();
		initialSubQuery.resultVariable = inputVariable;
	}
	
	private void subqueryRfuzzyComputeOperatorEndTestAndSave() throws QueryConversorExceptionClass {
		AnswerTermInJava [] PredInfo = connection.getPredicateInfo(tmpPredicate);
		if (PredInfo[1].toString() == null) {
			throw new QueryConversorExceptionClass("No defined arity for the predicate " + tmpPredicate);
		}
		else {
			int PredArity = Integer.getInteger(PredInfo[1].toString());
			if (PredArity != 2) 
				throw new QueryConversorExceptionClass("Arity of predicate is not 2. Predicate " + tmpPredicate);
		}
		
		PLVariable tmpVar = new PLVariable();
		PLVariable resultVar = new PLVariable();
		
		PLStructure subGoal1 = new PLStructure(tmpPredicate, new PLTerm[]{inputVariable, tmpVar});
		PLAtom operator = new PLAtom(tmpRfuzzyComputeOperator);
		PLAtom value = new PLAtom(tmpRfuzzyComputeValue);
		PLStructure subGoal2 = new PLStructure("rfuzzy_compute", new PLTerm[]{operator, tmpVar, value, resultVar});
		
		SubQueryConversionClass subQuery = new SubQueryConversionClass();
		subQuery.subQuery = new PLStructure(",", new PLTerm[]{subGoal1, subGoal2});
		subQuery.SubQuerySimpleInfoString = " " + tmpPredicate + " " + tmpRfuzzyComputeOperator + " " + tmpRfuzzyComputeValue;
		AnswerTermInJava tmpQuery = new AnswerTermInJava(subQuery.subQuery, null);
		subQuery.SubQueryComplexInfoString = tmpQuery.toString();
		subQuery.resultVariable = resultVar;
		subQueries.add(subQuery);
		
	}
	
	private void subqueryFuzzyEndTestAndSave() throws QueryConversorExceptionClass {
		AnswerTermInJava [] PredInfo = connection.getPredicateInfo(tmpPredicate);
		if (PredInfo == null) 
			throw new QueryConversorExceptionClass("Predicate is not in database. Predicate: " + tmpPredicate);
		if (PredInfo[1].toString() == null) {
			throw new QueryConversorExceptionClass("No defined arity for the predicate " + tmpPredicate);
		}
		else {
			int PredArity = Integer.getInteger(PredInfo[1].toString());
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
		
		AnswerTermInJava tmpQuery = new AnswerTermInJava(subQuery.subQuery, null);
		subQuery.SubQueryComplexInfoString = tmpQuery.toString();
		subQuery.resultVariable = tmpVar3;
		subQueries.add(subQuery);
	}
	
	public PLStructure queryConvert () throws QueryConversorExceptionClass {
		
		if (initialSubQuery == null) {
			throw new QueryConversorExceptionClass("No initial subQuery to convert.");
		}
		
		querySimpleInfoString = "";
		queryComplexInfoString = "";

		PLStructure finalQuery = null;
		PLVariable currentVar = null;
		PLVariable tmpVar = null;
		PLStructure aggrSubQuery = null;
		AnswerTermInJava tmpQuery = null;
		
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
					finalQuery = new PLStructure(",", new PLTerm[]{finalQuery, aggrSubQuery});

					querySimpleInfoString += ", " + currentSubQuery.SubQuerySimpleInfoString;
					queryComplexInfoString += ", " + currentSubQuery.SubQueryComplexInfoString;
					tmpQuery = new AnswerTermInJava(currentSubQuery.subQuery, null);
					queryComplexInfoString += ", " +  tmpQuery.toString();


				}
			}
			
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
	
	
	public PLVariable getInputVariable() { return inputVariable; };
	public String getInputVariableName() { return inputVariableName; };
	public PLVariable getOutputVariable() { return outputVariable; };
	public String getOutputVariableName() { return outputVariableName; };
	
	public String getQuerySimpleInfoString() {return querySimpleInfoString; };
	public String getQueryComplexInfoString() { return queryComplexInfoString; };

}





//////