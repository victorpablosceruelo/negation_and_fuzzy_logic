package conversors;

import java.util.ArrayList;
import java.util.Iterator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import auxiliar.LocalUserInfoException;
import prologConnector.AnswerTermInJavaClass;
import prologConnector.AnswerTermInJavaClassException;
import prologConnector.CiaoPrologProgramIntrospectionQuery;
import prologConnector.CiaoPrologQueryException;
import prologConnector.PlConnectionEnvelopeException;
import storeHouse.CacheStoreHouseException;
import storeHouse.RequestStoreHouse;
import storeHouse.RequestStoreHouseException;
import CiaoJava.PLAtom;
import CiaoJava.PLFloat;
import CiaoJava.PLInteger;
import CiaoJava.PLStructure;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;
import constants.KConstants;
import filesAndPaths.FileInfoException;
import filesAndPaths.PathsMgmtException;

public class ConversorToPrologQuery {

	final Log LOG = LogFactory.getLog(ConversorToPrologQuery.class);

	public class ConversionInput {
		public String initialPredicate = null;
		public String aggregator = null;

		// Parameters to be retrieved, converted and saved:
		// quantifier0, quantifier1, predicate, rfuzzyComputeOperator,
		// rfuzzyComputeValue, aggregator;
		public String quantifier0 = null;
		public String quantifier1 = null;
		public String predicate = null;
		public String rfuzzyComputeOperator = null;
		public String rfuzzyComputeValue = null;

	}

	public class PrologSubQuery {
		public ConversionInput input = new ConversionInput();

		public PLStructure subQuery = null;
		public String SubQuerySimpleInfoString = null;
		public String SubQueryComplexInfoString = null;
		public PLVariable resultVariable = null;
	}

	private ArrayList<PrologSubQuery> subQueries = null;
	private PrologSubQuery initialSubQuery = null;
	private PLVariable outputVariable = null;
	private String outputVariableName = "Truth Value";
	private PLVariable[] showVariables = null;
	private String[] showVariablesNames = null;
	private PLStructure finalQuery = null;

	private String queryComplexInfoString = null;
	private String querySimpleInfoString = null;
	private CiaoPrologProgramIntrospectionQuery ciaoPrologIntrospectionQuery = null;
	private RequestStoreHouse requestStoreHouse = null;

	public ConversorToPrologQuery(RequestStoreHouse sessionStoreHouse) throws QueryConversorException, CacheStoreHouseException,
			PathsMgmtException, CiaoPrologQueryException, PlConnectionEnvelopeException, AnswerTermInJavaClassException, FileInfoException, LocalUserInfoException, RequestStoreHouseException {

		this.requestStoreHouse = sessionStoreHouse;
		ciaoPrologIntrospectionQuery = CiaoPrologProgramIntrospectionQuery.getInstance(sessionStoreHouse.getProgramFileInfo());

		String queryLinesCounterString = sessionStoreHouse.getRequestParameter(KConstants.QueryParams.queryLinesCounter);

		int queryLinesCounter = Integer.parseInt(queryLinesCounterString);

		String msg = "";

		// Parameters to be retrieved and saved:
		// quantifier0, quantifier1, predicate, rfuzzyComputeOperator,
		// rfuzzyComputeValue, aggregator;

		PrologSubQuery prologSubQuery = new PrologSubQuery();
		ConversionInput input = prologSubQuery.input;
		input.initialPredicate = sessionStoreHouse.getRequestParameter("selectQueryStartupType");
		input.aggregator = sessionStoreHouse.getRequestParameter("queryLines.selectAggregator");
		testConversionInput(prologSubQuery);

		for (int i = 0; i < queryLinesCounter; i++) {
			prologSubQuery = new PrologSubQuery();
			input = prologSubQuery.input;

			input.initialPredicate = sessionStoreHouse.getRequestParameter("selectQueryStartupType");
			input.aggregator = sessionStoreHouse.getRequestParameter("queryLines.selectAggregator");
			input.quantifier0 = sessionStoreHouse.getRequestParameter("queryLine[" + i + "].selectQuantifier_0");
			input.quantifier1 = sessionStoreHouse.getRequestParameter("queryLine[" + i + "].selectQuantifier_1");
			input.predicate = sessionStoreHouse.getRequestParameter("queryLine[" + i + "].selectPredicate");
			input.rfuzzyComputeOperator = sessionStoreHouse.getRequestParameter("queryLine[" + i + "].selectRfuzzyComputeOperator");
			input.rfuzzyComputeValue = sessionStoreHouse.getRequestParameter("queryLine[" + i + "].selectRfuzzyComputeValue");

			testConversionInput(prologSubQuery);
			// We only initialize the list if we really need it.
			if (subQueries == null)
				subQueries = new ArrayList<PrologSubQuery>();
			subQueries.add(prologSubQuery);
		}
		LOG.info(msg);
		queryConvert();
	}

	public void testConversionInput(PrologSubQuery prologSubQuery) throws QueryConversorException, AnswerTermInJavaClassException, RequestStoreHouseException {
		ConversionInput input = prologSubQuery.input;
		String msg = "";
		msg += ("\n  fp: " + input.quantifier0 + "(" + input.quantifier1 + "(" + input.predicate + "))");
		msg += ("\n  cp: " + input.predicate + " " + input.rfuzzyComputeOperator + " " + input.rfuzzyComputeValue);
		msg += ("\n  aggregator: " + input.aggregator);
		msg += ("\n  tmpInitialPredicate: " + input.initialPredicate);
		LOG.info(msg);
		if ((input.quantifier0 != null) || (input.quantifier1 != null) || (input.predicate != null)
				|| (input.rfuzzyComputeOperator != null) || (input.rfuzzyComputeValue != null)) {
			if (input.predicate == null) {
				throw new QueryConversorException("Cannot build a query without a predicate.");
			}
			if (((input.rfuzzyComputeOperator != null) || (input.rfuzzyComputeValue != null))
					&& ((input.quantifier0 != null) || (input.quantifier1 != null))) {
				throw new QueryConversorException("Cannot build a so complex query.");
			}

			if ((input.rfuzzyComputeOperator != null) && (input.rfuzzyComputeValue != null)) {
				subqueryRfuzzyComputeOperatorEndTestAndSave(prologSubQuery);
			} else {
				AnswerTermInJavaClass[] predInfo = ciaoPrologIntrospectionQuery.getPredicateInfo(input.predicate);
				if (hasTruthValueTypeAsReturnType(predInfo[2], input)) {
					subqueryFuzzyEndTestAndSave(prologSubQuery);
				} else {
					throw new QueryConversorException("You need to fill all the fields for non-fuzzy queries.");
				}
			}
		} else {
			if ((input.initialPredicate != null) && (showVariables == null)) {
				subQueryInitialEndTestAndSave(prologSubQuery);
			}
		}
	}

	private boolean hasTruthValueTypeAsReturnType(AnswerTermInJavaClass predInfoType, ConversionInput input) throws QueryConversorException {
		boolean found = false;
		String msg = "";
		if (predInfoType == null)
			throw new QueryConversorException("predInfoType is null.");
		msg += "tmpInitialPredicate: " + input.initialPredicate;
		msg += "\n predInfoType: " + predInfoType.toString();
		if (!predInfoType.isList())
			throw new QueryConversorException("predInfoType is not a list.");

		int predInfoTypeLength = predInfoType.length();
		int predInfoTypeIndex = 0;
		while ((predInfoTypeIndex < predInfoTypeLength) && (!found)) {
			AnswerTermInJavaClass type = predInfoType.atPosition(predInfoTypeIndex);
			if (type == null)
				throw new QueryConversorException("predInfoType type is null.");
			LOG.info("type: " + type.toString());
			if (!type.isList())
				throw new QueryConversorException("predInfoType type is not a list. type: " + type.toString());
			if ((type.length() == 2) && (type.atPosition(0) != null) && (type.atPosition(1) != null)
					&& (input.initialPredicate.equals(type.atPosition(0).toString()))
					&& ("rfuzzy_truth_value_type".equals(type.atPosition(1).toString()))) {
				found = true;
				msg += "\n valid type: " + type.toString();
			} else
				msg += "\n NOT valid type: " + type.toString();
			if (!found)
				predInfoTypeIndex++;
		}
		LOG.info(msg);
		return found;
	}

	private void subQueryInitialEndTestAndSave(PrologSubQuery prologSubQuery) throws QueryConversorException,
			AnswerTermInJavaClassException, RequestStoreHouseException {
		ConversionInput input = prologSubQuery.input;
		if (input.initialPredicate == null) {
			throw new QueryConversorException("No initial predicate for the query.");
		}
		if (showVariables != null) {
			throw new QueryConversorException("You cannot configure twice the initial subquery.");
		}
		AnswerTermInJavaClass[] PredInfo = ciaoPrologIntrospectionQuery.getPredicateInfo(input.initialPredicate);
		if (PredInfo == null)
			throw new QueryConversorException("No possible conversion for the initial predicate.");
		if (PredInfo[1] == null)
			throw new QueryConversorException("No defined arity for the initial predicate.");
		if (PredInfo[1].toString() == null)
			throw new QueryConversorException("No defined arity for the initial predicate.");

		LOG.info("PredInfo[1].toString(): " + PredInfo[1].toString());

		// SubGoal1: call the typing predicate.
		int PredArity = Integer.parseInt(PredInfo[1].toString());
		PLVariable[] plArgsSubGoal1 = new PLVariable[PredArity];
		for (int i = 0; i < PredArity; i++) {
			plArgsSubGoal1[i] = new PLVariable();
		}

		showVariables = new PLVariable[plArgsSubGoal1.length + 1];
		showVariables[0] = new PLVariable();
		for (int i = 0; i < plArgsSubGoal1.length; i++) {
			showVariables[i + 1] = plArgsSubGoal1[i];
		}

		copyVariablesNamesToo(input.initialPredicate, PredInfo[3]);

		PLStructure subGoal1 = new PLStructure(input.initialPredicate, plArgsSubGoal1);

		// SubGoal2: ensure the input variable always has values from the typing
		// predicate.
		PLTerm[] plArgsSubGoal2 = new PLTerm[2];
		plArgsSubGoal2[0] = showVariables[0];
		plArgsSubGoal2[1] = subGoal1;

		PLStructure subGoal2 = new PLStructure("=", plArgsSubGoal2);

		String localUserName = requestStoreHouse.getSession().getLocalUserInfo().getLocalUserName();
		PLStructure subGoal3 = new PLStructure("assertLocalUserName", new PLTerm[] { new PLAtom("'" + localUserName + "'") });

		initialSubQuery = new PrologSubQuery();
		initialSubQuery.subQuery = new PLStructure(",", new PLTerm[] { subGoal1, subGoal2 });
		initialSubQuery.subQuery = new PLStructure(",", new PLTerm[] { subGoal3, initialSubQuery.subQuery });
		initialSubQuery.SubQuerySimpleInfoString = "";
		AnswerTermInJavaClass tmpQuery = new AnswerTermInJavaClass(initialSubQuery.subQuery, null);
		initialSubQuery.SubQueryComplexInfoString = tmpQuery.toString();
		initialSubQuery.resultVariable = showVariables[0];
	}

	private void copyVariablesNamesToo(String firstVarName, AnswerTermInJavaClass predInfoMoreInfo) throws QueryConversorException {
		if (predInfoMoreInfo == null) {
			throw new QueryConversorException("predInfoMoreInfo is null.");
		}
		LOG.info("copyVariablesNamesToo: firstVarName: " + firstVarName + " predInfoMoreInfo: " + predInfoMoreInfo);
		// latestEvaluatedQueryAnswersIteratorLOG.info("copyVariablesNamesToo: firstVarName: "
		// + firstVarName + " predInfoMoreInfo: "+ predInfoMoreInfo.toString());

		int i = 0;
		int j = 0;
		boolean found = false;
		AnswerTermInJavaClass aux1 = null;
		AnswerTermInJavaClass aux2 = null;

		if (predInfoMoreInfo.isList()) {
			while (i < predInfoMoreInfo.length() && (!found)) {
				aux1 = predInfoMoreInfo.atPosition(i);

				if (aux1 == null)
					LOG.info("ERROR: aux1 is NULL.");
				else {
					if (aux1.isArray()) {
						if ((aux1.atPosition(0) != null) && (aux1.atPosition(0).toString() != null)
								&& ("database".equals(aux1.atPosition(0).toString()))) {
							LOG.info("Found database information: " + aux1.toString());
							found = true;
							aux2 = aux1.atPosition(1);
						} else
							LOG.info("ERROR: not valid value for aux1.atPosition(0)");
					} else
						LOG.info("ERROR: aux1 is NOT an array.");
				}

				if (!found)
					i++;
			}
		} else
			LOG.info("ERROR: predInfoMoreInfo is NOT a list.");

		showVariablesNames = null;
		if (aux2 != null) {
			if (aux2.isList()) {
				showVariablesNames = new String[aux2.length() + 1];
				showVariablesNames[0] = firstVarName;
				for (j = 0; j <= aux2.length(); j++) {
					if ((aux2 != null) && (aux2.atPosition(j) != null)) {
						showVariablesNames[j + 1] = aux2.atPosition(j).toString();
					}
				}
			} else
				LOG.info("ERROR: aux2 is not a list. aux2: " + aux2.toString());
		} else
			LOG.info("ERROR: aux2 is null.");
	}

	private void subqueryRfuzzyComputeOperatorEndTestAndSave(PrologSubQuery prologSubQuery) throws QueryConversorException,
			AnswerTermInJavaClassException {
		ConversionInput input = prologSubQuery.input;
		AnswerTermInJavaClass[] PredInfo = ciaoPrologIntrospectionQuery.getPredicateInfo(input.predicate);
		if ((PredInfo == null) || (PredInfo[1] == null) || (PredInfo[1].toString() == null)) {
			throw new QueryConversorException("No defined arity for the predicate " + input.predicate);
		} else {
			int PredArity = Integer.parseInt(PredInfo[1].toString());
			if (PredArity != 2)
				throw new QueryConversorException("Arity of predicate is not 2. Predicate " + input.predicate);
		}

		PLTerm database = new PLAtom(input.initialPredicate);
		PLVariable resultVar = new PLVariable();
		PLStructure origin = new PLStructure(input.predicate, new PLTerm[] { showVariables[0] });
		PLAtom operator = new PLAtom(input.rfuzzyComputeOperator);

		boolean isInteger = true;
		int valueInt = 0;
		try {
			valueInt = Integer.parseInt(input.rfuzzyComputeValue);
		} catch (Exception e) {
			isInteger = false;
		}

		boolean isDouble = true;
		double valueDouble = 0;
		try {
			valueDouble = Double.parseDouble(input.rfuzzyComputeValue);
		} catch (Exception e) {
			isDouble = false;
		}

		PLTerm value = null;
		if ((!isInteger) && (!isDouble)) {
			value = new PLAtom(input.rfuzzyComputeValue);
		} else {
			if (isInteger) {
				value = new PLInteger(valueInt);
			} else {
				value = new PLFloat(valueDouble);
			}
		}

		/*
		 * if ("=~=".equals(tmpRfuzzyComputeOperator)) { value = new
		 * PLStructure(tmpPredicate, new PLTerm[]{value}); }
		 */

		prologSubQuery.subQuery = new PLStructure("rfuzzy_compute", new PLTerm[] { operator, origin, value, database, resultVar });
		prologSubQuery.SubQuerySimpleInfoString = " " + input.predicate + "(" + input.initialPredicate + ")" + " "
				+ input.rfuzzyComputeOperator + " " + input.rfuzzyComputeValue;
		AnswerTermInJavaClass tmpQuery = new AnswerTermInJavaClass(prologSubQuery.subQuery, null);
		prologSubQuery.SubQueryComplexInfoString = tmpQuery.toString();
		prologSubQuery.resultVariable = resultVar;

	}

	private void subqueryFuzzyEndTestAndSave(PrologSubQuery prologSubQuery) throws QueryConversorException, AnswerTermInJavaClassException {
		ConversionInput input = prologSubQuery.input;
		AnswerTermInJavaClass[] PredInfo = ciaoPrologIntrospectionQuery.getPredicateInfo(input.predicate);
		if (PredInfo == null)
			throw new QueryConversorException("Predicate is not in database. Predicate: " + input.predicate);
		if (PredInfo[1].toString() == null) {
			throw new QueryConversorException("No defined arity for the predicate " + input.predicate);
		} else {
			int PredArity = Integer.parseInt(PredInfo[1].toString());
			if (PredArity != 2)
				throw new QueryConversorException("Arity of predicate is not 2. Predicate " + input.predicate);
		}

		PLVariable tmpVar1 = new PLVariable();
		PLVariable tmpVar2 = null;
		PLVariable tmpVar3 = null;

		PLStructure subGoal1 = new PLStructure(input.predicate, new PLTerm[] { showVariables[0], tmpVar1 });
		PLStructure subGoal2 = null;
		PLStructure subGoal3 = null;

		if (input.quantifier1 != null) {
			tmpVar2 = new PLVariable();
			subGoal2 = new PLStructure(input.quantifier1, new PLTerm[] { subGoal1, tmpVar2 });
		} else {
			subGoal2 = subGoal1;
			tmpVar2 = tmpVar1;
		}

		if (input.quantifier0 != null) {
			tmpVar3 = new PLVariable();
			subGoal3 = new PLStructure(input.quantifier0, new PLTerm[] { subGoal2, tmpVar3 });
		} else {
			subGoal3 = subGoal2;
			tmpVar3 = tmpVar2;
		}

		prologSubQuery.subQuery = subGoal3;
		prologSubQuery.SubQuerySimpleInfoString = " ";
		if (input.quantifier0 != null)
			prologSubQuery.SubQuerySimpleInfoString += input.quantifier0 + "(";
		if (input.quantifier1 != null)
			prologSubQuery.SubQuerySimpleInfoString += input.quantifier1 + "(";
		prologSubQuery.SubQuerySimpleInfoString += input.predicate + "(" + input.initialPredicate + ")";

		if (input.quantifier0 != null)
			prologSubQuery.SubQuerySimpleInfoString += ")";
		if (input.quantifier0 != null)
			prologSubQuery.SubQuerySimpleInfoString += ")";

		AnswerTermInJavaClass tmpQuery = new AnswerTermInJavaClass(prologSubQuery.subQuery, null);
		prologSubQuery.SubQueryComplexInfoString = tmpQuery.toString();
		prologSubQuery.resultVariable = tmpVar3;

	}

	private void queryConvert() throws QueryConversorException, AnswerTermInJavaClassException {

		if (initialSubQuery == null) {
			throw new QueryConversorException("No initial subQuery to convert.");
		}

		querySimpleInfoString = "";
		queryComplexInfoString = "";

		PLVariable currentVar = null;
		PLVariable tmpVar = null;
		PLStructure aggrSubQuery = null;
		AnswerTermInJavaClass tmpQuery = null;

		Iterator<PrologSubQuery> subQueriesIterator = null;
		PrologSubQuery currentSubQuery = null;

		if (subQueries != null) {
			subQueriesIterator = subQueries.iterator();
			while (subQueriesIterator.hasNext()) {
				currentSubQuery = subQueriesIterator.next();

				if (finalQuery == null) {
					finalQuery = currentSubQuery.subQuery;
					currentVar = currentSubQuery.resultVariable;
					querySimpleInfoString += currentSubQuery.SubQuerySimpleInfoString;
					queryComplexInfoString += currentSubQuery.SubQueryComplexInfoString;
				} else {
					if (currentSubQuery.input.aggregator == null) {
						throw new QueryConversorException("No aggregator to combine the truth values.");
					}

					finalQuery = new PLStructure(",", new PLTerm[] { finalQuery, currentSubQuery.subQuery });
					tmpVar = new PLVariable();
					aggrSubQuery = new PLStructure(currentSubQuery.input.aggregator, new PLTerm[] { currentVar,
							currentSubQuery.resultVariable, tmpVar });
					currentVar = tmpVar;
					tmpVar = null;
					finalQuery = new PLStructure(",", new PLTerm[] { finalQuery, aggrSubQuery });

					querySimpleInfoString += ", " + currentSubQuery.SubQuerySimpleInfoString;
					queryComplexInfoString += ", " + currentSubQuery.SubQueryComplexInfoString;
					tmpQuery = new AnswerTermInJavaClass(currentSubQuery.subQuery, null);
					queryComplexInfoString += ", " + tmpQuery.toString();

				}
			}

			// currentVar, after all subQueries, is our output variable !!!
			outputVariable = currentVar;

			if ((currentSubQuery != null) && (currentSubQuery.input.aggregator != null)) {
				querySimpleInfoString = currentSubQuery.input.aggregator + "(" + querySimpleInfoString + ")";
			}
		}

		if (finalQuery == null) {
			outputVariable = new PLVariable();
			PLStructure hackTruthValueQuery = new PLStructure("=", new PLTerm[] { outputVariable, new PLAtom("1") });
			finalQuery = new PLStructure(",", new PLTerm[] { initialSubQuery.subQuery, hackTruthValueQuery });
			querySimpleInfoString = initialSubQuery.SubQuerySimpleInfoString;
			queryComplexInfoString = initialSubQuery.SubQueryComplexInfoString;
		} else {
			finalQuery = new PLStructure(",", new PLTerm[] { initialSubQuery.subQuery, finalQuery });
			querySimpleInfoString = initialSubQuery.SubQuerySimpleInfoString + querySimpleInfoString;
			queryComplexInfoString = initialSubQuery.SubQueryComplexInfoString + queryComplexInfoString;

		}

		// rfuzzy_var_truth_value(Var, Condition, Value)
		finalQuery = fixVariables(finalQuery);

		LOG.debug("\n -- Query Conversor ended -- \n");
	}

	private PLStructure fixVariables(PLStructure finalQueryIn) {
		PLVariable tmpVar = null;
		PLVariable auxVar = null;
		PLStructure conversion = null;
		for (int i = 0; i < showVariables.length; i++) {
			tmpVar = new PLVariable();
			auxVar = new PLVariable();
			// rfuzzy_var_truth_value(Var, Condition, Value)
			conversion = new PLStructure("rfuzzy_var_truth_value", new PLTerm[] { showVariables[i], auxVar, tmpVar });
			finalQueryIn = new PLStructure(",", new PLTerm[] { finalQueryIn, conversion });
			showVariables[i] = tmpVar;
		}
		if (outputVariable != null) {
			tmpVar = new PLVariable();
			auxVar = new PLVariable();
			conversion = new PLStructure("rfuzzy_var_truth_value", new PLTerm[] { outputVariable, auxVar, tmpVar });
			finalQueryIn = new PLStructure(",", new PLTerm[] { finalQueryIn, conversion });
			outputVariable = tmpVar;
		}
		return finalQueryIn;
	}

	public PLVariable[] getListOfVariables() {
		int length = showVariables.length;
		if (outputVariable != null)
			length++;
		PLVariable[] listOfVariables = new PLVariable[length];
		for (int i = 0; i < showVariables.length; i++)
			listOfVariables[i] = showVariables[i];
		if (outputVariable != null)
			listOfVariables[length - 1] = outputVariable;
		return listOfVariables;
	}

	public String[] getListOfNamesForVariables() {
		if (showVariablesNames == null)
			return null;
		int length = showVariablesNames.length;
		if (outputVariable != null)
			length++;
		String[] listOfNamesForVariables = new String[length];
		for (int i = 0; i < showVariablesNames.length; i++)
			listOfNamesForVariables[i] = showVariablesNames[i];
		if (outputVariable != null)
			listOfNamesForVariables[length - 1] = outputVariableName;
		return listOfNamesForVariables;
	}

	public String getQuerySimpleInfoString() {
		return querySimpleInfoString;
	};

	public String getQueryComplexInfoString() {
		return queryComplexInfoString;
	};
	
	public PLStructure getConvertedQuery() {
		return finalQuery;
	};


}

// ////