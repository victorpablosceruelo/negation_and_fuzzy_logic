package conversors;

import java.util.ArrayList;
import java.util.Iterator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import prologConnector.CiaoPrologConnectorException;
import prologConnector.CiaoPrologProgramIntrospectionQuery;
import prologConnector.CiaoPrologTermInJava;
import prologConnector.PlConnectionEnvelopeException;
import prologConnector.PredicateInfo;
import prologConnector.moreInfo.PredMoreInfoInterface;
import storeHouse.CacheStoreHouseException;
import storeHouse.RequestStoreHouse;
import storeHouse.RequestStoreHouseException;
import CiaoJava.PLAtom;
import CiaoJava.PLFloat;
import CiaoJava.PLInteger;
import CiaoJava.PLStructure;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;
import auxiliar.LocalUserInfoException;
import constants.KConstants;
import filesAndPaths.FilesAndPathsException;

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
			FilesAndPathsException, CiaoPrologConnectorException, PlConnectionEnvelopeException, FilesAndPathsException,
			LocalUserInfoException, RequestStoreHouseException {

		this.requestStoreHouse = sessionStoreHouse;
		ciaoPrologIntrospectionQuery = CiaoPrologProgramIntrospectionQuery.getInstance(sessionStoreHouse.getProgramFileInfo());

		String queryLinesCounterString = sessionStoreHouse.getRequestParameter(KConstants.Request.linesCounterParam);

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

	public void testConversionInput(PrologSubQuery prologSubQuery) throws QueryConversorException, RequestStoreHouseException,
			CiaoPrologConnectorException {
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
				PredicateInfo predicateInfo = ciaoPrologIntrospectionQuery.getProgramIntrospection().getPredicateInfo(input.predicate);
				String[] type = { input.initialPredicate, KConstants.PrologTypes.rfuzzy_truth_value_type };
				if (predicateInfo.hasType(type, false)) {
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

	private void subQueryInitialEndTestAndSave(PrologSubQuery prologSubQuery) throws QueryConversorException, RequestStoreHouseException,
			CiaoPrologConnectorException {
		ConversionInput input = prologSubQuery.input;
		if (input.initialPredicate == null) {
			throw new QueryConversorException("No initial predicate for the query.");
		}
		if (showVariables != null) {
			throw new QueryConversorException("You cannot configure twice the initial subquery.");
		}

		PredicateInfo predicateInfo = ciaoPrologIntrospectionQuery.getProgramIntrospection().getPredicateInfo(input.initialPredicate);
		// SubGoal1: call the typing predicate.
		int predArityInt = predicateInfo.getPredicateArity();
		PLVariable[] plArgsSubGoal1 = new PLVariable[predArityInt];
		for (int i = 0; i < predArityInt; i++) {
			plArgsSubGoal1[i] = new PLVariable();
		}

		showVariables = new PLVariable[plArgsSubGoal1.length + 1];
		showVariables[0] = new PLVariable();
		for (int i = 0; i < plArgsSubGoal1.length; i++) {
			showVariables[i + 1] = plArgsSubGoal1[i];
		}

		PredMoreInfoInterface predMoreInfo = predicateInfo.getPredicateMoreInfoAs("database");
		showVariablesNames = predMoreInfo.generateVariablesNames(input.initialPredicate);

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
		CiaoPrologTermInJava tmpQuery = new CiaoPrologTermInJava(initialSubQuery.subQuery, null);
		initialSubQuery.SubQueryComplexInfoString = tmpQuery.toString();
		initialSubQuery.resultVariable = showVariables[0];
	}

	private void subqueryRfuzzyComputeOperatorEndTestAndSave(PrologSubQuery prologSubQuery) throws QueryConversorException,
			CiaoPrologConnectorException {
		ConversionInput input = prologSubQuery.input;
		PredicateInfo predicateInfo = ciaoPrologIntrospectionQuery.getProgramIntrospection().getPredicateInfo(input.initialPredicate);
		if (predicateInfo.getPredicateArity() != 2) {
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
		CiaoPrologTermInJava tmpQuery = new CiaoPrologTermInJava(prologSubQuery.subQuery, null);
		prologSubQuery.SubQueryComplexInfoString = tmpQuery.toString();
		prologSubQuery.resultVariable = resultVar;

	}

	private void subqueryFuzzyEndTestAndSave(PrologSubQuery prologSubQuery) throws QueryConversorException, CiaoPrologConnectorException {
		ConversionInput input = prologSubQuery.input;

		PredicateInfo predicateInfo = ciaoPrologIntrospectionQuery.getProgramIntrospection().getPredicateInfo(input.initialPredicate);
		if (predicateInfo.getPredicateArity() != 2) {
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

		CiaoPrologTermInJava tmpQuery = new CiaoPrologTermInJava(prologSubQuery.subQuery, null);
		prologSubQuery.SubQueryComplexInfoString = tmpQuery.toString();
		prologSubQuery.resultVariable = tmpVar3;

	}

	private void queryConvert() throws QueryConversorException, CiaoPrologConnectorException {

		if (initialSubQuery == null) {
			throw new QueryConversorException("No initial subQuery to convert.");
		}

		querySimpleInfoString = "";
		queryComplexInfoString = "";

		PLVariable currentVar = null;
		PLVariable tmpVar = null;
		PLStructure aggrSubQuery = null;
		CiaoPrologTermInJava tmpQuery = null;

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
					tmpQuery = new CiaoPrologTermInJava(currentSubQuery.subQuery, null);
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