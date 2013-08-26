package prologConnector;

import java.util.Iterator;

import CiaoJava.PLStructure;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;
import auxiliar.LocalUserInfoException;
import filesAndPaths.PathsMgmtException;

public class CiaoPrologProgramIntrospectionQuery extends CiaoPrologQuery {

	public CiaoPrologProgramIntrospectionQuery(String fileOwner, String fileName) throws CiaoPrologQueryException, PathsMgmtException,
			LocalUserInfoException {
		super(fileOwner, fileName);

		// Prepare the query structure.
		// rfuzzy_introspection(PClass, PName, PArity, PType).
		PLVariable[] variables = new PLVariable[4];
		variables[0] = new PLVariable(); // predicateType
		variables[1] = new PLVariable(); // predicateName
		variables[2] = new PLVariable(); // predicateArity
		variables[3] = new PLVariable(); // predicateType
		PLTerm[] args = { variables[0], variables[1], variables[2], variables[3] };
		PLStructure query = new PLStructure("rfuzzy_introspection", args);

		String[] variablesNames = { "predicateType", "predicateName", "predicateArity", "predicateType" };

		setRealQuery(query, variables, variablesNames);

		isProgramIntrospectionQuery = true;
	}

	public String[] getQueryAnswersInJS() {
		if (queryAnswers == null)
			return null;
		
		Iterator<AnswerTermInJavaClass[]> queryAnswersIterator = queryAnswers.iterator();
		if (queryAnswersIterator == null)
			return null;

		String[] result = new String[queryAnswers.size() + 1];
		int answersCounter = 0;

		if (variablesNames != null) {
			result[answersCounter] = "addToProgramQueryAnsers(" + answersCounter + ", new Array(";
			for (int i = 0; i < variablesNames.length; i++) {
				result[answersCounter] += "'" + variablesNames[i] + "'";
				if ((i + 1) < variablesNames.length)
					result[answersCounter] += ", ";
			}
			result[answersCounter] += ")); ";
		}

		answersCounter++;
		AnswerTermInJavaClass[] answer;
		while (queryAnswersIterator.hasNext()) {
			answer = queryAnswersIterator.next();
			result[answersCounter] = "addToProgramQueryAnsers(" + answersCounter + ", new Array(";
			for (int i = 0; i < answer.length; i++) {
				result[answersCounter] += answer[i].toJavaScript();
				if ((i + 1) < answer.length)
					result[answersCounter] += ", ";
			}
			result[answersCounter] += ")); ";
			answersCounter++;
		}
		return result;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public AnswerTermInJavaClass[] getPredicateInfo(String predicateName) {
		Iterator<AnswerTermInJavaClass[]> iterator = null;
		if (queryAnswers != null)
			iterator = queryAnswers.iterator();

		if ((predicateName == null) || ("".equals(predicateName))) {
			LOG.info("Predicate Name is not valid. predicateName: " + predicateName);
		}
		if (iterator == null) {
			LOG.error("Iterator of Program Introspection is NULL!! ");
		}

		AnswerTermInJavaClass[] answer = null;
		if ((predicateName != null) && (iterator != null)) {
			while ((iterator.hasNext()) && (answer == null)) {
				answer = iterator.next();
				if (!predicateName.equals(answer[0].toString()))
					answer = null;
			}
		}
		return answer;
	}
	
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	public String[] getProgramIntrospectionInJS() {
		if (queryAnswers == null)
			return null;
		Iterator<AnswerTermInJavaClass[]> queryAnswersIterator = queryAnswers.iterator();

		if (queryAnswersIterator == null)
			return null;

		String[] result = new String[queryAnswers.size()];

		int counter = 0;
		String tmp = null;
		AnswerTermInJavaClass[] predInfo;
		while (queryAnswersIterator.hasNext()) {
			predInfo = queryAnswersIterator.next();
			tmp = "";
			tmp += "addToProgramIntrospection(" + counter + ", new predInfo(";
			for (int i = 0; i < predInfo.length; i++) {
				tmp += predInfo[i].toJavaScript();
				if (i + 1 < predInfo.length)
					tmp += ",";
			}
			tmp += "));";
			result[counter] = tmp;
			counter++;
		}
		return result;
	}
}
