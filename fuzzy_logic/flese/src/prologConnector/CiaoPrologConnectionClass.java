package prologConnector;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import CiaoJava.PLAtom;
import CiaoJava.PLConnection;
import CiaoJava.PLException;
import CiaoJava.PLGoal;
import CiaoJava.PLStructure;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;
import filesAndPaths.PathsMgmt;

public class CiaoPrologConnectionClass {

	final static private Log LOG = LogFactory.getLog(CiaoPrologConnectionClass.class);
	final static private 

	// This one can not be shared between different processes.
	private ArrayList<AnswerTermInJavaClass[]> latestEvaluatedQueryAnswers = null;
	private ArrayList<AnswerTermInJavaClass[]> programIntrospection = null;
	private String latestEvaluatedQueryProgramFileName = null;
	private String latestEvaluatedQueryProgramFileOwner = null;
	private String latestEvaluatedQueryProgramFileOwnerWithPath = null;
	private String latestEvaluatedQuery = null;

	private String[] variablesNames = null;

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void clearCacheInCiaoPrologConnectionClass() {
		latestEvaluatedQueryAnswers = null;
		programIntrospection = null;
		latestEvaluatedQueryProgramFileName = null;
		latestEvaluatedQueryProgramFileOwner = null;
		latestEvaluatedQueryProgramFileOwnerWithPath = null;
		latestEvaluatedQuery = null;

	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void programFileIntrospectionQuery(String plServerPath, String programFilesPath, String fileOwner, String fileName)
			throws Exception {
		LOG.info("programFileIntrospectionQuery: fileOwner: " + fileOwner + " fileName: " + fileName);

		if (fileOwner == null)
			throw new Exception("fileOwner cannot be null.");
		if (fileName == null)
			throw new Exception("fileName cannot be null.");
		if ("".equals(fileOwner))
			throw new Exception("fileOwner cannot be empty string.");
		if ("".equals(fileName))
			throw new Exception("fileName cannot be empty string.");

		if ((latestEvaluatedQueryProgramFileName != null) && (latestEvaluatedQueryProgramFileOwner != null)
				&& (latestEvaluatedQueryProgramFileName.equals(fileName)) && (latestEvaluatedQueryProgramFileOwner.equals(fileOwner))) {
			LOG.info("programFileIntrospectionQuery: using the last query results.");
		} else {
			// Prepare the query structure.
			// rfuzzy_introspection(PClass, PName, PArity, PType).
			PLVariable[] variables = new PLVariable[4];
			variables[0] = new PLVariable(); // predicateType
			variables[1] = new PLVariable(); // predicateName
			variables[2] = new PLVariable(); // predicateArity
			variables[3] = new PLVariable(); // predicateType
			PLTerm[] args = { variables[0], variables[1], variables[2], variables[3] };
			PLStructure query = new PLStructure("rfuzzy_introspection", args);

			// Run the query and save the results in programIntrospection
			performQuery(plServerPath, query, programFilesPath, fileOwner, fileName, variables, null);
			programIntrospection = latestEvaluatedQueryAnswers;

			/*
			 * if (programIntrospection == null)
			 * LOG.info("ERROR: queryAnswers is null."); else {
			 * Iterator<AnswerTermInJavaClass []> test =
			 * getProgramIntrospectionIterator(); String testMsg =
			 * " - ProgramIntrospection - "; while (test.hasNext()) {
			 * AnswerTermInJavaClass [] subTest = test.next(); for (int i=0;
			 * i<subTest.length; i++) { testMsg += "\n[" + i + "]: " +
			 * subTest[i].toString(); } } LOG.info(testMsg + "\n"); }
			 */
			LOG.info("programFileIntrospectionQuery: END");
		}
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public String[] getProgramIntrospectionInJS() {
		if (programIntrospection == null)
			return null;
		Iterator<AnswerTermInJavaClass[]> programIntrospectionIterator = null;
		if (programIntrospection != null)
			programIntrospectionIterator = programIntrospection.iterator();

		if (programIntrospectionIterator == null)
			return null;

		String[] result = new String[programIntrospection.size()];

		int counter = 0;
		String tmp = null;
		AnswerTermInJavaClass[] predInfo;
		while (programIntrospectionIterator.hasNext()) {
			predInfo = programIntrospectionIterator.next();
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

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public String[] getQueryAnswersInJS() {
		if (latestEvaluatedQueryAnswers == null)
			return null;
		Iterator<AnswerTermInJavaClass[]> latestEvaluatedQueryAnswersIterator = null;
		if (latestEvaluatedQueryAnswers != null)
			latestEvaluatedQueryAnswersIterator = latestEvaluatedQueryAnswers.iterator();
		if (latestEvaluatedQueryAnswersIterator == null)
			return null;

		String[] result = new String[latestEvaluatedQueryAnswers.size() + 1];
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
		while (latestEvaluatedQueryAnswersIterator.hasNext()) {
			answer = latestEvaluatedQueryAnswersIterator.next();
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
		if (programIntrospection != null)
			iterator = programIntrospection.iterator();

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

	public String getLatestEvaluatedQueryProgramFileName() {
		return latestEvaluatedQueryProgramFileName;
	}

	public String getLatestEvaluatedQueryProgramFileOwner() {
		return latestEvaluatedQueryProgramFileOwner;
	}

	public String getLatestEvaluatedQueryProgramFileOwnerWithPath() {
		return latestEvaluatedQueryProgramFileOwnerWithPath;
	}

	public ArrayList<AnswerTermInJavaClass[]> getLatestEvaluatedQueryAnswers() {
		return latestEvaluatedQueryAnswers;
	}

	public String getLatestEvaluatedQuery() {
		return latestEvaluatedQuery;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////







	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Serves for testing the query system, but has no use at all.
	 * 
	 * @throws AnswerTermInJavaClassException
	 */
	public void testingQuery(String plServerPath, String programFilesPath, String owner, String programFile) throws Exception {
		LOG.info("testingQuery ...");
		if ("restaurant.pl".equals(programFile)) {
			PLVariable[] variables = new PLVariable[6];
			variables[0] = new PLVariable(); // X
			variables[1] = new PLVariable(); // V1
			variables[2] = new PLVariable(); // V2
			variables[3] = new PLVariable(); // V3
			variables[4] = new PLVariable(); // Condition -
												// rfuzzy_var_truth_value
			variables[5] = new PLVariable(); // V - rfuzzy_var_truth_value

			PLTerm[] args_expensive = { variables[0], variables[1] };
			PLStructure query_expensive = new PLStructure("expensive", args_expensive);
			PLTerm[] args_very = { query_expensive, variables[2] };
			PLStructure query_very_expensive = new PLStructure("very", args_very);
			PLTerm[] args_fnot = { query_very_expensive, variables[3] };
			PLStructure query_not_very_expensive = new PLStructure("fnot", args_fnot);

			// PLTerm[] dump_constraints_vars_java_list = {variables[3]};
			// PLList dump_constraints_vars_list = null;
			// try {
			// dump_constraints_vars_list= new
			// PLList(dump_constraints_vars_java_list);
			// } catch (PLException e) {}

			PLTerm[] args_rfuzzy_var_truth_value = { variables[3], variables[4], variables[5] };
			PLStructure query_dump_constraints = new PLStructure("rfuzzy_var_truth_value", args_rfuzzy_var_truth_value);

			PLTerm[] args_conjunction = { query_not_very_expensive, query_dump_constraints };
			PLStructure query = new PLStructure(",", args_conjunction);

			performQuery(plServerPath, query, programFilesPath, owner, programFile, variables, null);
			LOG.info("testingQuery ... num of answers: " + latestEvaluatedQueryAnswers.size());
		}
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
