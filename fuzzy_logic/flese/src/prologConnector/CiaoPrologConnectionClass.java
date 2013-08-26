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



	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////



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

	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
