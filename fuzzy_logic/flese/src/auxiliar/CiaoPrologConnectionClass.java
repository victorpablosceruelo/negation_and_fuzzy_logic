package auxiliar;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import CiaoJava.*;

public class CiaoPrologConnectionClass {

	final static private Log LOG = LogFactory.getLog(CiaoPrologConnectionClass.class);
	static private FoldersUtilsClass FoldersUtilsObject = null;
	final static private long maximumLong = 9223372036854775807L;
	
	// This one can not be shared between different processes.
	private ArrayList<AnswerTermInJavaClass []> latestEvaluatedQueryAnswers = null;
	private ArrayList<AnswerTermInJavaClass []> programIntrospection = null;
	private String latestEvaluatedQueryProgramFileName = null;
	private String latestEvaluatedQueryProgramFileOwner = null;
	private String latestEvaluatedQueryProgramFileOwnerWithPath = null;
	private String latestEvaluatedQuery = null;
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public CiaoPrologConnectionClass() throws PLException, IOException, FoldersUtilsClassException {
		LOG.info("CiaoPrologConnectionClass: Connecting to Ciao Prolog server (plServer) ...");
		if (FoldersUtilsObject == null) {
			FoldersUtilsObject = new FoldersUtilsClass();
		}
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void programFileIntrospectionQuery(String fileOwner, String fileName) 
			throws PLException, IOException, FoldersUtilsClassException, LocalUserNameFixesClassException, AnswerTermInJavaClassException {
		LOG.info("programFileIntrospectionQuery: fileOwner: "+fileOwner+" fileName: "+fileName);
		
		if ((latestEvaluatedQueryProgramFileName != null) &&
			(latestEvaluatedQueryProgramFileOwner != null) &&
			(latestEvaluatedQueryProgramFileName.equals(fileName)) &&
			(latestEvaluatedQueryProgramFileOwner.equals(fileOwner))) {
			LOG.info("programFileIntrospectionQuery: using the last query results.");
		}
		else {
			// Prepare the query structure.
			// rfuzzy_introspection(PClass, PName, PArity, PType).
			PLVariable[] variables = new PLVariable[4];
			variables[0] = new PLVariable(); // predicateType
			variables[1] = new PLVariable(); // predicateName
			variables[2] = new PLVariable(); // predicateArity
			variables[3] = new PLVariable(); // predicateType
			PLTerm[] args = {variables[0], variables[1], variables[2], variables[3]};
			PLStructure query = new PLStructure("rfuzzy_introspection", args); 

			// Run the query and save the results in programIntrospection
			performQuery(query, fileOwner, fileName, variables);
			programIntrospection = latestEvaluatedQueryAnswers;

			if (programIntrospection == null) LOG.info("ERROR: queryAnswers is null.");
			else {
				Iterator<AnswerTermInJavaClass []> test = getProgramIntrospectionIterator();
				String testMsg = " - ProgramIntrospection - ";
				while (test.hasNext()) {
					AnswerTermInJavaClass [] subTest = test.next();
					for (int i=0; i<subTest.length; i++) {
						testMsg += "\n[" + i + "]: " + subTest[i].toString();
					}
				}
				LOG.info(testMsg + "\n");
			}
			LOG.info("programFileIntrospectionQuery: END");
		}
	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public Iterator<AnswerTermInJavaClass []> getProgramIntrospectionIterator() {
		if (programIntrospection == null) return null;
		else return programIntrospection.iterator();
	}
	public Iterator<AnswerTermInJavaClass []> getLatestEvaluatedQueryAnswersIterator() {
		if (latestEvaluatedQueryAnswers == null) return null;
		else return latestEvaluatedQueryAnswers.iterator();
	}
	public AnswerTermInJavaClass [] getPredicateInfo (String predicateName) {
		Iterator<AnswerTermInJavaClass []> iterator = getProgramIntrospectionIterator();
		if ((predicateName == null) || ("".equals(predicateName))) {
			LOG.info("Predicate Name is not valid. predicateName: " + predicateName);
		}
		if (iterator == null) {
			LOG.error("Iterator of Program Introspection is NULL!! ");
		}
		
		AnswerTermInJavaClass [] answer = null;
		if ((predicateName != null) && (iterator != null)) {
			while ((iterator.hasNext()) && (answer == null)) {
				answer = iterator.next();
				if (! predicateName.equals(answer[0].toString())) answer = null;
			}
		}
		return answer;
	}
	
	public String getLatestEvaluatedQueryProgramFileName () { 
		return latestEvaluatedQueryProgramFileName; }
	public String getLatestEvaluatedQueryProgramFileOwner () { 
		return latestEvaluatedQueryProgramFileOwner; }
	public String getLatestEvaluatedQueryProgramFileOwnerWithPath () { 
		return latestEvaluatedQueryProgramFileOwnerWithPath; }
	public ArrayList<AnswerTermInJavaClass []> getLatestEvaluatedQueryAnswers () { 
		return latestEvaluatedQueryAnswers; } 
	public String getLatestEvaluatedQuery() { 
		return latestEvaluatedQuery; }
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	public void performQuery(PLStructure query, String fileOwner, String fileName, PLVariable [] variables) 
			throws PLException, IOException, FoldersUtilsClassException, LocalUserNameFixesClassException, AnswerTermInJavaClassException {

		// Connect to the Ciao Prolog Server.
		String [] argv = new String[1];
		argv[0] = FoldersUtilsObject.getPlServerPath();
		PLConnection plConnection = new PLConnection(argv);
		LOG.info("performQuery: Connected to Ciao Prolog server (plServer). ");

		// Change working folder and run the query.
		changeCiaoPrologWorkingFolder(fileOwner, plConnection);
		performQueryAux(query, fileName, variables, maximumLong, maximumLong, plConnection);
		
		if (plConnection != null) {
			try {
				plConnection.stop();
			} catch (Exception e) {
				e.printStackTrace();
			} 
		}
	}
	
	/**
	 * Changes the Ciao Prolog Working Folder 
	 * 
	 * @param     programFileOwner It is the owner of the new program file, which coincides with the folder that contains the file.
	 * @exception FoldersUtilsClassException if the folder can not be created
	 * @exception PLException
	 * @exception IOException
	 * @exception LocalUserNameFixesClassException if the owner string is empty or null
	 * @throws AnswerTermInJavaClassException when the term cannot be converted.
	 */
	private void changeCiaoPrologWorkingFolder(String programFileOwner, PLConnection plConnection) 
			throws FoldersUtilsClassException, PLException, IOException, LocalUserNameFixesClassException, AnswerTermInJavaClassException {
		// Log info
		LOG.info("changeCiaoPrologWorkingFolder: folder selected: " + programFileOwner);
		
		if ((programFileOwner == null) || ("".equals(programFileOwner))){
			LOG.info("changeCiaoPrologWorkingFolder: programFileOwner is null or empty.");
			throw new FoldersUtilsClassException("changeCiaoPrologWorkingFolder: programFileOwner is null or empty.");
		}
		
		if ((! FoldersUtilsObject.folderExists(programFileOwner, true))) {
			LOG.info("changeCiaoPrologWorkingFolder: programFileOwner is an invalid folder.");
			throw new FoldersUtilsClassException("changeCiaoPrologWorkingFolder: newWorkingFolder is an invalid folder.");
		}
		
		// Adequate the value of programFileOwner (it was relative until here).
		String programFileOwnerWithPath = FoldersUtilsObject.getProgramFilesPath() + programFileOwner;
		
		// Change working folder.
		PLVariable [] variables = new PLVariable[1];
		variables[0] = new PLVariable();
		PLStructure query = new PLStructure("working_directory",
				new PLTerm[]{variables[0], new PLAtom(programFileOwnerWithPath)}); 

		performQueryAux(query, null, variables, maximumLong, maximumLong, plConnection);
		Iterator<AnswerTermInJavaClass []> queryAnswersIterator = latestEvaluatedQueryAnswers.iterator();

		// Log results
		while (queryAnswersIterator.hasNext()) {
			LOG.info("changeCiaoPrologWorkingFolder: " + queryAnswersIterator.next().toString());
		}
		latestEvaluatedQueryProgramFileOwnerWithPath = programFileOwnerWithPath;
		latestEvaluatedQueryProgramFileOwner = programFileOwner;
		LOG.info("changeCiaoPrologWorkingFolder: changed working folder to " + latestEvaluatedQueryProgramFileOwner + " at " + latestEvaluatedQueryProgramFileOwnerWithPath);
		
	}
	
	private void performQueryAux(PLStructure query, String programFileName, PLVariable [] variables, long maxNumAnswers, long maxNumberOfTries, PLConnection plConnection) 
			throws PLException, IOException, AnswerTermInJavaClassException {
		
		// Initialize ...
		latestEvaluatedQuery = null;
		latestEvaluatedQueryProgramFileName = null;
		latestEvaluatedQueryAnswers = new ArrayList<AnswerTermInJavaClass []>();
		
		if (query == null) throw new PLException("query is null.");
		if (plConnection == null) throw new PLException("runQuery: plConnection is null.");
		for (int i=0; i<variables.length; i++) {
			if (variables[i] == null) {
				throw new PLException("runQuery: variables["+i+"] is null.");
			}
		}
		
		PLGoal currentGoal = null;
		long answersCounter = 0;

		LOG.info("runQuery: executing query: " + query.toString() + " .... ");
		latestEvaluatedQuery = query.toString();

		currentGoal = new PLGoal(plConnection, query); 
		if ((programFileName != null) && (! "".equals(programFileName))) {
			LOG.info("runQuery: changing programFile to: " + programFileName + ".");
			latestEvaluatedQueryProgramFileName = programFileName;
			currentGoal.useModule(programFileName);
		}
		currentGoal.query();

		LOG.info("performQueryAux: getting answers ...");
		PLTerm prologQueryAnswer;
		AnswerTermInJavaClass [] answerTermInJava = null;
		long timesCounter;

		do { // Get all the answers you can.
			prologQueryAnswer = null;
			answerTermInJava = null;
			timesCounter = 0;
			// Save the current answer.
			answersCounter ++;
			LOG.info("performQueryAux: getting answer number: "  + answersCounter);
			do { // Get the current answer.
				prologQueryAnswer = currentGoal.nextSolution();
				timesCounter++;
			} while ((prologQueryAnswer == null) && (currentGoal.isStillRunning()) && (timesCounter < maxNumberOfTries));

			if (timesCounter >= maxNumberOfTries){
				LOG.info("performQueryAux: reached maxNumberOfTries: " + timesCounter + " >= " + maxNumberOfTries);
			}

			String preMsg = "\n goal: " + currentGoal.toString();
			if (prologQueryAnswer != null) {
				preMsg += PLVariablesToString(variables);
				preMsg += "\n   result: ";
				answerTermInJava = new AnswerTermInJavaClass [variables.length];
				for (int i=0; i<variables.length; i++) {
					if (variables[i] != null) {
						answerTermInJava[i] = new AnswerTermInJavaClass(variables[i], prologQueryAnswer);
						preMsg += answerTermInJava[i].toString() + " ";
					}
					else {
						answerTermInJava[i] = null;
						preMsg += "null ";
					}
				}
				/*
				preMsg += "\n   Creation MSGS: ";
				for (int i=0; i<variables.length; i++) {
					if (answerTermInJava[i] != null) {
						preMsg += answerTermInJava[i].getCreationMsgs();
					}
					else {
						preMsg += " answerTermInJava["+i+"] is null ";
					}
				}
				*/
				latestEvaluatedQueryAnswers.add(answerTermInJava);
				LOG.info(preMsg);
			}
			else {
				LOG.info("performQueryAux: answer obtained: null ");
			}

		} while ((prologQueryAnswer != null) && (answersCounter < maxNumAnswers));

		LOG.info("performQueryAux: terminating goal execution ...");
		if (currentGoal != null) {
			try {
				currentGoal.terminate();
			} catch (Exception e) {
				e.printStackTrace();
			} 
			currentGoal = null;
			answersCounter=-1; // Notify that there is no currentGoal.
		}
		
		LOG.info("performQueryAux: end.");
	}
		
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Serves for testing the query system, but has no use at all. 
	 * @throws AnswerTermInJavaClassException 
	 */	
	public void testingQuery (String owner, String programFile) throws PLException, IOException, FoldersUtilsClassException, LocalUserNameFixesClassException, AnswerTermInJavaClassException {
		LOG.info("testingQuery ...");
		if ("restaurant.pl".equals(programFile)) {
			PLVariable[] variables = new PLVariable[6];
			variables[0] = new PLVariable(); // X
			variables[1] = new PLVariable(); // V1
			variables[2] = new PLVariable(); // V2
			variables[3] = new PLVariable(); // V3
			variables[4] = new PLVariable(); // Condition - rfuzzy_var_truth_value
			variables[5] = new PLVariable(); // V - rfuzzy_var_truth_value

			PLTerm[] args_expensive = {variables[0], variables[1]};
			PLStructure query_expensive = new PLStructure("expensive", args_expensive);
			PLTerm[] args_very = {query_expensive, variables[2]};
			PLStructure query_very_expensive = new PLStructure("very", args_very);
			PLTerm[] args_fnot = {query_very_expensive, variables[3]};
			PLStructure query_not_very_expensive = new PLStructure("fnot", args_fnot);

			//PLTerm[] dump_constraints_vars_java_list = {variables[3]};
			//PLList dump_constraints_vars_list = null;
			//try {
			//	dump_constraints_vars_list= new PLList(dump_constraints_vars_java_list);
			//} catch (PLException e) {}

			PLTerm[] args_rfuzzy_var_truth_value = {variables[3], variables[4], variables[5]};
			PLStructure query_dump_constraints = new PLStructure("rfuzzy_var_truth_value", args_rfuzzy_var_truth_value);

			PLTerm[] args_conjunction = {query_not_very_expensive, query_dump_constraints};
			PLStructure query = new PLStructure(",", args_conjunction);

			performQuery(query, owner, programFile, variables);
			LOG.info("testingQuery ... num of answers: " + latestEvaluatedQueryAnswers.size());
		}
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/** 
	 * Logs the information in the variables.
	 * 
	 * @param variables is an array of PL variables to be logged. 
	 */
	private String PLVariablesToString(PLVariable [] variables) {
		String msg = "";
		for(int i=0; i<variables.length; i++) {
			msg += ("\n   var["+i+"]: ");
			if (variables[i] != null) {
				msg += (variables[i].toString() + " bind: " + variables[i].getBinding());
			}
			else {
				msg += "null ";
			}
		}
		return msg;
	}
}
