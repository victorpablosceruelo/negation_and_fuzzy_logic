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
	private String currentProgramFileName = null;
	private String currentProgramFileOwner = null;
	private String currentProgramFileOwnerWithPath = null;
	private ArrayList<String []> loadedProgramQuantifiers = null;
	private ArrayList<String []> loadedProgramCrispPredicates = null;
	private ArrayList<String []> loadedProgramFuzzyRules = null;
	private String lastQuery = null;
	private ArrayList<String []> lastAnswers = null;
	
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
			throws PLException, IOException, FoldersUtilsClassException, LocalUserNameFixesClassException {
		LOG.info("programFileIntrospectionQuery: fileOwner: "+fileOwner+" fileName: "+fileName);
		
		// rfuzzy_introspection(T, PN, PA).
		PLVariable[] variables = new PLVariable[3];
		variables[0] = new PLVariable(); // predicateType
		variables[1] = new PLVariable(); // predicateName
		variables[2] = new PLVariable(); // predicateArity
		PLTerm[] args = {variables[0], variables[1], variables[2]};
		PLStructure query = new PLStructure("rfuzzy_introspection", args); 
		
		ArrayList<String []> queryAnswers = performQuery(query, fileOwner, fileName, variables);
		Iterator<String []> queryAnswersIterator;
		if (queryAnswers == null) {
			queryAnswersIterator = null;
		}
		else {
			queryAnswersIterator = queryAnswers.iterator();
		
			// Format and store the answers in the lists.
			loadedProgramQuantifiers = new ArrayList<String []>();
			loadedProgramCrispPredicates = new ArrayList<String []>();
			loadedProgramFuzzyRules = new ArrayList<String []>();

			String [] answer;
			while (queryAnswersIterator.hasNext()) {
				answer = queryAnswersIterator.next();

				if ((answer[0] != null) && (answer[1] != null) && (answer[2] != null)) {
					if ("quantifier".equals(answer[0])) {
						loadedProgramQuantifiers.add(answer);
					}
					if ("fuzzy_rule".equals(answer[0])) {
						loadedProgramFuzzyRules.add(answer);
					}
					if ("crisp".equals(answer[0])) {
						loadedProgramCrispPredicates.add(answer);
					}
				}

			}
		}
		currentProgramFileName = fileName;
		LOG.info("programFileIntrospectionQuery: END");
	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public Iterator<String []> getLoadedProgramFuzzyRulesIterator () {
		if (loadedProgramFuzzyRules == null) return null;
		else return loadedProgramFuzzyRules.iterator();
	}
	public Iterator<String []> getLoadedProgramQuantifiersIterator () {
		if (loadedProgramQuantifiers == null) return null;
		else return loadedProgramQuantifiers.iterator();
	}
	public Iterator<String []> getLoadedProgramCrispPredicatesIterator () {
		if (loadedProgramCrispPredicates == null) return null;
		else return loadedProgramCrispPredicates.iterator();
	}
	
	public String getCurrentProgramFileName () { return currentProgramFileName; }
	public String getCurrentProgramFileOwner () { return currentProgramFileOwner; }
	public String getCurrentProgramFileOwnerWithPath () { return currentProgramFileOwnerWithPath; }
	public ArrayList<String []> getLastAnswers () { return lastAnswers; } 
	public String getLastQuery() { return lastQuery; }
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	public ArrayList<String []> performQuery(PLStructure query, String fileOwner, String fileName, PLVariable [] variables) 
			throws PLException, IOException, FoldersUtilsClassException, LocalUserNameFixesClassException {

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
		
		return lastAnswers;
	}
	
	/**
	 * Changes the Ciao Prolog Working Folder 
	 * 
	 * @param     programFileOwner It is the owner of the new program file, which coincides with the folder that contains the file.
	 * @exception FoldersUtilsClassException if the folder can not be created
	 * @exception PLException
	 * @exception IOException
	 * @exception LocalUserNameFixesClassException if the owner string is empty or null
	 */
	private void changeCiaoPrologWorkingFolder(String programFileOwner, PLConnection plConnection) 
			throws FoldersUtilsClassException, PLException, IOException, LocalUserNameFixesClassException {
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

		ArrayList<String []> queryAnswers = performQueryAux(query, null, variables, maximumLong, maximumLong, plConnection);
		Iterator<String []> queryAnswersIterator = queryAnswers.iterator();

		String [] answer;
		while (queryAnswersIterator.hasNext()) {
			answer = queryAnswersIterator.next();
			LOG.info("changeCiaoPrologWorkingFolder: " + answer[0]);
		}
		currentProgramFileOwnerWithPath = programFileOwnerWithPath;
		currentProgramFileOwner = programFileOwner;
		LOG.info("changeCiaoPrologWorkingFolder: changed current working folder to " + currentProgramFileOwner + " at " + currentProgramFileOwnerWithPath);
		
	}
	
	private ArrayList<String []> performQueryAux(PLStructure query, String programFile, PLVariable [] variables, long maxNumAnswers, long maxNumberOfTries, PLConnection plConnection) 
			throws PLException, IOException {
		
		ArrayList<String []> queryAnswers = new ArrayList<String []>();
		
		if (query != null) {
			PLGoal currentGoal = null;
			long answersCounter = 0;

			LOG.info("runQuery: executing query: " + query.toString() + " .... ");
			lastQuery = query.toString();
			
			if (plConnection == null) throw new PLException("runQuery: plConnection is null.");
			currentGoal = new PLGoal(plConnection, query); 
			if ((programFile != null) && (! "".equals(programFile))) {
				LOG.info("runQuery: changing programFile to: " + programFile + ".");
				currentGoal.useModule(programFile);
			}
			currentGoal.query();

			LOG.info("performQueryAux: getting answers ...");
			PLTerm currentQueryAnswer;
			long timesCounter;
			String [] variablesCopy;
			
			do { // Get all the answers you can.
				currentQueryAnswer = null;
				timesCounter = 0;
				// Save the current answer.
				answersCounter ++;
				LOG.info("performQueryAux: getting answer number: "  + answersCounter);
				do { // Get the current answer.
					currentQueryAnswer = currentGoal.nextSolution();
					timesCounter++;
				} while ((currentQueryAnswer == null) && (currentGoal.isStillRunning()) && (timesCounter < maxNumberOfTries));

				if (timesCounter >= maxNumberOfTries){
					LOG.info("performQueryAux: reached maxNumberOfTries: " + timesCounter + " >= " + maxNumberOfTries);
				}
				
				if (currentQueryAnswer != null) {
					String logMsg="\n goal: " + currentGoal.toString();
					logMsg += ("\n answer: " + currentQueryAnswer.toString());
					logMsg += PLVariablesArrayToString(variables);
					LOG.info("performQueryAux: " + logMsg + " ");
					
					variablesCopy = new String [variables.length];
					for (int i=0; i < variables.length; i++) {
						variablesCopy[i] = variables[i].getBinding().toString();
					}
					queryAnswers.add(variablesCopy);
				}
				else {
					LOG.info("performQueryAux: answer obtained: null ");
				}
				
			} while ((currentQueryAnswer != null) && (answersCounter < maxNumAnswers));
			
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
		}
		
		LOG.info("performQueryAux: end.");
		return queryAnswers;
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/** 
	 * Logs the information in the variables.
	 * 
	 * @param variables is an array of PL variables to be logged. 
	 */
	private String PLVariablesArrayToString(PLVariable [] variables) {
		String retVal = "";
		for(int i=0; i<variables.length; i++) {
			retVal += ("\n   var["+i+"]: ");
			if (variables[i] != null) {
				retVal += (variables[i].toString() + " bind: " + variables[i].getBinding());
			}
			else {
				retVal += "null ";
			}
		}
		return retVal;
	}
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Serves for testing the query system, but has no use at all. 
	 */	
	public void testingQuery (String owner, String programFile) throws PLException, IOException, FoldersUtilsClassException, LocalUserNameFixesClassException {
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

			ArrayList<String []> queryAnswers = performQuery(query, owner, programFile, variables);
			LOG.info("testingQuery ... num of answers: " + queryAnswers.size());
		}
	}
}
