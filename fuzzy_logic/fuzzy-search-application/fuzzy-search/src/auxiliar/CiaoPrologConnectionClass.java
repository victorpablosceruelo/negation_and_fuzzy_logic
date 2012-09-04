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
	
	// This one can not be shared between different processes.
	private PLConnection currentPlConnection = null;
	private PLGoal currentGoal = null;
	private int answersCounter;
	private String currentDatabase = null;
	private String currentDatabaseOwner = null;
	private ArrayList<CiaoPrologProgramElementInfoClass> loadedProgramInfo = null;
	// private String currentWorkingFolder = null;
	
	public CiaoPrologConnectionClass() throws PLException, IOException, FoldersUtilsClassException {
		LOG.info("CiaoPrologConnectionClass: Connecting to Ciao Prolog server (plServer) ...");
		if (FoldersUtilsObject == null) {
			FoldersUtilsObject = new FoldersUtilsClass();
		}
		
		String [] argv = new String[1];
		argv[0] = FoldersUtilsObject.getPlServerPath();
		if (currentPlConnection == null) {
			currentPlConnection = new PLConnection(argv);
		}
		LOG.info("CiaoPrologConnectionClass: Connected to Ciao Prolog server (plServer). Initializing local objects.");

		// changeCiaoPrologWorkingFolder("");
		LOG.info("CiaoPrologConnectionClass: Connected to Ciao Prolog server (plServer). Initialized local objects.");
	}
	
	/**
	 * Changes the Ciao Prolog Working Folder 
	 * 
	 * @param     dataBaseOwner It is the owner of the new database, which coincides with the folder that contains the database.
	 * @exception FoldersUtilsClassException if the folder can not be created
	 * @exception PLException
	 * @exception IOException
	 * @exception LocalUserNameFixesClassException if the owner string is empty or null
	 */
	private void changeCiaoPrologWorkingFolder(String dataBaseOwner) 
			throws FoldersUtilsClassException, PLException, IOException, LocalUserNameFixesClassException {
		// Log info
		LOG.info("changeCiaoPrologWorkingFolder: folder selected: " + dataBaseOwner);
		
		if ((dataBaseOwner == null) || ("".equals(dataBaseOwner))){
			LOG.info("changeCiaoPrologWorkingFolder: dataBaseOwner is null or empty.");
			throw new FoldersUtilsClassException("changeCiaoPrologWorkingFolder: dataBaseOwner is null or empty.");
		}
		
		if ((! FoldersUtilsObject.folderExists(dataBaseOwner, true))) {
			LOG.info("changeCiaoPrologWorkingFolder: dataBaseOwner is an invalid folder.");
			throw new FoldersUtilsClassException("changeCiaoPrologWorkingFolder: newWorkingFolder is an invalid folder.");
		}
		
		// Adequate the value of dataBaseOwner (it was relative until here).
		dataBaseOwner = FoldersUtilsObject.getprogramsPath() + dataBaseOwner;
		
		// Change it only if necessary.
		if ((currentDatabaseOwner == null) || (! currentDatabaseOwner.equals(dataBaseOwner))) { 
			
				// Change working folder.
				PLVariable var1 = new PLVariable();
				PLStructure query = new PLStructure("working_directory",
						new PLTerm[]{var1, new PLAtom(dataBaseOwner)}); 
				runQueryEvaluation(query);
				PLTerm queryAnswered = getQueryAnswered();
				currentDatabaseOwner = dataBaseOwner;
				
				LOG.info("changeCiaoPrologWorkingFolder: queryAnswered: " + queryAnswered.toString());
				LOG.info("changeCiaoPrologWorkingFolder: var1 value: " + var1.toString());
				LOG.info("changeCiaoPrologWorkingFolder: changed current working folder to " + currentDatabaseOwner);
		}
		else {
			LOG.info("changeCiaoPrologWorkingFolder: not changing current working folder. " + 
					 "Current working folder: " + currentDatabaseOwner);
		}
		
	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void selectDatabase(String owner, String database) 
			throws FoldersUtilsClassException, LocalUserNameFixesClassException, PLException, IOException {
		// Usage example:
		// use_module('/home/vpablos/secured/negation_and_fuzzy_logic/fuzzy_logic/rfuzzy/examples/age.pl').
		LOG.info("selectDatabase: selecting owner: " + owner + " database: " + database);
		if (! FoldersUtilsObject.databaseExists(owner, database, true)) {
			throw new FoldersUtilsClassException("ERROR: requested database does not exist.");
		}
		String databaseFullPath = FoldersUtilsObject.getCompletePathOfDatabase(owner, database);
		if ((currentDatabase == null) || (! currentDatabase.equals(database))) { 
			changeCiaoPrologWorkingFolder(owner);

			PLStructure query = new PLStructure("use_module", new PLTerm[]{new PLAtom(databaseFullPath)}); 
			runQueryEvaluation(query);
			PLTerm queryAnswered = getQueryAnswered();
			LOG.info("selectDatabase: queryAnswered: " + queryAnswered.toString());
			LOG.info("selectDatabase: selected database: " + database + " of owner: " + owner);
			
			databaseIntrospectionQuery();
		}
		else {
			LOG.info("selectDatabase: not changing current database: " + database);
		}
		
	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	private void databaseIntrospectionQuery() throws PLException, IOException {
		LOG.info("databaseIntrospectionQuery");
		loadedProgramInfo = new ArrayList<CiaoPrologProgramElementInfoClass>();
		// rfuzzy_introspection(T, PN, PA).
		PLVariable predicateType = new PLVariable();
		PLVariable predicateName = new PLVariable();
		PLVariable predicateArity = new PLVariable();
		PLStructure query = new PLStructure("rfuzzy_introspection", new PLTerm[]{predicateType, predicateName,predicateArity}); 
		runQueryEvaluation(query);
		PLTerm queryAnswered = getQueryAnswered();
		while (queryAnswered != null) {
			CiaoPrologProgramElementInfoClass answer = new CiaoPrologProgramElementInfoClass();
			answer.setPredicateType(predicateType.toString());
			answer.setPredicateName(predicateName.toString());
			answer.setPredicateArity(predicateArity.toString());
			answer.log_info();
			loadedProgramInfo.add(answer);
			queryAnswered = getQueryAnswered();
		}
	}

	public ArrayList<CiaoPrologProgramElementInfoClass> getDatabaseIntrospectionArrayList() {
		return loadedProgramInfo;
	}
	
	public Iterator<CiaoPrologProgramElementInfoClass> getDatabaseIntrospectionIterator() {
		Iterator<CiaoPrologProgramElementInfoClass> loadedProgramInfoIterator = null;
		try {
			if ((! (loadedProgramInfo == null)) && (! (loadedProgramInfo.isEmpty()))) {
				loadedProgramInfoIterator = loadedProgramInfo.iterator(); 
			}
		} catch (Exception e) {
			LOG.info("Exception: " + e);
			e.printStackTrace();
			loadedProgramInfoIterator = null;
		}
		return loadedProgramInfoIterator;
	}
	
	public String getCurrentDatabase () { return currentDatabase; }
	public String getCurrentDatabaseOwner () { return currentDatabaseOwner; }
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Asks the prolog server to evaluate the query.
	 * 
	 * @param query is the query to be evaluated.
	 */
	public void runQueryEvaluation(PLStructure query) throws PLException, IOException {
		if (query == null) {
			LOG.info("runQuery: query is null.");
			throw new PLException("runQuery: query is null.");
		}
		
		if (currentGoal != null) {
			runQueryTermination();
		}
		
		LOG.info("runQuery: executing query: " + query.toString());
		if (currentPlConnection == null) throw new PLException("runQuery: plConnection is null.");
		currentGoal = new PLGoal(currentPlConnection, query);
		currentGoal.query();
		answersCounter=0;
		
		LOG.info("runQuery: executed query: " + query.toString());
	}
	
	/**
	 * Terminates the evaluation of the current query.
	 */
	public void runQueryTermination() {
		LOG.info("runQueryTermination: start");
		if (currentGoal != null) {
			try {
				currentGoal.terminate();
			} catch (Exception e) {
				e.printStackTrace();
			} 
			currentGoal = null;
			answersCounter=-1; // Notify that there is no currentGoal.
		}
		LOG.info("runQueryTermination: end");
	}
	
	/**
	 * Destroys the current connection to the Ciao Prolog server, so it should dye. 
	 */
	public void connectionTermination() {
		runQueryTermination();
		if (currentPlConnection != null) {
			try {
				currentPlConnection.stop();
			} catch (Exception e) {
				e.printStackTrace();
			} 
			currentPlConnection = null;
		}
	}
	
	
	/**
	 * Obtains the answer to the query, but the answer is contained in the original query.
	 * This differs from the usual behavior in Prolog interpreters, 
	 * where we get just the answers to the variables involved.
	 */
	public PLTerm getQueryAnswered() throws IOException, PLException {
		LOG.info("getAnswer: getting another answer");
		PLTerm queryAnswered = null;
		Integer counter = new Integer(0);
		do {
			queryAnswered = currentGoal.nextSolution();
			counter++;
			if (counter.equals(Integer.MAX_VALUE)) {
				LOG.info("We have waited for the answer for " + counter.toString() + " times.");
				counter = new Integer(0);
			}
		} while ((queryAnswered == null) && currentGoal.isStillRunning()); // && (counter < loopCounterMaximum));
		
		if (queryAnswered == null) {
			LOG.info("getAnswer: answer obtained is null. AnswersCounter: " + answersCounter);
			// throw new PLException("getAnswer: answer obtained is null.");
		}
		else {
			answersCounter++;
			LOG.info("getAnswer: obtained another answer. AnswersCounter: "  + answersCounter + " Answer: " + queryAnswered.toString());	
		}
		return queryAnswered;
	}
	
	public int getAnswersCounter () { return answersCounter; }

}
