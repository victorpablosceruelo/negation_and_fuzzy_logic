package auxiliar;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import CiaoJava.*;

public class CiaoPrologConnectionClass {

	final static private Log LOG = LogFactory.getLog(CiaoPrologConnectionClass.class);
	static private FoldersUtilsClass FoldersUtilsObject = null;
	static private String currentWorkingFolder = null;
	
	// This one can not be shared between different processes.
	private PLConnection plServer = null;
	private PLGoal currentGoal = null;
	
	public CiaoPrologConnectionClass() throws PLException, IOException, FoldersUtilsClassException {
		LOG.info("CiaoPrologConnectionClass: Connecting to Ciao Prolog PLServer");
		if (FoldersUtilsObject == null) {
			FoldersUtilsObject = new FoldersUtilsClass();
		}
		
		String [] argv = new String[1];
		argv[0] = FoldersUtilsObject.getPlServerPath();
		if (plServer == null) {
			plServer = new PLConnection(argv);
		}
		LOG.info("CiaoPrologConnectionClass: Connected to Ciao Prolog PLServer. Initializing local objects.");

		// changeCiaoPrologWorkingFolder("");
		LOG.info("CiaoPrologConnectionClass: Connected to Ciao Prolog PLServer. Initialized local objects.");
	}
	
	public void changeCiaoPrologWorkingFolder(String newWorkingFolder) 
			throws FoldersUtilsClassException, PLException, IOException, LocalUserNameFixesClassException {
		// Log info
		LOG.info("changeCiaoPrologWorkingFolder: folder selected: " + newWorkingFolder);
		
		if ((newWorkingFolder == null) || ("".equals(newWorkingFolder))){
			LOG.info("changeCiaoPrologWorkingFolder: newWorkingFolder is null or empty.");
			throw new FoldersUtilsClassException("changeCiaoPrologWorkingFolder: newWorkingFolder is null or empty.");
		}
		
		if ((! FoldersUtilsObject.folderExists(newWorkingFolder, true))) {
			LOG.info("changeCiaoPrologWorkingFolder: newWorkingFolder is an invalid folder.");
			throw new FoldersUtilsClassException("changeCiaoPrologWorkingFolder: newWorkingFolder is an invalid folder.");
		}
		
		// Adequate the value of newWorkingFolder (it was relative until here).
		newWorkingFolder = FoldersUtilsObject.getprogramsPath() + newWorkingFolder;
		
		// Change it only if necessary.
		if ((currentWorkingFolder == null) || (! currentWorkingFolder.equals(newWorkingFolder))) { 
			
				// Change working folder.
				PLVariable var1 = new PLVariable();
				PLStructure query = new PLStructure("working_directory",
						new PLTerm[]{var1, new PLAtom(newWorkingFolder)}); 
				runQueryEvaluation(query);
				PLTerm queryAnswered = getQueryAnswered();
				LOG.info("changeCiaoPrologWorkingFolder: queryAnswered: " + queryAnswered.toString());
				LOG.info("changeCiaoPrologWorkingFolder: var1 value: " + var1.toString());
		}
		else {
			LOG.info("changeCiaoPrologWorkingFolder: not changing working folder because " + 
					newWorkingFolder + " = " + currentWorkingFolder);
		}
		LOG.info("changeCiaoPrologWorkingFolder: changed to " + newWorkingFolder);
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
		PLStructure query = new PLStructure("use_module", new PLTerm[]{new PLAtom(databaseFullPath)}); 
		runQueryEvaluation(query);
		PLTerm queryAnswered = getQueryAnswered();
		LOG.info("selectDatabase: queryAnswered: " + queryAnswered.toString());
		LOG.info("selectDatabase: selected owner: " + owner + " database: " + database);
		
	}
	
	public ArrayList<CiaoPrologProgramElementInfoClass> databaseIntrospectionQuery() 
			throws PLException, IOException {
		ArrayList<CiaoPrologProgramElementInfoClass> programElements = new ArrayList<CiaoPrologProgramElementInfoClass>();
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
			programElements.add(answer);
		}
		return programElements;
	}

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
		if (plServer == null) throw new PLException("runQuery: plServer is null.");
		currentGoal = new PLGoal(plServer, query);
		currentGoal.query();
		
		LOG.info("runQuery: executed query: " + query.toString());
	}
	
	/**
	 * Terminates the evaluation of the current query.
	 */
	public void runQueryTermination() {
		if (currentGoal != null) {
			try {
				currentGoal.terminate();
			} catch (Exception e) {
				e.printStackTrace();
			} 
			currentGoal = null;
		}	
	}
	
	/**
	 * Destroys the current connection to the plServer, so it should dye. 
	 */
	public void connectionTermination() {
		runQueryTermination();
		if (plServer != null) {
			try {
				plServer.stop();
			} catch (Exception e) {
				e.printStackTrace();
			} 
			plServer = null;
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
			LOG.info("getAnswer: answer obtained is null.");
			// throw new PLException("getAnswer: answer obtained is null.");
		}
		else {
			LOG.info("getAnswer: obtained another answer: " + queryAnswered.toString());	
		}
		return queryAnswered;
	}

}
