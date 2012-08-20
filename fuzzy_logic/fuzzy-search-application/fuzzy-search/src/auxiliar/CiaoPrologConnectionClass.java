package auxiliar;

import java.io.IOException;
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
				runQuery(query);
				PLTerm answer = getAnswer();
				LOG.info("changeCiaoPrologWorkingFolder: answer: " + answer.toString());
		}
		else {
			LOG.info("changeCiaoPrologWorkingFolder: not changing working folder because " + 
					newWorkingFolder + " = " + currentWorkingFolder);
		}
		LOG.info("changeCiaoPrologWorkingFolder: changed to " + newWorkingFolder);
	}

	public void selectDatabase(String database) {
		
	}
	
	public List<CiaoPrologProgramElementClass> databaseIntrospectionQuery() {
		List<CiaoPrologProgramElementClass> programElements = null;
		return programElements;
	}

	
	public void runQuery(PLStructure query) throws PLException, IOException {
		if (query == null) {
			LOG.info("runQuery: query is null.");
			throw new PLException("runQuery: query is null.");
		}
		
		if (currentGoal != null) {
			currentGoal.terminate();
			currentGoal = null;
		}
		
		LOG.info("runQuery: executing query: " + query.toString());
		if (plServer == null) throw new PLException("runQuery: plServer is null.");
		currentGoal = new PLGoal(plServer, query);
		currentGoal.query();
		
		LOG.info("runQuery: executed query: " + query.toString());
	}
	
	public PLTerm getAnswer() throws IOException, PLException {
		LOG.info("getAnswer: getting another answer");
		PLTerm answer = null;
		Integer counter = new Integer(0);
		do {
			answer = currentGoal.nextSolution();
			counter++;
			if (counter.equals(Integer.MAX_VALUE)) {
				LOG.info("We have waited for the answer for " + counter.toString() + " times.");
				counter = new Integer(0);
			}
		} while ((answer == null) && currentGoal.isStillRunning()); // && (counter < loopCounterMaximum));
		
		if (answer == null) {
			LOG.info("getAnswer: answer obtained is null.");
			// throw new PLException("getAnswer: answer obtained is null.");
		}
		else {
			LOG.info("getAnswer: obtained another answer: " + answer.toString());	
		}
		return answer;
	}

}
