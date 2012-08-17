package auxiliar;

import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import CiaoJava.*;

public class CiaoPrologConnectionClass {

	final static private Log LOG = LogFactory.getLog(CiaoPrologConnectionClass.class);
	static private PLConnection plServer = null;
	static private FoldersUtilsClass workingFolderObject = null;
	static private String currentlocalUserName = null;
	
	public CiaoPrologConnectionClass() throws PLException, IOException, FoldersUtilsClassException {
		LOG.info("CiaoPrologConnectionClass: Connecting to Ciao Prolog PLServer");
		if (workingFolderObject == null) {
			workingFolderObject = new FoldersUtilsClass();
		}
		
		String [] argv = new String[1];
		argv[0] = workingFolderObject.getPlServerPath();
		if (plServer == null) {
			plServer = new PLConnection(argv);
		}
		LOG.info("CiaoPrologConnectionClass: Connected to Ciao Prolog PLServer. Initializing local objects.");

		// changeCiaoPrologWorkingFolder("");
		LOG.info("CiaoPrologConnectionClass: Connected to Ciao Prolog PLServer. Initialized local objects.");
	}
	
	public void changeCiaoPrologWorkingFolder(String newWorkingFolder) 
			throws FoldersUtilsClassException, PLException, IOException, LocalUserNameFixesClassException {
		// Change it only if necessary.
		LOG.info("changeCiaoPrologWorkingFolder: changing to " + localUserName);
		if ((currentlocalUserName == null) || (! currentlocalUserName.equals(localUserName))) {

			String newWorkingFolder = workingFolderObject.getUserWorkingFolder(localUserName);
			
			// Change working folder.
			PLVariable var1 = new PLVariable();
			PLStructure query = new PLStructure("working_directory",
					new PLTerm[]{var1, new PLAtom(newWorkingFolder)}); 
			PLGoal execution = runQuery(query);
			PLTerm answer = getAnswer(execution);
			LOG.info("changeCiaoPrologWorkingFolder: answer: " + answer.toString());
			
		}
		LOG.info("changeCiaoPrologWorkingFolder: changed to " + localUserName);
	}

	public PLGoal runQuery(PLStructure query) throws PLException {
		LOG.info("runQuery: executing query: " + query);
		if (plServer == null) throw new PLException("runQuery: plServer is null.");
		if (query == null) throw new PLException("runQuery: query is null.");
		LOG.info("runQuery: executed query: " + query);
		return new PLGoal(plServer, query);
	}
	
	public PLTerm getAnswer(PLGoal execution) throws IOException, PLException {
		LOG.info("getAnswer: getting another answer");
		PLTerm answer = null;
		do {
			answer = execution.nextSolution();
		} while ((answer == null) && execution.isStillRunning());
		
		if (answer == null) {
			throw new PLException("getAnswer: Invalid answer.");
		}
		LOG.info("getAnswer: obtained another answer");
		return answer;
	}

}
