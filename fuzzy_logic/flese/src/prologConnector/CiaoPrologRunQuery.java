package prologConnector;

import java.io.IOException;
import java.util.ArrayList;

import filesAndPaths.PathsMgmt;

import CiaoJava.PLConnection;
import CiaoJava.PLException;
import CiaoJava.PLGoal;
import CiaoJava.PLStructure;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;

public class CiaoPrologRunQuery {

	
	
	public CiaoPrologRunQuery(CiaoPrologQuery query) throws Exception {
		
		performQuery(query);
	}
	
	public void performQuery(CiaoPrologQuery query) throws Exception {


		this.variablesNames = variablesNames;

		// Connect to the Ciao Prolog Server.

		PLConnection plConnection = new PLConnection(argv);
		LOG.info("performQuery: Connected to Ciao Prolog server (plServer). ");

		// Change working folder and run the query.
		changeCiaoPrologWorkingFolder(fileOwner, programFilesPath, plConnection);
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
	 * @param programFileOwner
	 *            It is the owner of the new program file, which coincides with
	 *            the folder that contains the file.
	 * @exception FoldersUtilsClassException
	 *                if the folder can not be created
	 * @exception PLException
	 * @exception IOException
	 * @exception LocalUserNameFixesClassException
	 *                if the owner string is empty or null
	 * @throws AnswerTermInJavaClassException
	 *             when the term cannot be converted.
	 */
	private void changeCiaoPrologWorkingFolder(String programFileOwner, String programFilesPath, PLConnection plConnection)
			throws Exception {
		// Log info
		LOG.info("changeCiaoPrologWorkingFolder: folder selected: " + programFileOwner);
	
		if ((programFileOwner == null) || ("".equals(programFileOwner))) {
			throw new Exception("programFileOwner is null or empty.");
		}
	
		if ((programFilesPath == null) || ("".equals(programFilesPath))) {
			throw new Exception("programFilesPath is null or empty.");
		}
	
		// Adequate the value of programFileOwner (it was relative until here).
		PathsMgmt pathsMgmt = new PathsMgmt();
		String programFileOwnerWithPath = pathsMgmt.getFullPathOfFile(programFileOwner, null, false);
	
		// Change working folder.
		PLVariable[] variables = new PLVariable[1];
		variables[0] = new PLVariable();
		PLStructure query = new PLStructure("working_directory", new PLTerm[] { variables[0], new PLAtom(programFileOwnerWithPath) });
	
		performQueryAux(query, null, variables, maximumLong, maximumLong, plConnection);
		LOG.info("changeCiaoPrologWorkingFolder: amount of answers: " + latestEvaluatedQueryAnswers.size());
	
		latestEvaluatedQueryProgramFileOwnerWithPath = programFileOwnerWithPath;
		latestEvaluatedQueryProgramFileOwner = programFileOwner;
		LOG.info("changeCiaoPrologWorkingFolder: changed working folder to " + latestEvaluatedQueryProgramFileOwner + " at "
				+ latestEvaluatedQueryProgramFileOwnerWithPath);
	
	}
	
	private void performQueryAux(PLStructure query, String programFileName, PLVariable[] variables, long maxNumAnswers,
			long maxNumberOfTries, PLConnection plConnection) throws PLException, IOException, AnswerTermInJavaClassException {

	}
	
	
	
}
