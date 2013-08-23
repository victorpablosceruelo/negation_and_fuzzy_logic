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
	
	}
	
	private void performQueryAux(PLStructure query, String programFileName, PLVariable[] variables, long maxNumAnswers,
			long maxNumberOfTries, PLConnection plConnection) throws PLException, IOException, AnswerTermInJavaClassException {

	}
	
	
	
}
