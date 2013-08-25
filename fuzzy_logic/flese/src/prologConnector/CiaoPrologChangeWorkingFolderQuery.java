package prologConnector;

import auxiliar.LocalUserInfoException;
import filesAndPaths.PathsMgmtException;
import CiaoJava.PLAtom;
import CiaoJava.PLStructure;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;

public class CiaoPrologChangeWorkingFolderQuery extends CiaoPrologQuery {

	public CiaoPrologChangeWorkingFolderQuery(String fileOwner, String fileName) throws CiaoPrologQueryException, PathsMgmtException, LocalUserInfoException {
		super(fileOwner, fileName);

		
		String programFileFolderName = getProgramFileFolderName();

		// Prepare the variables to get the result
		PLVariable[] variables = new PLVariable[1];
		variables[0] = new PLVariable();

		// Prepare the variables names
		String[] variablesNames = new String[1];
		variablesNames[0] = "result";
		
		// Prepare the "change working folder" query.
		PLStructure query = new PLStructure("working_directory", new PLTerm[] { variables[0], new PLAtom(programFileFolderName) });

		setRealQuery(query, variables, variablesNames);

	}
	
}
