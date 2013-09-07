package prologConnector;

import CiaoJava.PLAtom;
import CiaoJava.PLStructure;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;
import filesAndPaths.PathsMgmtException;
import filesAndPaths.ProgramFileInfo;

public class CiaoPrologChangeWorkingFolderQuery extends CiaoPrologQueryAbstract {

	private CiaoPrologChangeWorkingFolderQuery(ProgramFileInfo programFileInfo) throws CiaoPrologQueryException, PathsMgmtException {
		super(programFileInfo);

		String programFileFolderName = programFileInfo.getProgramFileFolderFullPath();

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

	public static CiaoPrologChangeWorkingFolderQuery getInstance(ProgramFileInfo programFileInfo) throws CiaoPrologQueryException,
			PathsMgmtException {
		return new CiaoPrologChangeWorkingFolderQuery(programFileInfo);
	}

}
