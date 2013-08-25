package prologConnector;

import CiaoJava.PLStructure;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;
import auxiliar.LocalUserInfoException;
import filesAndPaths.PathsMgmtException;

public class CiaoPrologProgramIntrospectionQuery extends CiaoPrologQuery {

	public CiaoPrologProgramIntrospectionQuery(String fileOwner, String fileName) throws CiaoPrologQueryException, PathsMgmtException,
			LocalUserInfoException {
		super(fileOwner, fileName);

		// Prepare the query structure.
		// rfuzzy_introspection(PClass, PName, PArity, PType).
		PLVariable[] variables = new PLVariable[4];
		variables[0] = new PLVariable(); // predicateType
		variables[1] = new PLVariable(); // predicateName
		variables[2] = new PLVariable(); // predicateArity
		variables[3] = new PLVariable(); // predicateType
		PLTerm[] args = { variables[0], variables[1], variables[2], variables[3] };
		PLStructure query = new PLStructure("rfuzzy_introspection", args);

		String[] variablesNames = { "predicateType", "predicateName", "predicateArity", "predicateType" };

		setRealQuery(query, variables, variablesNames);

		isProgramIntrospectionQuery = true;
	}

}
