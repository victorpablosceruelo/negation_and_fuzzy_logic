package prologConnector;

import constants.KConstants;
import storeHouse.CacheStoreHouse;
import storeHouse.CacheStoreHouseException;
import CiaoJava.PLStructure;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;
import filesAndPaths.FilesAndPathsException;
import filesAndPaths.ProgramFileInfo;

public class CiaoPrologProgramIntrospectionQuery extends CiaoPrologQueryAbstract {

	ProgramIntrospection programIntrospection = null;
	
	private CiaoPrologProgramIntrospectionQuery(ProgramFileInfo fileInfo) throws CiaoPrologConnectorException, FilesAndPathsException {
		super(fileInfo);

		// Prepare the query structure.
		// rfuzzy_introspection(PClass, PName, PArity, PType).
		PLVariable[] variables = new PLVariable[4];
		variables[0] = new PLVariable(); // predicateType
		variables[1] = new PLVariable(); // predicateName
		variables[2] = new PLVariable(); // predicateArity
		variables[3] = new PLVariable(); // predicateType
		PLTerm[] args = { variables[0], variables[1], variables[2], variables[3] };
		PLStructure query = new PLStructure("rfuzzy_introspection", args);

		String[] variablesNames = { KConstants.ProgramIntrospectionFields.predicateTypes, KConstants.ProgramIntrospectionFields.predicateName, KConstants.ProgramIntrospectionFields.predicateArity, KConstants.ProgramIntrospectionFields.predicateMoreInfo };

		setRealQuery(query, variables, variablesNames);

		isProgramIntrospectionQuery = true;
	}

	public static CiaoPrologProgramIntrospectionQuery getInstance(ProgramFileInfo programFileInfo) throws CacheStoreHouseException,
			FilesAndPathsException, CiaoPrologConnectorException, PlConnectionEnvelopeException {
		String fullPath = programFileInfo.getProgramFileFullPath();
		String key = CiaoPrologProgramIntrospectionQuery.class.getName();

		Object o = CacheStoreHouse.retrieve(CiaoPrologProgramIntrospectionQuery.class, fullPath, key, key);
		CiaoPrologProgramIntrospectionQuery query = (CiaoPrologProgramIntrospectionQuery) o;
		if (query == null) {
			query = new CiaoPrologProgramIntrospectionQuery(programFileInfo);
			PlConnectionsPool.launchQuery(query);
			CacheStoreHouse.store(CiaoPrologProgramIntrospectionQuery.class, fullPath, key, key, query);
		}
		return query;
	}

	public static void clearCacheInstancesFor(ProgramFileInfo programFileInfo) throws FilesAndPathsException, CacheStoreHouseException {
		String fullPath = programFileInfo.getProgramFileFullPath();
		String key = CiaoPrologProgramIntrospectionQuery.class.getName();
		CacheStoreHouse.store(CiaoPrologProgramIntrospectionQuery.class, fullPath, key, key, null);
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	
	public ProgramIntrospection getProgramIntrospection () {
		if (programIntrospection != null) return programIntrospection;
		
		programIntrospection = new ProgramIntrospection();
		CiaoPrologQueryAnswer answer = null;
		for (int i=0; i<queryAnswers.size(); i++) {
			answer = queryAnswers.get(i);
			programIntrospection.addAnswerInfo(answer);
		}
		return programIntrospection;
	}
	
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
