package prologConnector;

import storeHouse.CacheStoreHouse;
import storeHouse.CacheStoreHouseException;
import CiaoJava.PLStructure;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;
import filesAndPaths.PathsMgmtException;
import filesAndPaths.ProgramFileInfo;

public class CiaoPrologProgramIntrospectionQuery extends CiaoPrologQueryAbstract {

	public static class Constants {
		public static final String predicateType = "predicateType";
		public static final String predicateName = "predicateName";
		public static final String predicateArity = "predicateArity";
		public static final String predicateTypes = "predicateTypes";
	}

	private CiaoPrologProgramIntrospectionQuery(ProgramFileInfo fileInfo) throws CiaoPrologQueryException, PathsMgmtException {
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

		String[] variablesNames = { Constants.predicateType, Constants.predicateName, Constants.predicateArity, Constants.predicateTypes };

		setRealQuery(query, variables, variablesNames);

		isProgramIntrospectionQuery = true;
	}

	public static CiaoPrologProgramIntrospectionQuery getInstance(ProgramFileInfo programFileInfo) throws CacheStoreHouseException,
			PathsMgmtException, CiaoPrologQueryException, PlConnectionEnvelopeException, CiaoPrologTermInJavaException, CiaoPrologQueryAnswerException {
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

	public static void clearCacheInstancesFor(ProgramFileInfo programFileInfo) throws PathsMgmtException, CacheStoreHouseException {
		String fullPath = programFileInfo.getProgramFileFullPath();
		String key = CiaoPrologProgramIntrospectionQuery.class.getName();
		CacheStoreHouse.store(CiaoPrologProgramIntrospectionQuery.class, fullPath, key, key, null);
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public CiaoPrologQueryAnswer getPredicateInfo(String predicateName) throws CiaoPrologQueryException, CiaoPrologQueryAnswerException {
		if (predicateName == null) {
			throw new CiaoPrologQueryException("predicateName cannot be null.");
		}
		if ("".equals(predicateName)) {
			throw new CiaoPrologQueryException("predicateName cannot be empty string.");
		}

		CiaoPrologQueryAnswer answer = null;
		int i = 0;
		boolean found = false;
		while (i < queryAnswers.size() && (!found)) {
			answer = queryAnswers.get(i);
			CiaoPrologTermInJava term = answer.getCiaoPrologQueryVariableAnswer(Constants.predicateName);
			String currentDefPredicateName = term.toString();
			if (predicateName.equals(currentDefPredicateName))
				found = true;
			else
				i++;
		}
		return answer;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
