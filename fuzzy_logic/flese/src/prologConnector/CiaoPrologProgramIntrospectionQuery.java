package prologConnector;

import java.util.Iterator;

import storeHouse.CacheStoreHouse;
import storeHouse.CacheStoreHouseException;
import CiaoJava.PLStructure;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;
import filesAndPaths.PathsMgmtException;
import filesAndPaths.ProgramFileInfo;

public class CiaoPrologProgramIntrospectionQuery extends CiaoPrologQuery {

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

		String[] variablesNames = { "predicateType", "predicateName", "predicateArity", "predicateType" };

		setRealQuery(query, variables, variablesNames);

		isProgramIntrospectionQuery = true;
	}

	public static CiaoPrologProgramIntrospectionQuery getInstance(ProgramFileInfo fileInfo) throws CacheStoreHouseException, PathsMgmtException,
			CiaoPrologQueryException, PlConnectionEnvelopeException, AnswerTermInJavaClassException {
		String fullPath = fileInfo.getProgramFileFullPath();
		String key = CiaoPrologProgramIntrospectionQuery.class.getName();

		Object o = CacheStoreHouse.retrieve(CiaoPrologProgramIntrospectionQuery.class, fullPath, key, key);
		CiaoPrologProgramIntrospectionQuery query = (CiaoPrologProgramIntrospectionQuery) o;
		if (query == null) {
			query = new CiaoPrologProgramIntrospectionQuery(fileInfo);
			PlConnectionsPool.launchQuery(query);
			CacheStoreHouse.store(CiaoPrologProgramIntrospectionQuery.class, fullPath, key, key, query);
		}
		return query;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public AnswerTermInJavaClass[] getPredicateInfo(String predicateName) {
		Iterator<AnswerTermInJavaClass[]> iterator = null;
		if (queryAnswers != null)
			iterator = queryAnswers.iterator();

		if ((predicateName == null) || ("".equals(predicateName))) {
			LOG.info("Predicate Name is not valid. predicateName: " + predicateName);
		}
		if (iterator == null) {
			LOG.error("Iterator of Program Introspection is NULL!! ");
		}

		AnswerTermInJavaClass[] answer = null;
		if ((predicateName != null) && (iterator != null)) {
			while ((iterator.hasNext()) && (answer == null)) {
				answer = iterator.next();
				if (!predicateName.equals(answer[0].toString()))
					answer = null;
			}
		}
		return answer;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public String[] getProgramIntrospectionInJS() {
		if (queryAnswers == null)
			return null;
		Iterator<AnswerTermInJavaClass[]> queryAnswersIterator = queryAnswers.iterator();

		if (queryAnswersIterator == null)
			return null;

		String[] result = new String[queryAnswers.size()];

		int counter = 0;
		String tmp = null;
		AnswerTermInJavaClass[] predInfo;
		while (queryAnswersIterator.hasNext()) {
			predInfo = queryAnswersIterator.next();
			tmp = "";
			tmp += "addToProgramIntrospection(" + counter + ", new predInfo(";
			for (int i = 0; i < predInfo.length; i++) {
				tmp += predInfo[i].toJavaScript();
				if (i + 1 < predInfo.length)
					tmp += ",";
			}
			tmp += "));";
			result[counter] = tmp;
			counter++;
		}
		return result;
	}
}
