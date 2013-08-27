package prologConnector;

import conversors.ConversorToPrologQuery;
import conversors.QueryConversorException;
import storeHouse.CacheStoreHouse;
import storeHouse.CacheStoreHouseException;
import storeHouse.SessionStoreHouse;
import filesAndPaths.FileInfoException;
import filesAndPaths.PathsMgmtException;
import filesAndPaths.ProgramFileInfo;

public class CiaoPrologNormalQuery extends CiaoPrologQuery {

	private CiaoPrologNormalQuery(ProgramFileInfo programFileInfo) throws CiaoPrologQueryException {
		super(programFileInfo);		
	}

	public static CiaoPrologNormalQuery getInstance(SessionStoreHouse sessionStoreHouse) throws CacheStoreHouseException, PathsMgmtException,
			CiaoPrologQueryException, PlConnectionEnvelopeException, AnswerTermInJavaClassException, FileInfoException, QueryConversorException {
		
		String fullPath = sessionStoreHouse.getProgramFileInfo().getProgramFileFullPath();
		ConversorToPrologQuery conversor = new ConversorToPrologQuery(sessionStoreHouse);

		String key1 = sessionStoreHouse.getLocalUserInfo().getLocalUserName();
		String key2 = conversor.getQueryComplexInfoString(); 

		Object o = CacheStoreHouse.retrieve(CiaoPrologNormalQuery.class, fullPath, key1, key2);
		CiaoPrologNormalQuery query = (CiaoPrologNormalQuery) o;
		if (query == null) {
			query = new CiaoPrologNormalQuery(sessionStoreHouse.getProgramFileInfo());
			query.setRealQuery(conversor.getConvertedQuery(), conversor.getListOfVariables(), conversor.getListOfNamesForVariables());
			PlConnectionsPool.launchQuery(query);
			CacheStoreHouse.store(CiaoPrologNormalQuery.class, fullPath, key1, key2, query);
		}
		return query;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	
	
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
