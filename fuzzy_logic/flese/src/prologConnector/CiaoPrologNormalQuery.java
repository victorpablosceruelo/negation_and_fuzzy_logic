package prologConnector;

import storeHouse.CacheStoreHouse;
import storeHouse.CacheStoreHouseException;
import storeHouse.RequestStoreHouse;
import storeHouse.RequestStoreHouseException;
import auxiliar.LocalUserInfoException;
import conversors.ConversorToPrologQuery;
import conversors.QueryConversorException;
import filesAndPaths.FileInfoException;
import filesAndPaths.PathsMgmtException;
import filesAndPaths.ProgramFileInfo;

public class CiaoPrologNormalQuery extends CiaoPrologQuery {

	private CiaoPrologNormalQuery(ProgramFileInfo programFileInfo) throws CiaoPrologQueryException {
		super(programFileInfo);
	}

	public static CiaoPrologNormalQuery getInstance(RequestStoreHouse requestStoreHouse) throws CacheStoreHouseException,
			PathsMgmtException, CiaoPrologQueryException, PlConnectionEnvelopeException, CiaoPrologTermInJavaException, FileInfoException,
			QueryConversorException, LocalUserInfoException, RequestStoreHouseException {

		String fullPath = requestStoreHouse.getProgramFileInfo().getProgramFileFullPath();
		String key1 = requestStoreHouse.getSession().getLocalUserInfo().getLocalUserName();

		ConversorToPrologQuery conversor = new ConversorToPrologQuery(requestStoreHouse);
		String key2 = conversor.getQueryComplexInfoString();

		Object o = CacheStoreHouse.retrieve(CiaoPrologNormalQuery.class, fullPath, key1, key2);
		CiaoPrologNormalQuery query = (CiaoPrologNormalQuery) o;
		if (query == null) {
			query = new CiaoPrologNormalQuery(requestStoreHouse.getProgramFileInfo());
			query.setRealQuery(conversor.getConvertedQuery(), conversor.getListOfVariables(), conversor.getListOfNamesForVariables());
			PlConnectionsPool.launchQuery(query);
			CacheStoreHouse.store(CiaoPrologNormalQuery.class, fullPath, key1, key2, query);
		}
		return query;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public static void clearCacheInstance(RequestStoreHouse requestStoreHouse) throws PathsMgmtException, CacheStoreHouseException,
			FileInfoException, LocalUserInfoException {
		String fullPath = requestStoreHouse.getProgramFileInfo().getProgramFileFullPath();
		// String key1 = requestStoreHouse.session.getLocalUserInfo().getLocalUserName();
		String key1 = null;
		CacheStoreHouse.store(CiaoPrologNormalQuery.class, fullPath, key1, null, null);
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
