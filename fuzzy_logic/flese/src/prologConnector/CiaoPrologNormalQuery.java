package prologConnector;

import storeHouse.CacheStoreHouse;
import storeHouse.CacheStoreHouseException;
import storeHouse.RequestStoreHouse;
import storeHouse.RequestStoreHouseException;
import auxiliar.LocalUserInfoException;
import conversors.ConversorToPrologQuery;
import conversors.QueryConversorException;
import filesAndPaths.FilesAndPathsException;
import filesAndPaths.ProgramFileInfo;

public class CiaoPrologNormalQuery extends CiaoPrologQueryAbstract {

	public boolean isOfType(String type) {
		return CiaoPrologQueryAbstract.Constants.NormalQuery.equals(type);
	}
	
	private CiaoPrologNormalQuery(ProgramFileInfo programFileInfo) throws CiaoPrologConnectorException {
		super(programFileInfo);
	}

	public static CiaoPrologNormalQuery getInstance(RequestStoreHouse requestStoreHouse) throws CacheStoreHouseException,
			FilesAndPathsException, CiaoPrologConnectorException, PlConnectionEnvelopeException, FilesAndPathsException,
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
			PlConnectionEnvelope plConnectionEnvelope = new PlConnectionEnvelope();
			plConnectionEnvelope.runPrologQuery(query);
			CacheStoreHouse.store(CiaoPrologNormalQuery.class, fullPath, key1, key2, query);
		}
		return query;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public static void clearCacheInstancesFor(ProgramFileInfo programFileInfo) throws FilesAndPathsException, CacheStoreHouseException,
			FilesAndPathsException, LocalUserInfoException {
		String fullPath = programFileInfo.getProgramFileFullPath();
		// String key1 =
		// requestStoreHouse.session.getLocalUserInfo().getLocalUserName();
		String key1 = null;
		CacheStoreHouse.store(CiaoPrologNormalQuery.class, fullPath, key1, null, null);
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
