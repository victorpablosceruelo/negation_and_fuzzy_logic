package storeHouse;

import prologConnector.CiaoPrologNormalQuery;
import prologConnector.CiaoPrologProgramIntrospectionQuery;
import auxiliar.LocalUserInfoException;
import filesAndPaths.FileInfoException;
import filesAndPaths.PathsMgmtException;
import filesAndPaths.ProgramFileInfo;

public class CacheStoreHouseCleaner {

	public static void clean(RequestStoreHouse requestStoreHouse) throws PathsMgmtException, CacheStoreHouseException, FileInfoException,
			LocalUserInfoException {

		ProgramFileInfo programFileInfo = requestStoreHouse.getProgramFileInfo();
		
		CiaoPrologProgramIntrospectionQuery.clearCacheInstancesFor(programFileInfo);
		CiaoPrologNormalQuery.clearCacheInstancesFor(programFileInfo);

	}

}
