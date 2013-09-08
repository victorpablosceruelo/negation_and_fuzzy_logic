package storeHouse;

import prologConnector.CiaoPrologNormalQuery;
import prologConnector.CiaoPrologProgramIntrospectionQuery;
import auxiliar.LocalUserInfoException;
import filesAndPaths.FilesAndPathsException;
import filesAndPaths.FilesAndPathsException;
import filesAndPaths.ProgramFileInfo;

public class CacheStoreHouseCleaner {

	public static void clean(RequestStoreHouse requestStoreHouse) throws FilesAndPathsException, CacheStoreHouseException, FilesAndPathsException,
			LocalUserInfoException {

		ProgramFileInfo programFileInfo = requestStoreHouse.getProgramFileInfo();
		
		CiaoPrologProgramIntrospectionQuery.clearCacheInstancesFor(programFileInfo);
		CiaoPrologNormalQuery.clearCacheInstancesFor(programFileInfo);

	}

}
