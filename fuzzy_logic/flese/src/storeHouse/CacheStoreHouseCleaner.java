package storeHouse;

import prologConnector.CiaoPrologNormalQuery;
import prologConnector.CiaoPrologProgramIntrospectionQuery;
import auxiliar.LocalUserInfoException;
import auxiliar.ProgramAnalysisClass;
import filesAndPaths.FilesAndPathsException;
import filesAndPaths.ProgramFileInfo;

public class CacheStoreHouseCleaner {

	public static void clean(ProgramFileInfo programFileInfo) throws FilesAndPathsException, CacheStoreHouseException,
			FilesAndPathsException, LocalUserInfoException {
		ProgramAnalysisClass.clearCacheInstancesFor(programFileInfo);
		CiaoPrologProgramIntrospectionQuery.clearCacheInstancesFor(programFileInfo);
		CiaoPrologNormalQuery.clearCacheInstancesFor(programFileInfo);

	}

}
