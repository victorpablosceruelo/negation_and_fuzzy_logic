package results;

import storeHouse.RequestStoreHouse;
import filesAndPaths.ProgramFileInfo;

public class ResultsStoreHouseUtils {

	public static void addMessage(RequestStoreHouse requestStoreHouse, String msg) {

		ResultsStoreHouse resultsStoreHouse = requestStoreHouse.getResultsStoreHouse();
		resultsStoreHouse.addMessage(msg);
		requestStoreHouse.setResultsStoreHouse(resultsStoreHouse);
	}

	public static void updateFilesList(RequestStoreHouse requestStoreHouse, ProgramFileInfo[] filesList) {

		ResultsStoreHouse resultsStoreHouse = requestStoreHouse.getResultsStoreHouse();
		resultsStoreHouse.setFilesList(filesList);
		requestStoreHouse.setResultsStoreHouse(resultsStoreHouse);
	}

}
