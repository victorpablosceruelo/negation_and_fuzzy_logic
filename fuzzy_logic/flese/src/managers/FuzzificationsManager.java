package managers;

import storeHouse.CacheStoreHouseCleaner;
import auxiliar.NextStep;
import auxiliar.ProgramAnalysisClass;
import constants.KConstants;
import constants.KUrls;
import filesAndPaths.ProgramFileInfo;

public class FuzzificationsManager extends AbstractManager {

	public FuzzificationsManager() {
		super();
	}

	@Override
	public NextStep getExceptionPage() {
		NextStep nextStep = new NextStep(KConstants.NextStep.forward_to, KUrls.Pages.Exception, "");
		return nextStep;
	}

	@Override
	public NextStep byDefaultMethod() throws Exception {
		return list();
	}

	@Override
	public boolean createSessionIfNull() {
		return false;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public NextStep list() throws Exception {

		ProgramFileInfo programFileInfo = requestStoreHouse.getProgramFileInfo();

		// String filePath = FilesManagerAux.getFullPath(programFilesPath,
		// fileOwner, fileName, false);
		// request.setAttribute("filePath", filePath);

		NextStep nextStep = new NextStep(KConstants.NextStep.forward_to, KUrls.Fuzzifications.ListPage, "");
		return nextStep;

	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public NextStep saveProgramFuzzification() throws Exception {

		ProgramFileInfo programFileInfo = requestStoreHouse.getProgramFileInfo();

		// String filePath = FilesManagerAux.getFullPath(programFilesPath,
		// fileOwner, fileName, false);
		// request.setAttribute("filePath", filePath);

		String predDefined = requestStoreHouse.getRequestParameter("predDefined");
		if (predDefined == null)
			throw new Exception("predDefined is null.");

		String predNecessary = requestStoreHouse.getRequestParameter("predNecessary");
		if (predNecessary == null)
			throw new Exception("predNecessary is null.");

		String predOwner = requestStoreHouse.getRequestParameter("predOwner");
		if (predOwner == null)
			throw new Exception("predOwner is null.");

		int counter = 0;
		String[][] params = null;
		String paramsDebug = "Function definition to save: ";

		while ((requestStoreHouse.getRequestParameter("fpx[" + counter + "]") != null)
				&& (requestStoreHouse.getRequestParameter("fpy[" + counter + "]") != null)) {
			counter++;
		}

		if (counter > 0) {
			params = new String[counter][2];
			for (int i = 0; i < counter; i++) {
				params[i][0] = requestStoreHouse.getRequestParameter("fpx[" + i + "]");
				params[i][1] = requestStoreHouse.getRequestParameter("fpy[" + i + "]");
				paramsDebug += "\n" + params[i][0] + " -> " + params[i][1] + " ";
			}
		}

		ProgramAnalysisClass programAnalized = new ProgramAnalysisClass(requestStoreHouse);
		programAnalized.updateProgramFile(predDefined, predNecessary, predOwner, params);

		CacheStoreHouseCleaner.clean(requestStoreHouse);

		/*
		 * This is just to test if the send button produces errors. int j = 0;
		 * while (true) { j++; }
		 */

		NextStep nextStep = new NextStep(KConstants.NextStep.forward_to, KUrls.Fuzzifications.SavePage, "");
		return nextStep;

	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
