package managers;

import constants.KConstants;
import constants.KUrls;
import filesAndPaths.ProgramFileInfo;
import auxiliar.NextStep;
import auxiliar.ProgramAnalysisClass;
import auxiliar.ServletsAuxMethodsClass;

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
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean createSessionIfNull() {
		return false;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void list() throws Exception {

		ProgramFileInfo[] filesList = FilesManagerAux.list(requestStoreHouse);

		String filePath = FilesManagerAux.getFullPath(programFilesPath, fileOwner, fileName, false);
		request.setAttribute("filePath", filePath);

		// Forward to the jsp page.
		ServletsAuxMethodsClass.forward_to(KConstants.Pages.ListProgramFuzzificationsAnswer, "", request, response, LOG);
	}
	
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void saveProgramFuzzification() throws Exception {

		testAndInitialize_fileName_and_fileOwner();

		String filePath = FilesManagerAux.getFullPath(programFilesPath, fileOwner, fileName, false);
		request.setAttribute("filePath", filePath);

		String predDefined = request.getParameter("predDefined");
		if (predDefined == null)
			throw new Exception("predDefined is null.");

		String predNecessary = request.getParameter("predNecessary");
		if (predNecessary == null)
			throw new Exception("predNecessary is null.");

		String predOwner = request.getParameter("predOwner");
		if (predOwner == null)
			throw new Exception("predOwner is null.");

		int counter = 0;
		String[][] params = null;
		String paramsDebug = "Function definition to save: ";

		while ((request.getParameter("fpx[" + counter + "]") != null) && (request.getParameter("fpy[" + counter + "]") != null)) {
			counter++;
		}

		if (counter > 0) {
			params = new String[counter][2];
			for (int i = 0; i < counter; i++) {
				params[i][0] = request.getParameter("fpx[" + i + "]");
				params[i][1] = request.getParameter("fpy[" + i + "]");
				paramsDebug += "\n" + params[i][0] + " -> " + params[i][1] + " ";
			}
		}

		LOG.info(paramsDebug);

		ProgramAnalysisClass programAnalized = new ProgramAnalysisClass(localUserName.getLocalUserName(), fileName, fileOwner, filePath);
		programAnalized.updateProgramFile(predDefined, predNecessary, predOwner, params);

		connection.clearCacheInCiaoPrologConnectionClass();

		/*
		 * This is just to test if the send button produces errors. int j = 0;
		 * while (true) { j++; }
		 */

		// Forward to the jsp page.
		ServletsAuxMethodsClass.forward_to(KConstants.Pages.SaveProgramFuzzificationAnswer, "", request, response, LOG);

	}
	
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
