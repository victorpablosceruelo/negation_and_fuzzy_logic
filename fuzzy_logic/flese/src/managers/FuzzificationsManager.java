package managers;

import storeHouse.CacheStoreHouseCleaner;
import auxiliar.LocalUserInfo;
import auxiliar.NextStep;
import auxiliar.ProgramAnalysisClass;
import auxiliar.ProgramPartAnalysis;
import constants.KConstants;
import constants.KUrls;
import filesAndPaths.ProgramFileInfo;

public class FuzzificationsManager extends AbstractManager {

	public FuzzificationsManager() {
		super();
	}

	@Override
	public void byDefaultMethod() throws Exception {
		list();
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void list() throws Exception {
		ProgramFileInfo programFileInfo = requestStoreHouse.getProgramFileInfo();
		LocalUserInfo localUserInfo = requestStoreHouse.getSession().getLocalUserInfo();
		ProgramAnalysisClass programAnalized = ProgramAnalysisClass.getProgramAnalysisClass(programFileInfo);
		ProgramPartAnalysis [][] programPartAnalysis = programAnalized.getProgramFuzzifications(localUserInfo, "", "");
		resultsStoreHouse.setProgramFileInfo(programFileInfo);
		resultsStoreHouse.setProgramPartAnalysis(programPartAnalysis);

		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Fuzzifications.ListPage, ""));

	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void edit() throws Exception {
		ProgramFileInfo programFileInfo = requestStoreHouse.getProgramFileInfo();
		LocalUserInfo localUserInfo = requestStoreHouse.getSession().getLocalUserInfo();
		String predDefined = requestStoreHouse.getRequestParameter(KConstants.Fuzzifications.predDefined);
		String predNecessary = requestStoreHouse.getRequestParameter(KConstants.Fuzzifications.predNecessary);
		
		ProgramAnalysisClass programAnalized = ProgramAnalysisClass.getProgramAnalysisClass(programFileInfo);
		ProgramPartAnalysis [][] programPartAnalysis = programAnalized.getProgramFuzzifications(localUserInfo, predDefined, predNecessary);
		resultsStoreHouse.setProgramFileInfo(programFileInfo);
		resultsStoreHouse.setProgramPartAnalysis(programPartAnalysis);

		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Fuzzifications.EditPage, ""));

	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void save() throws Exception {
		ProgramFileInfo programFileInfo = requestStoreHouse.getProgramFileInfo();
		LocalUserInfo localUserInfo = requestStoreHouse.getSession().getLocalUserInfo();
		ProgramAnalysisClass programAnalized = ProgramAnalysisClass.getProgramAnalysisClass(programFileInfo);

		String predDefined = requestStoreHouse.getRequestParameter(KConstants.Fuzzifications.predDefined);
		String predNecessary = requestStoreHouse.getRequestParameter(KConstants.Fuzzifications.predNecessary);
		String predOwner = requestStoreHouse.getRequestParameter(KConstants.Fuzzifications.predOwner);
		String[][] functionDefinition = programAnalized.getFunctionDefinition(requestStoreHouse);

		programAnalized.updateProgramFile(localUserInfo, predDefined, predNecessary, predOwner, functionDefinition);

		CacheStoreHouseCleaner.clean(requestStoreHouse);

		/*
		 * This is just to test if the send button produces errors. int j = 0;
		 * while (true) { j++; }
		 */

		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Fuzzifications.SavePage, ""));

	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
