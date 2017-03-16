package managers;

import programAnalysis.ProgramAnalysis;
import programAnalysis.ProgramPartAnalysis;
import auxiliar.LocalUserInfo;
import auxiliar.NextStep;
import constants.KConstants;
import constants.KUrls;
import filesAndPaths.ProgramFileInfo;

public class FuzzificationsManager extends AbstractManager {


	public FuzzificationsManager() {
		super();
	}
	

	   
	@Override
	public String methodToInvokeIfMethodRequestedIsNotAvailable() {
		return "list";
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void list() throws Exception {
		ProgramFileInfo programFileInfo = requestStoreHouse.getProgramFileInfo();
		LocalUserInfo localUserInfo = requestStoreHouse.getSession().getLocalUserInfo();
		ProgramAnalysis programAnalized = ProgramAnalysis.getProgramAnalysisClass(programFileInfo);

		String mode = requestStoreHouse.getRequestParameter(KConstants.Request.mode);
		ProgramPartAnalysis[][] programPartAnalysis = programAnalized.getProgramFuzzifications(localUserInfo, "", "", mode);
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
		String mode = requestStoreHouse.getRequestParameter(KConstants.Request.mode);

		ProgramAnalysis programAnalized = ProgramAnalysis.getProgramAnalysisClass(programFileInfo);
		ProgramPartAnalysis[][] programPartAnalysis = programAnalized.getProgramFuzzifications(localUserInfo, predDefined, predNecessary,
				mode);
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
		ProgramAnalysis programAnalized = ProgramAnalysis.getProgramAnalysisClass(programFileInfo);
        //preDefined = expensive(house) - predNecessary price(house) mode advanced - functionDefinition table
		
		
		String predDefined = requestStoreHouse.getRequestParameter(KConstants.Fuzzifications.predDefined);
		String predNecessary = requestStoreHouse.getRequestParameter(KConstants.Fuzzifications.predNecessary);
		String mode = requestStoreHouse.getRequestParameter(KConstants.Request.mode);
		
		String[][] functionDefinition = programAnalized.getFunctionDefinition(requestStoreHouse);
		
		
		int result = programAnalized.updateProgramFile(localUserInfo, predDefined, predNecessary, mode, functionDefinition);

		String msg = "Program file " + programFileInfo.getFileName() + " owned by " + programFileInfo.getFileOwner()
				+ " has NOT been updated. ";
		if (result == 0) {
			msg = "Program file " + programFileInfo.getFileName() + " owned by " + programFileInfo.getFileOwner() + " has been updated. ";
		}

		resultsStoreHouse.addResultMessage(msg);
		
		//getting the data
		programAnalized = ProgramAnalysis.getProgramAnalysisClass(programFileInfo);
		
		//analyzing the data
		String[][] resul = FuzzificationsAlgorithms.algo(programAnalized.getAllDefinedFunctionDefinition(predDefined));
		//setting the data
		mode = KConstants.Request.modeEditingDefault;
		programAnalized.updateProgramFile(localUserInfo, predDefined, predNecessary, mode, resul);
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Fuzzifications.SavePage, ""));
		
	}
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

}
