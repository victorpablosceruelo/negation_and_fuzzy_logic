package managers;

import results.ResultsStoreHouseUtils;
import storeHouse.CacheStoreHouseCleaner;
import auxiliar.NextStep;
import auxiliar.ProgramAnalysisClass;
import constants.KConstants;
import constants.KUrls;

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
	public void byDefaultMethod() throws Exception {
		list();
	}

	@Override
	public boolean createSessionIfNull() {
		return false;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void list() throws Exception {

		ProgramAnalysisClass programAnalized = new ProgramAnalysisClass(requestStoreHouse);
		String[] fuzzificationsList = programAnalized.getProgramFuzzificationsInJS();
		ResultsStoreHouseUtils.updateFuzzificationsList(requestStoreHouse, fuzzificationsList);

		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Fuzzifications.ListPage, ""));

	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void saveProgramFuzzification() throws Exception {

		ProgramAnalysisClass programAnalized = new ProgramAnalysisClass(requestStoreHouse);
		programAnalized.updateProgramFile();

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
