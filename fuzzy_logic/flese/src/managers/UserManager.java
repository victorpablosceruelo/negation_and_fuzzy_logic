package managers;

import constants.KConstants;
import constants.KUrls;
import auxiliar.NextStep;

public class UserManager extends AbstractManager {

	public UserManager() {
		super();
	}

	@Override
	public String methodToInvokeIfMethodRequestedIsNotAvailable() {
		return "options";
	}
	
	@Override
	public boolean reinitializeResultsStoreHouse(String op) {
		return true;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void options() throws Exception {
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.User.OptionsPage, ""));
	}

}
