package managers;

import constants.KConstants;
import constants.KUrls;
import auxiliar.NextStep;

public class UserManager extends AbstractManager {

	public UserManager() {
		super();
	}

	@Override
	public NextStep getExceptionPage() {
		NextStep nextStep = new NextStep(KConstants.NextStep.forward_to, KUrls.Pages.Exception, "");
		return nextStep;
	}

	@Override
	public NextStep byDefaultMethod() throws Exception {
		return options();
	}

	@Override
	public boolean createSessionIfNull() {
		return false;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public NextStep options() throws Exception {
		NextStep nextStep = new NextStep(KConstants.NextStep.forward_to, KUrls.User.OptionsPage, "");
		return nextStep;
	}

}
