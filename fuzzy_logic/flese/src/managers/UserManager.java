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
	public void byDefaultMethod() throws Exception {
		options();
	}

	@Override
	public boolean createSessionIfNull() {
		return false;
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void options() throws Exception {
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.User.OptionsPage, ""));
	}

}
