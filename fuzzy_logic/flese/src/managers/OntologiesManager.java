package managers;

import auxiliar.NextStep;
import constants.KConstants;
import constants.KUrls;

public class OntologiesManager extends AbstractManager {

	public OntologiesManager() {
	}

	@Override
	public String methodToInvokeIfMethodRequestedIsNotAvailable() {
		return "start";
	}

	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////

	public void start() throws Exception {
		// Forward to the jsp page.
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Ontologies.StartPage, ""));
	}

	public void main() throws Exception {
		// Forward to the jsp page.
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Ontologies.MainPage, ""));
	}

	public void mainQuery() throws Exception {
		// Forward to the jsp page.
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Ontologies.MainQueryPage, ""));
	}

}
