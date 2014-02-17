package managers;

import ontologies.InterfaceOntologyQuery;
import ontologies.OntologyInstancesQuery;
import ontologies.OntologyRootQuery;
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
		InterfaceOntologyQuery classesQuery = OntologyRootQuery.getInstance();
		
		classesQuery.setQueryArguments(new String[0][]);
		classesQuery.query();
		String [][] results = classesQuery.getResults();
		
		for (String [] result : results) {
			String [][] args = new String[1][2];
			args[0][0] = OntologyInstancesQuery.nameArg1;
			args[0][1] = result[0].toString();
			
			InterfaceOntologyQuery instancesQuery = OntologyInstancesQuery.getInstance();
			instancesQuery.setQueryArguments(args);
			instancesQuery.query();
		}
		
		// Forward to the jsp page.
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Ontologies.MainQueryPage, ""));
	}

}
