package managers;

import java.util.ArrayList;
import java.util.HashMap;

import com.hp.hpl.jena.rdf.model.RDFNode;

import ontologies.InterfaceOntologyQuery;
import ontologies.OntologyInstancesQuery;
import ontologies.OntologyQueryArgument;
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
		
		String serviceEndPoint = requestStoreHouse.getRequestParameter(KConstants.JspsDivsIds.ontologyUrlFieldId);
		
		
		InterfaceOntologyQuery classesQuery = OntologyRootQuery.getInstance();
		classesQuery.setServiceEndPoint(serviceEndPoint);
		
		classesQuery.setQueryArguments(null);
		classesQuery.query();
		// ArrayList<HashMap<String, RDFNode>> classesQueryResults = classesQuery.getResults();
		
		ArrayList<String [][][]> allResults = new ArrayList<String [][][]>();
		allResults.add(classesQuery.getResultsAsStrings());
		
		resultsStoreHouse.setOntologyQueryResults(allResults);
		
		// Forward to the jsp page.
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Ontologies.MainQueryPage, ""));
	}
	
	public void subQuery() throws Exception {
		
		String serviceEndPoint = requestStoreHouse.getRequestParameter(KConstants.JspsDivsIds.ontologyUrlFieldId);
		
		ArrayList<String [][][]> allResults = new ArrayList<String [][][]>();
		
			HashMap<String, RDFNode> args = new HashMap<String, RDFNode>();
			RDFNode value = new lit 
					
			args.put(OntologyInstancesQuery.nameArg1, value);
			
			InterfaceOntologyQuery instancesQuery = OntologyInstancesQuery.getInstance();
			instancesQuery.setQueryArguments(args);
			instancesQuery.query();
			String [][][] instancesQueryResults = instancesQuery.getResultsAsStrings();
			allResults.add(instancesQueryResults);
			}
			i++;
		}
		
		resultsStoreHouse.setOntologyQueryResults(allResults);
		
		// Forward to the jsp page.
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Ontologies.MainQueryPage, ""));
	}
	
	public void test() throws Exception {
		String serviceEndPoint = requestStoreHouse.getRequestParameter(KConstants.JspsDivsIds.ontologyUrlFieldId);
		
		
		InterfaceOntologyQuery classesQuery = OntologyRootQuery.getInstance();
		classesQuery.setServiceEndPoint(serviceEndPoint);
		
		classesQuery.setQueryArguments(null);
		classesQuery.query();
		ArrayList<HashMap<String, RDFNode>> classesQueryResults = classesQuery.getResults();
		
		ArrayList<String [][][]> allResults = new ArrayList<String [][][]>();
		allResults.add(classesQuery.getResultsAsStrings());
		
		int i = 0;
		for (HashMap<String, RDFNode> classesQueryResult : classesQueryResults) {
			if (i < 1) {
			HashMap<String, OntologyQueryArgument> args = new HashMap<String, OntologyQueryArgument>();
			RDFNode value = classesQueryResult.get(OntologyRootQuery.nameVar1);
					
			args.put(OntologyInstancesQuery.nameArg1, new OntologyQueryArgument(value));
			
			InterfaceOntologyQuery instancesQuery = OntologyInstancesQuery.getInstance();
			instancesQuery.setQueryArguments(args);
			instancesQuery.query();
			String [][][] instancesQueryResults = instancesQuery.getResultsAsStrings();
			allResults.add(instancesQueryResults);
			}
			i++;
		}
		
		resultsStoreHouse.setOntologyQueryResults(allResults);
		
		// Forward to the jsp page.
		setNextStep(new NextStep(KConstants.NextStep.forward_to, KUrls.Ontologies.MainQueryPage, ""));
	}
}
