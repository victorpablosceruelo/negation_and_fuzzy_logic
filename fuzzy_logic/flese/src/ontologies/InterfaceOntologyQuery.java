package ontologies;

import java.util.ArrayList;
import java.util.HashMap;

import com.hp.hpl.jena.rdf.model.RDFNode;

public interface InterfaceOntologyQuery {
	
	public abstract void setServiceEndPoint(String serviceEndPoint);
	
	public abstract void setQueryString(String queryString);
	
	public abstract String getQueryString(boolean argumentsSubstituted);
	
	public abstract String [] getVariablesNames();
	
	public abstract String [] getArgumentsNames();
	
	public abstract void setQueryArguments(HashMap<String, RDFNode> args);
	
	public abstract void query();
	
	public abstract ArrayList<HashMap<String, RDFNode>> getResults();
	
	public abstract String[][][] getResultsAsStrings();
	
}
