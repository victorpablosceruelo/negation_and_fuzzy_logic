package ontologies;

public interface InterfaceOntologyQuery {
	
	public abstract void setServiceEndPoint(String serviceEndPoint);
	
	public abstract void setQueryString(String queryString);
	
	public abstract String getQueryString(boolean argumentsSubstituted);
	
	public String [] getVariablesNames();
	
	public String [] getArgumentsNames();
	
	public abstract void setQueryArguments(String[][] args);
	
	public abstract void query();
	
	public abstract String [] [] getResults();
	
}
