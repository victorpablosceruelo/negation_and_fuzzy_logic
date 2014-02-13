package ontologies;

public interface InterfaceOntologyQuery {

	public abstract void query(String serviceEndPoint);
	
	public abstract void setArguments(String [] [] args);
	
	public abstract String [] getResults();
	
}
