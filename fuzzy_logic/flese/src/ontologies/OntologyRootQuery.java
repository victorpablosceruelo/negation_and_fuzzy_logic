package ontologies;

public class OntologyRootQuery extends AbstractOntologyQuery {

	public OntologyRootQuery() {
		// TODO Auto-generated constructor stub
	}

	
	public void query() {
		//Create the Parameterized String
		SparqlParameterizedString queryString = new SparqlParameterizedString();
		queryString.Namespaces.AddNamespace("ex", new Uri("http://example.org/ns#"));
		queryString.CommandText = "SELECT * WHERE { ?s ex:property @value }";

		//Inject a Value for the parameter
		queryString.SetUri("value", new Uri("http://example.org/value"));

		//When we call ToString() we get the full command text with namespaces appended as PREFIX
		//declarations and any parameters replaced with their declared values
		Console.WriteLine(queryString.ToString());
	}
}
