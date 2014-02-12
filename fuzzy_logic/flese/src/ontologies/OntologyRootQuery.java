package ontologies;

import com.hp.hpl.jena.query.Query;
import com.hp.hpl.jena.query.QueryExecution;
import com.hp.hpl.jena.query.QueryExecutionFactory;
import com.hp.hpl.jena.query.QueryFactory;
import com.hp.hpl.jena.query.ResultSet;
import com.hp.hpl.jena.query.ResultSetFormatter;

public class OntologyRootQuery extends AbstractOntologyQuery {

	private final String queryPrefixLine01 = "PREFIX owl: <http://www.w3.org/2002/07/owl#>";
	private final String queryPrefixLine02 = "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>";
	private final String queryPrefixLine03 = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>";
	private final String queryPrefixLine04 = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>";
	private final String queryPrefixLine05 = "PREFIX foaf: <http://xmlns.com/foaf/0.1/>";
	private final String queryPrefixLine06 = "PREFIX dc: <http://purl.org/dc/elements/1.1/>";
	private final String queryPrefixLine07 = "PREFIX : <http://dbpedia.org/resource/>";
	private final String queryPrefixLine08 = "PREFIX dbpedia2: <http://dbpedia.org/property/>";
	private final String queryPrefixLine09 = "PREFIX dbpedia: <http://dbpedia.org/>";
	private final String queryPrefixLine10 = "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>";

	private final String queryEndLine01 = "SELECT ?class ";
	private final String queryEndLine02 = "WHERE { ?class rdfs:subClassOf owl:Thing . ";
	private final String queryEndLine03 = "        FILTER ( ?class != owl:Thing && ?class != owl:Nothing ) . ";
	private final String queryEndLine04 = "        OPTIONAL { ?class rdfs:subClassOf ?super . ";
	private final String queryEndLine05 = "                   FILTER ( ?super != owl:Thing && ?super != ?class ) } . ";
	private final String queryEndLine06 = "        FILTER ( !bound(?super) ) } ";

	public OntologyRootQuery() {

	}

	final static String defaultServiceEndpoint = "http://dbpedia.org/sparql";

	private final String getQuery() {
		return queryPrefixLine01 + queryPrefixLine02 + queryPrefixLine03 + queryPrefixLine04 + queryPrefixLine05 + queryPrefixLine06
				+ queryPrefixLine07 + queryPrefixLine08 + queryPrefixLine09 + queryPrefixLine10 + queryEndLine01 + queryEndLine02
				+ queryEndLine03 + queryEndLine04 + queryEndLine05 + queryEndLine06;
	}

	public void query(String serviceEndPoint) {

		if ((serviceEndPoint == null) || ("".equals(serviceEndPoint))) {
			serviceEndPoint = defaultServiceEndpoint;
		}
		
		Query query = QueryFactory.create(getQuery());
		QueryExecution qe = QueryExecutionFactory.sparqlService(serviceEndPoint, query);

		try {
			ResultSet rs = qe.execSelect();
			if (rs.hasNext()) {
				System.out.println(ResultSetFormatter.asText(rs));
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());
		} finally {
			qe.close();
		}

		// //Create the Parameterized String
		// ParameterizedSparqlString queryString = new
		// ParameterizedSparqlString();
		// queryString.Namespaces.AddNamespace("ex", new
		// Uri("http://example.org/ns#"));
		// queryString.CommandText = "SELECT * WHERE { ?s ex:property @value }";
		//
		// //Inject a Value for the parameter
		// queryString.SetUri("value", new Uri("http://example.org/value"));
		//
		// //When we call ToString() we get the full command text with
		// namespaces appended as PREFIX
		// //declarations and any parameters replaced with their declared values
		// Console.WriteLine(queryString.ToString());
	}
}
