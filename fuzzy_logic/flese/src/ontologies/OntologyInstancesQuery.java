package ontologies;

import java.util.ArrayList;

import com.hp.hpl.jena.query.ParameterizedSparqlString;
import com.hp.hpl.jena.query.Query;
import com.hp.hpl.jena.query.QueryExecution;
import com.hp.hpl.jena.query.QueryExecutionFactory;
import com.hp.hpl.jena.query.QueryFactory;
import com.hp.hpl.jena.query.QuerySolution;
import com.hp.hpl.jena.query.ResultSet;
import com.hp.hpl.jena.query.ResultSetFormatter;
import com.hp.hpl.jena.rdf.model.RDFNode;

public class OntologyInstancesQuery extends AbstractOntologyQuery {

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

	private final String queryEndLine01 = "SELECT ?instance ";
	private final String queryEndLine02 = "WHERE { ?instance a @url } ";
	private final String queryEndLine03 = "        ORDER BY ?instance ";
	// @url = <http://dbpedia.org/ontology/Disease>

	private final String nameVar1 = "instance";

	public static final String nameArg1 = "url";

	ArrayList<String> results;
	ParameterizedSparqlString queryString;

	public OntologyInstancesQuery() {
		results = new ArrayList<String>();
		queryString = new ParameterizedSparqlString();
	}

	final static String defaultServiceEndpoint = "http://dbpedia.org/sparql";

	private final String getQuery() {
		return queryPrefixLine01 + queryPrefixLine02 + queryPrefixLine03 + queryPrefixLine04 + queryPrefixLine05 + queryPrefixLine06
				+ queryPrefixLine07 + queryPrefixLine08 + queryPrefixLine09 + queryPrefixLine10 + queryEndLine01 + queryEndLine02
				+ queryEndLine03;
	}

	@Override
	public void setArguments(String[][] args) {
		// Create the Parameterized String
		// ParameterizedSparqlString queryString = new
		// ParameterizedSparqlString();
		queryString.setCommandText(getQuery());

		for (String[] arg : args) {
			System.out.println("Setting args: arg0: " + arg[0] + " arg1: " + arg[1]);
			queryString.setLiteral(arg[0], arg[1]);
		}
		// queryString.setLiteral("url", serviceEndPoint);
		// queryString.toString();

	}

	public void query(String serviceEndPoint) {

		if ((serviceEndPoint == null) || ("".equals(serviceEndPoint))) {
			serviceEndPoint = defaultServiceEndpoint;
		}

		// queryString.Namespaces.AddNamespace("ex", new
		// Uri("http://example.org/ns#"));
		// queryString.CommandText = "SELECT * WHERE { ?s ex:property @value }";
		// Inject a Value for the parameter
		// queryString.SetUri("value", new Uri("http://example.org/value"));

		// When we call ToString() we get the full command text with namespaces
		// appended as PREFIX
		// declarations and any parameters replaced with their declared values
		// Console.WriteLine(queryString.ToString());

		String realQuery = queryString.toString();
		System.out.println(realQuery);
		Query query = QueryFactory.create(realQuery);
		QueryExecution qe = QueryExecutionFactory.sparqlService(serviceEndPoint, query);

		try {
			ResultSet rs = qe.execSelect();
			if (rs.hasNext()) {
				QuerySolution qs = rs.next();
				RDFNode node = qs.get(nameVar1);
				results.add(node.toString());
				System.out.println(ResultSetFormatter.asText(rs));
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());
		} finally {
			qe.close();
		}
	}

	@Override
	public String[] getResults() {
		return this.results.toArray(new String[this.results.size()]);
	}
}
