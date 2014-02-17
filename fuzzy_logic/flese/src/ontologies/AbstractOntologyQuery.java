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

public abstract class AbstractOntologyQuery implements InterfaceOntologyQuery {

	private ArrayList<ArrayList<String>> results;
	private String queryString;
	private String substitutedQueryString;
	private String serviceEndPoint;

	final static String defaultServiceEndpoint = "http://dbpedia.org/sparql";

	protected AbstractOntologyQuery() {
		queryString = null;
		substitutedQueryString = null;
		results = new ArrayList<ArrayList<String>>();
		serviceEndPoint = null;
	}

	public final void setServiceEndPoint(String serviceEndPoint) {
		this.serviceEndPoint = serviceEndPoint;
	}

	public final void setQueryString(String queryString) {
		this.queryString = queryString;
	}

	public final String getQueryString(boolean argumentsSubstituted) {
		if (argumentsSubstituted) {
			return this.substitutedQueryString == null ? "" : this.substitutedQueryString;
		}
		return this.queryString == null ? "" : this.queryString;
	}

	public final void setQueryArguments(String[][] args) {
		String tmpQuery = getQueryString(false);
		ParameterizedSparqlString parametrizedQuery = new ParameterizedSparqlString();

		// Create the Parameterized String
		// ParameterizedSparqlString queryString = new
		// ParameterizedSparqlString();
		parametrizedQuery.setCommandText(tmpQuery);

		for (String[] arg : args) {
			System.out.println("Setting args: arg0: " + arg[0] + " arg1: " + arg[1]);
			parametrizedQuery.setLiteral(arg[0], arg[1]);
		}
		// queryString.setLiteral("url", serviceEndPoint);
		// queryString.toString();

		this.substitutedQueryString = parametrizedQuery.toString();
	}

	public void query() {

		if ((serviceEndPoint == null) || ("".equals(serviceEndPoint))) {
			serviceEndPoint = defaultServiceEndpoint;
		}

		String queryToBeSend = getQueryString(true);
		System.out.println("Sending query: " + queryToBeSend);
		if ((queryToBeSend != null) && (!"".equals(queryToBeSend))) {

			Query query = QueryFactory.create(queryToBeSend);
			QueryExecution qe = QueryExecutionFactory.sparqlService(serviceEndPoint, query);

			try {
				ResultSet rs = qe.execSelect();
				if (rs.hasNext()) {
					QuerySolution qs = rs.next();
					ArrayList<String> tmpResults = new ArrayList<String>();
					results.add(tmpResults);

					String[] varsNames = getVariablesNames();
					for (String varName : varsNames) {
						RDFNode node = qs.get(varName);
						tmpResults.add(node.toString());
					}

					System.out.println(ResultSetFormatter.asText(rs));
				}
			} catch (Exception e) {
				System.out.println(e.getMessage());
			} finally {
				qe.close();
			}
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

	public final String[][] getResults() {
		String[][] arrayResults = new String[this.results.size()][];
		for (int i = 0; i < this.results.size(); i++) {
			ArrayList<String> tmpResult = this.results.get(i);
			arrayResults[i] = tmpResult.toArray(new String[tmpResult.size()]);
		}
		return arrayResults;
	}

}
