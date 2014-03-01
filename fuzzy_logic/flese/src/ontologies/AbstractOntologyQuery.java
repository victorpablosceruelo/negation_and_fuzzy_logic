package ontologies;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

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

	private String serviceEndPoint;
	private String queryString;
	private String substitutedQueryString;
	private ArrayList<HashMap<String, RDFNode>> results;

	final static String defaultServiceEndpoint = "http://dbpedia.org/sparql";

	protected AbstractOntologyQuery() {
		serviceEndPoint = null;
		queryString = null;
		substitutedQueryString = null;
		results = new ArrayList<HashMap<String, RDFNode>>();
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

	public final void setQueryArguments(HashMap<String, OntologyQueryArgument> args) {
		if (args == null) {
			args = new HashMap<String, OntologyQueryArgument>();
		}

		String tmpQuery = getQueryString(false);
		ParameterizedSparqlString parametrizedQuery = new ParameterizedSparqlString();

		// Create the Parameterized String
		// ParameterizedSparqlString queryString = new
		// ParameterizedSparqlString();
		parametrizedQuery.setCommandText(tmpQuery);

		Set<String> keys = args.keySet();

		for (String key : keys) {
			OntologyQueryArgument argValue = args.get(key);
			setQueryArgument(key, argValue, parametrizedQuery);
		}
		// queryString.setLiteral("url", serviceEndPoint);
		// queryString.toString();

		this.substitutedQueryString = parametrizedQuery.toString();
	}
		
	private void setQueryArgument(String key, OntologyQueryArgument argValue, ParameterizedSparqlString parametrizedQuery) {
		System.out.println("Setting args: arg0: " + key + " arg1: " + argValue.toString());
		
		if (argValue.isNode()) {
			RDFNode value = argValue.getRDFNode();			
			parametrizedQuery.setParam(key, value);
		}
		else {
			parametrizedQuery.setLiteral(key, argValue.toString());
		}
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

			ResultSet rs = null;
			try {
				rs = qe.execSelect();
				while (rs.hasNext()) {
					QuerySolution qs = rs.next();
					HashMap<String, RDFNode> result = new HashMap<String, RDFNode>();

					String[] varsNames = getVariablesNames();
					for (String varName : varsNames) {
						RDFNode value = qs.get(varName);
						result.put(varName, value);
					}

					results.add(result);
				}
			} catch (Exception e) {
				rs = null;
				System.out.println(e.getMessage());
			} finally {
				qe.close();
			}
			
			if (rs != null) {
			System.out.println(ResultSetFormatter.asText(rs));
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

	public final ArrayList<HashMap<String, RDFNode>> getResults() {
		return results;
	}

	public final String[][][] getResultsAsStrings() {
		String[][][] arrayResults = new String[this.results.size()][][];

		int size = this.results.size();
		for (int i=0; i<size; i++) {
			HashMap<String, RDFNode> result = this.results.get(i); 
			Set<String> keys = result.keySet();
			String[][] stringsResult = new String[keys.size()][];
			int j = 0;

			for (String key : keys) {
				RDFNode node = result.get(key);
				String value = node.toString();
				stringsResult[j] = new String[2];
				stringsResult[j][0] = key;
				stringsResult[j][1] = value;
				j++;
			}
			
			arrayResults[i] = stringsResult;
		}

		return arrayResults;
	}

	// .
	// .
	// .
	// .
}
