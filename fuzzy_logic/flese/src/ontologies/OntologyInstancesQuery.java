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

	

	private OntologyInstancesQuery() {
		setQueryString(queryPrefixLine01 + queryPrefixLine02 + queryPrefixLine03 + queryPrefixLine04 + queryPrefixLine05 + queryPrefixLine06
				+ queryPrefixLine07 + queryPrefixLine08 + queryPrefixLine09 + queryPrefixLine10 + queryEndLine01 + queryEndLine02
				+ queryEndLine03);
	}

	public static AbstractOntologyQuery getInstance() {
		return new OntologyInstancesQuery();
	}
	
	@Override
	public String[] getVariablesNames() {
		String[] varsNames = new String[1];
		varsNames[0] = nameVar1;
		return varsNames;
	}

	@Override
	public String[] getArgumentsNames() {
		String[] argsNames = new String[1];
		argsNames[0] = nameArg1;
		return argsNames;
	}

}
