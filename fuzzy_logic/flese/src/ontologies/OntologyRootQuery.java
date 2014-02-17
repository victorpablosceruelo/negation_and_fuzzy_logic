package ontologies;

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

	private final String nameVar1 = "class";

	private OntologyRootQuery() {
		setQueryString(queryPrefixLine01 + queryPrefixLine02 + queryPrefixLine03 + queryPrefixLine04 + queryPrefixLine05
				+ queryPrefixLine06 + queryPrefixLine07 + queryPrefixLine08 + queryPrefixLine09 + queryPrefixLine10 + queryEndLine01
				+ queryEndLine02 + queryEndLine03 + queryEndLine04 + queryEndLine05 + queryEndLine06);
	}

	public static AbstractOntologyQuery getInstance() {
		return new OntologyRootQuery();
	}

	@Override
	public String[] getVariablesNames() {
		String[] varsNames = new String[1];
		varsNames[0] = nameVar1;
		return varsNames;
	}

	@Override
	public String[] getArgumentsNames() {
		return new String[0];
	}

}
