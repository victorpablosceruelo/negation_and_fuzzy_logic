package ontologies;

import com.hp.hpl.jena.rdf.model.RDFNode;

public class OntologyQueryResult {

	public OntologyQueryResult(String key, RDFNode node) {

		String value = getRDFNodeAsString(node);
		stringsResult[j] = new String[2];
		stringsResult[j][0] = key;
		stringsResult[j][0] = keyDescr;
		stringsResult[j][1] = value;
		stringsResult[j][1] = valueDesc;
	}
}
