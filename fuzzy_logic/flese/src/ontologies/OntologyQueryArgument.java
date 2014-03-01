package ontologies;

import com.hp.hpl.jena.rdf.model.RDFNode;

public class OntologyQueryArgument {

	private RDFNode rdfNodeArg;
	private String stringArg;

	public OntologyQueryArgument(RDFNode rdfNodeArg) throws OntologiesException {
		if (rdfNodeArg == null) {
			throw new OntologiesException("Node to create argument is null.");
		}

		this.rdfNodeArg = rdfNodeArg;
		this.stringArg = null;
	}

	public OntologyQueryArgument(String stringArg) throws OntologiesException {
		if (stringArg == null) {
			throw new OntologiesException("String to create argument is null.");
		}
		if ("".equals(stringArg)) {
			throw new OntologiesException(
					"String to create argument is an empty string.");
		}

		this.rdfNodeArg = null;
		this.stringArg = stringArg;
	}

	public boolean isNode() {
		return (this.rdfNodeArg != null);
	}

	public RDFNode getRDFNode() {
		return this.rdfNodeArg;
	}

	public String getStringArg() {
		return this.stringArg;
	}

	public String toString() {
		if (isNode()) {
			return this.rdfNodeArg.toString();
		}
		return this.stringArg;
	}
}
