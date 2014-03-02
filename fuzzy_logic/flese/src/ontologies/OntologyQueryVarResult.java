package ontologies;

import com.hp.hpl.jena.rdf.model.RDFNode;
import com.hp.hpl.jena.rdf.model.Resource;

public class OntologyQueryVarResult {

	private String key;
	private RDFNode node;
	
	public OntologyQueryVarResult(String key, RDFNode node) throws OntologiesException {

		if (key == null) {
			throw new OntologiesException("key cannot be null");
		}
		if ("".equals(key)) {
			throw new OntologiesException("key cannot be empty string");
		}
		if (node == null) {
			throw new OntologiesException("node cannot be null");
		}
		
		this.key = key;
		this.node = node;
	}
	
	public String getKey() {
		return this.key;
	}
	
	public RDFNode getNode() {
		return this.node;
	}
	
	public String getRDFNodeDescription() {
		String nodeDescr = null;
		// String nodeUri = null;

		if (this.node.isResource()) {
			Resource rNode = node.asResource();
			// nodeUri = rNode.getURI();
			nodeDescr = rNode.toString();
		}

		return nodeDescr; // + " " + nodeUri;
	}

	public String getRDFNodeHtmlRef(String prefix) {
		if (this.node.isResource()) {
			Resource rNode = node.asResource();
			String nodeUri = rNode.getURI();
			return prefix + nodeUri;
		}
		return "";
	}
}
