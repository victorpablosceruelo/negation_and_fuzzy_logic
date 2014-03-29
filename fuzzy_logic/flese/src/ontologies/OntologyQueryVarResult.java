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
		String nodeDescr = "";
		// String nodeUri = null;

		if ((this.node != null ) && (this.node.isResource())) {
			Resource rNode = node.asResource();
			// nodeUri = rNode.getURI();
			nodeDescr = rNode.toString();
		}

		return nodeDescr; // + " " + nodeUri;
	}

	public String getRDFNodeURI() {
		if ((this.node != null ) && (this.node.isResource())) {
			Resource rNode = node.asResource();
			String nodeUri = rNode.getURI();
			return nodeUri;
		}
		return "";
	}
}
