<%@page import="ontologies.OntologyQueryVarResult"%>
<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="storeHouse.RequestStoreHouse"%>

<%
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
	OntologyQueryVarResult[][][] ontologyQueryResults = resultsStoreHouse.getOntologyQueryResults();
%>

Results:

<% 
	for (int i=0; i<ontologyQueryResults.length; i++) {
		OntologyQueryVarResult [][] result = ontologyQueryResults[i];
		result = (result == null) ? new OntologyQueryVarResult [0][] : result;
		for (int j=0; j<result.length; j++) {
			OntologyQueryVarResult [] resultDetails = result[j];
			resultDetails = (resultDetails == null) ? new OntologyQueryVarResult [0] : resultDetails;
			for (int k=0; k<resultDetails.length; k++) {
				OntologyQueryVarResult varAndValue = resultDetails[k];
				if (varAndValue != null) {
%>
				<BR><%= varAndValue.getRDFNodeDescription() %>
<%
				}
			}
		}
	}
%>

<script type="text/javascript">
	// loadAjaxIn('<%=KConstants.JspsDivsIds.ontologyStartDivId %>', '<%=KUrls.Files.UploadDiv.getUrl(true) %>');
</script>

<!-- END -->
