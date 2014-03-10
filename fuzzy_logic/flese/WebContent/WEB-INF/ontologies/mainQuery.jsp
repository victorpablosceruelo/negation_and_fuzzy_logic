<%@page import="ontologies.OntologyQueryVarResult"%>
<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="storeHouse.RequestStoreHouse"%>


<%
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
	OntologyQueryVarResult[][][] ontologyQueryResults = resultsStoreHouse.getOntologyQueryResults();
	String destinyUrl = KUrls.Ontologies.InstancesQuery.getUrl(true);
	String [] serviceEndPoints = resultsStoreHouse.getRequestParamsHashMap().get(KConstants.Request.serviceEndPoint); 
	String serviceEndPoint = ((serviceEndPoints != null) && (serviceEndPoints.length > 0)) ? serviceEndPoints[0] : "";
	String prefix = destinyUrl + "&" + KConstants.Request.serviceEndPoint + "=" + serviceEndPoint + "&" + KConstants.Request.url + "=";
	String divId = KConstants.JspsDivsIds.ontologyQueryResultsDivId;
%>

<table>
  <tr>
    <th>Result</th>
  </tr>

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
					String url = varAndValue.getUrlToRDFNode(prefix);
					out.println();
%>
				<tr><td>
				<a href="#" onclick="return loadAjaxIn('<%=divId %>', '<%= url %>');">
				<%=varAndValue.getRDFNodeDescription() %></a>
				</td></tr>
<%
				}
			}
		}
	}
%>
</table>

<script type="text/javascript">
	// loadAjaxIn('<%=KConstants.JspsDivsIds.ontologyStartDivId %>', '<%=KUrls.Files.UploadDiv.getUrl(true) %>');
</script>


<!-- END -->
