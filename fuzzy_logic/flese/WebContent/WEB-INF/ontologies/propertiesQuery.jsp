<%@page import="ontologies.OntologyQueryVarResult"%>
<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="storeHouse.RequestStoreHouse"%>


<%
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
	OntologyQueryVarResult[][][] ontologyQueryResults = resultsStoreHouse.getOntologyQueryResults();
	String serviceEndPoint = JspsUtils.getServiceEndPointParam(resultsStoreHouse);
	String divIdPrefix = KConstants.JspsDivsIds.ontologySubQueryResultsDivId + "_";
	
	String instancesQueryUrl = KUrls.Ontologies.InstancesQuery.getUrl(true);
	String instancesPrefix = JspsUtils.getOntologyQueryUrl(instancesQueryUrl, serviceEndPoint);
	
	String propertiesQueryUrl = KUrls.Ontologies.PropertiesQuery.getUrl(true);
	String propertiesPrefix = JspsUtils.getOntologyQueryUrl(propertiesQueryUrl, serviceEndPoint);
%>

<table>
  <tr>
    <th></th>
    <th></th>
    <th></th>
  </tr>

<% 
	for (int i=0; i<ontologyQueryResults.length; i++) {
		OntologyQueryVarResult [][] result = ontologyQueryResults[i];
		result = (result == null) ? new OntologyQueryVarResult [0][] : result;
		for (int j=0; j<result.length; j++) {
			OntologyQueryVarResult [] resultDetails = result[j];
			resultDetails = (resultDetails == null) ? new OntologyQueryVarResult [0] : resultDetails;
			String divId = divIdPrefix + i + "_" + j;
%>
	<tr>
<%
			for (int k=0; k<resultDetails.length; k++) {
				OntologyQueryVarResult varAndValue = resultDetails[k];
				if (varAndValue != null) {
					String instancesUrl = varAndValue.getUrlToRDFNode(instancesPrefix);
					String propertiesUrl = varAndValue.getUrlToRDFNode(propertiesPrefix);
					out.println();
%>
				<td><%=varAndValue.getRDFNodeDescription() %>&nbsp;
				<a href="#" onclick="return loadAjaxIn('<%=divId %>', '<%= instancesUrl %>');">
					instances
				</a>&nbsp;
				<a href="#" onclick="return loadAjaxIn('<%=divId %>', '<%= propertiesUrl %>');">
					properties
				</a>
				</td>
<%
				}
			}
%>
	</tr>
	<tr><td><div id='<%=divId %>'></div></td></tr>
<%
		}
	}
%>
</table>

<script type="text/javascript">
	// loadAjaxIn('<%=KConstants.JspsDivsIds.ontologyStartDivId %>', '<%=KUrls.Files.UploadDiv.getUrl(true) %>');
</script>


<!-- END -->
