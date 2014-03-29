<%@page import="ontologies.InterfaceOntologyQuery"%>
<%@page import="ontologies.OntologyQueryVarResult"%>
<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="storeHouse.RequestStoreHouse"%>


<%
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
	InterfaceOntologyQuery[] ontologyQueries = resultsStoreHouse.getOntologyQueryResults();
	String serviceEndPoint = JspsUtils.getServiceEndPointParam(resultsStoreHouse);
	String divIdPrefix = JspsUtils.getDivIdPrefix(resultsStoreHouse, KConstants.JspsDivsIds.ontologyQueryResultsDivId);
	
	String instancesQueryUrl = KUrls.Ontologies.InstancesQuery.getUrl(true);
	String propertiesQueryUrl = KUrls.Ontologies.PropertiesQuery.getUrl(true);
	
	// String instancesPrefix = JspsUtils.setUrlParamServiceEndPoint(instancesQueryUrl, serviceEndPoint);
	// String propertiesPrefix = JspsUtils.setUrlParamServiceEndPoint(propertiesQueryUrl, serviceEndPoint);

	for (int i=0; i<ontologyQueries.length; i++) {
		InterfaceOntologyQuery ontologyQuery = ontologyQueries[i];
		String [] varsNames = ontologyQuery.getVariablesNames();
%> 
<table>
  <tr>
  <%
  		for (int j=0; j<varsNames.length; j++) {
  %>
    	<th><%=varsNames[j] %></th>
  <% 	} %>
  </tr>
<%		
		OntologyQueryVarResult [][] result = ontologyQueries[i].getResultsWithInfo();
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
					String rdfNodeUri = varAndValue.getRDFNodeURI();
					out.println();
%>
				<td><%=varAndValue.getRDFNodeDescription() %>&nbsp;
				<a href="#" 
				onclick="return ontologyQuery('<%=instancesQueryUrl %>', '<%= serviceEndPoint %>', '<%= rdfNodeUri %>', '<%= divId %>');">
					instances
				</a>&nbsp;
				<a href="#" 
				onclick="return ontologyQuery('<%=propertiesQueryUrl %>', '<%= serviceEndPoint %>', '<%= rdfNodeUri %>', '<%= divId %>');">
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
%>
</table>
<%
	}
%>


<script type="text/javascript">
	// loadAjaxIn('<%=KConstants.JspsDivsIds.ontologyStartDivId %>', '<%=KUrls.Files.UploadDiv.getUrl(true) %>');
</script>


<!-- END -->
