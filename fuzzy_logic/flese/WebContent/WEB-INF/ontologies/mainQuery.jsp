<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="storeHouse.RequestStoreHouse"%>

<%
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
	String [][][] ontologyQueryResults = resultsStoreHouse.getOntologyQueryResults();
%>

Results:

<% 
	for (int i=0; i<ontologyQueryResults.length; i++) {
		String [][] result = ontologyQueryResults[i];
		for (int j=0; j<result.length; j++) {
			String [] resultDetails = result[j];
			for (int k=0; k<resultDetails.length; k++) {
%>
				<%= resultDetails[k] %>
<%
			}
		}
	}
%>

<script type="text/javascript">
	// loadAjaxIn('<%=KConstants.JspsDivsIds.ontologyStartDivId %>', '<%=KUrls.Files.UploadDiv.getUrl(true) %>');
</script>

<!-- END -->
