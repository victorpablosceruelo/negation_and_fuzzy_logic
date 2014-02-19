<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="storeHouse.RequestStoreHouse"%>

<%
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
	String [][][][] ontologyQueryResults = resultsStoreHouse.getOntologyQueryResults();
%>

Results:

<% 
	for (int i=0; i<ontologyQueryResults.length; i++) {
		String [][][] result = ontologyQueryResults[i];
		result = (result == null) ? new String [0][][] : result;
		for (int j=0; j<result.length; j++) {
			String [][] resultDetails = result[j];
			resultDetails = (resultDetails == null) ? new String [0][] : resultDetails;
			for (int k=0; k<resultDetails.length; k++) {
				String [] varAndValue = resultDetails[k];
				varAndValue = (varAndValue == null) ? new String [0] : varAndValue;
%>
				<BR>
<%
				for (int l=0; l<varAndValue.length; l++) {
%>
					<%= varAndValue[l] %>
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
