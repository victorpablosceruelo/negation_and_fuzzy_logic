<%@page import="results.ResultsStoreHouse"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="java.util.ArrayList"%>
<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>

<%
	String msg = JspsUtils.getResultMessage(request);
%>
	<div class="fileViewTable">
<% 
if ((msg != null) && (msg.length() > 0)) { 
	out.println(msg);
} else { 
	
	ResultsStoreHouse resultsStoreHouse =  JspsUtils.getResultsStoreHouse(request);	
	String [] fileContents = resultsStoreHouse.getfileContents();
	
	if (fileContents != null) {
		for (int i=0; i< fileContents.length; i++) {
%>
			<div class="fileViewTableRow">
				<div class="fileViewTableCell">
					<%= fileContents[i] %>
				</div>
			</div>
<%
		}
	}
}
%>

</div>






<!--  END -->
