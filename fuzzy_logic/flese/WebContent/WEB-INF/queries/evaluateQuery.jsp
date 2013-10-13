<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="prologConnector.CiaoPrologQueryAnswer"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="storeHouse.RequestStoreHouse"%>

<script type="text/javascript">
cleanUpQueryAnswers();

<% 
	RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request);
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
	String [] variablesNames = resultsStoreHouse.getCiaoPrologQueryVariablesNames();
	CiaoPrologQueryAnswer [] answers = resultsStoreHouse.getCiaoPrologQueryAnswers();
	
	if (answers.length > 0) {
		out.print("addToProgramQueryAnsers(0, [");
		for (int j=0; j<variablesNames.length; j++) {
			out.print("'" + variablesNames[j] + "'");
			if (j+1 < variablesNames.length) {
				out.print(", ");
			}
		}
		out.println("]);");
		for (int i=0; i< answers.length; i++) {
			out.print("addToProgramQueryAnsers(" + (i + 1) + ", [");
			for (int j=0; j<variablesNames.length; j++) {
				out.print("'" + answers[i].getCiaoPrologQueryVariableAnswer(variablesNames[j]) + "'");
				if (j+1 < variablesNames.length) {
					out.print(", ");
				}
			}
			out.println("]);");
		}
	}
	
%>

showQueryAnswers('<%=KConstants.JspsDivsIds.runQueryDivId %>');
</script>
<%
	if (answers.length <= 0) {
		String [] msgs = resultsStoreHouse.getMessages();
		if (msgs.length <= 0) {
			%>The query has no answers. Maybe the database is empty? <%
		}
		else {
			for (int i=0; i<msgs.length; i++) {
				out.println(msgs[i] + "<br />");
			}
		}
	}
%>