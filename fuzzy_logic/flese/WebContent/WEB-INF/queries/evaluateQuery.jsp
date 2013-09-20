<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="prologConnector.CiaoPrologQueryAnswer"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="storeHouse.RequestStoreHouse"%>

<script type="text/javascript">
cleanUpQueryAnswers();

<% 
	RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request, false);
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
	String [] variablesNames = resultsStoreHouse.getCiaoPrologQueryVariablesNames();
	CiaoPrologQueryAnswer [] answers = resultsStoreHouse.getCiaoPrologQueryAnswers();
	
	if (answers.length > 0) {
		for (int i=0; i< answers.length; i++) {
			out.println("addToProgramQueryAnsers(" + i + ", [");
			for (int j=0; j<variablesNames.length; j++) {
				out.println(answers[i].getCiaoPrologQueryVariableAnswer(variablesNames[j]));
				if (j+1 < variablesNames.length) {
					out.println(", ");
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