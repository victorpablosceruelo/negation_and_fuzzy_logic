<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="prologConnector.CiaoPrologQueryAnswer"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="storeHouse.RequestStoreHouse"%>

<script type="text/javascript">
<% 
	RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request);
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
	String [] variablesNames = resultsStoreHouse.getCiaoPrologQueryVariablesNames();
	CiaoPrologQueryAnswer [] answers = resultsStoreHouse.getCiaoPrologQueryAnswers();
	
	if (answers.length > 0) {
		out.print("showAnswers('" + KConstants.JspsDivsIds.runQueryDivId + "', [ [ 0, ");
		for (int j=0; j<variablesNames.length; j++) {
			out.print("'" + variablesNames[j] + "'");
			if (j+1 < variablesNames.length) {
				out.print(", ");
			}
		}
		out.println("]");
		if (answers.length > 0) {
			out.print(", ");

			for (int i=0; i< answers.length; i++) {
				out.print("[ " + (i + 1) + ", ");
				for (int j=0; j<variablesNames.length; j++) {
					out.print("'" + answers[i].getCiaoPrologQueryVariableAnswer(variablesNames[j]) + "'");
					if (j+1 < variablesNames.length) {
						out.print(", ");
					}
				}
				out.println("]");
				if (i+1 < answers.length) {
					out.print(", ");
				}
			}
			out.print(" ]); ");
		}
	}
	
%>
</script>
<%
	if (answers.length <= 0) {
		String msg = JspsUtils.getResultMessage(request);
		if ((msg == null) || (msg.length() <= 0)) {
%>The query has no answers. <%
		} else {
			%>msg<%
		}
	}
%>