<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="storeHouse.RequestStoreHouse"%>

<script type="text/javascript">
<%
	RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request, false);
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);

	String [] msgs = resultsStoreHouse.getMessages();
	if ((msgs != null) && (msgs.length > 0)) {
		for (int i=0; i<msgs.length; i++) {
			out.println("alert('" + msgs[i] +"');");
		}
	}
%>
</script>