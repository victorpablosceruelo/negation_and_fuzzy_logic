<%@page import="auxiliar.JspsUtils"%>
<%@page import="storeHouse.RequestStoreHouse"%>
<%@page import="results.ResultsStoreHouse"%>

<%
	String [] msgs = JspsUtils.getResultMessages(request);
	for (int i=0; i<msgs.length; i++) {
		out.println(msgs[i]);
	}
%>

<!-- END -->