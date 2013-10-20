<%@page import="auxiliar.JspsUtils"%>
<%@page import="storeHouse.RequestStoreHouse"%>
<%@page import="results.ResultsStoreHouse"%>

<%
	RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request);
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);	
%>

<%=resultsStoreHouse.getResultMessage() %>

<!-- END -->