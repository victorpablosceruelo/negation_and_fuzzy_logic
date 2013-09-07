
<%@page import="constants.KUrls"%>
<%@page import="prologConnector.CiaoPrologQueryAnswer"%>
<%@page import="prologConnector.CiaoPrologProgramIntrospectionQuery"%>
<%@page import="storeHouse.RequestStoreHouse"%>

<%
	
	RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request, false);
	CiaoPrologQueryAnswer [] queryAnswers = requestStoreHouse.getResultsStoreHouse().getCiaoPrologQueryAnswers();	
	
%>



