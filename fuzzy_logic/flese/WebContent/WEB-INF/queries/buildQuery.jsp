
<%@page import="prologConnector.CiaoPrologQueryAnswer"%>
<%@page import="prologConnector.CiaoPrologProgramIntrospectionQuery"%>
<%@page import="storeHouse.RequestStoreHouse"%>

<%
	
	RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request, false);
	CiaoPrologProgramIntrospectionQuery ciaoPrologProgramIntrospectionQuery = requestStoreHouse.getResultsStoreHouse().getCiaoPrologProgramIntrospectionQuery();

	CiaoPrologQueryAnswer [] queryAnswers = ciaoPrologProgramIntrospectionQuery.getQueryAnswers();
	
	
%>


showMsgsToTheUser();

