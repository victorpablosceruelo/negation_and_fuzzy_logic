<%@page import="auxiliar.JspsUtils"%>
<%@page import="prologConnector.CiaoPrologQueryAnswer"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="storeHouse.RequestStoreHouse"%>

<% 
	RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request, false);
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
	String [] variablesNames = resultsStoreHouse.getCiaoPrologQueryVariablesNames();
	CiaoPrologQueryAnswer [] answers = resultsStoreHouse.getCiaoPrologQueryAnswers();
	
	out.println("cleanUpQueryAnswers();");
			for (int i=0; i<queryAnswers.length; i++) {
				out.println(queryAnswers[i]);
			}
%>
