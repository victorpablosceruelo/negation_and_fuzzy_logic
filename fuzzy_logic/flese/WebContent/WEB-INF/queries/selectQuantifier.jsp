
<%@page import="auxiliar.Conversors"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="constants.KUrls"%>
<%@page import="prologConnector.CiaoPrologQueryAnswer"%>
<%@page import="prologConnector.CiaoPrologProgramIntrospectionQuery"%>
<%@page import="storeHouse.RequestStoreHouse"%>
<%@page import="prologConnector.PredicateInfo"%>
<%@page import="prologConnector.ProgramIntrospection"%>

<%
	RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request, false);
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
	ProgramIntrospection programIntrospection = resultsStoreHouse.getCiaoPrologProgramIntrospection();
	
	String lineIndexString = requestStoreHouse.getRequestParameter(KConstants.JspsDivsAndFields.counterId);
	// int lineIndex = Conversors.toInt(lineIndexString);
	String database = requestStoreHouse.getRequestParameter(KConstants.Request.databaseParam);
	String predicate = requestStoreHouse.getRequestParameter(KConstants.Request.predicateParam);
	String lineNumber = requestStoreHouse.getRequestParameter(KConstants.Request.lineNumberParam);
	String lineId = requestStoreHouse.getRequestParameter(KConstants.Request.lineIdParam);
	
	PredicateInfo predicatePredicateInfo = programIntrospection.getPredicateInfo(predicate);
	String [] neededType = {database, "rfuzzy_truth_value_type"};
	
	String [] type = {"rfuzzy_predicate_type", "rfuzzy_truth_value_type"};
	PredicateInfo[] predicatesInfos = programIntrospection.getPredicatesInfosByType(type);
%>

<% if (predicatePredicateInfo.hasType(neededType, false)) { %>
	<select name="<%=lineId %>.quantifier" id="<%=lineId %>.quantifier">
		<%=JspsUtils.comboBoxDefaultValue() %>

		<% for (int i=0; i<predicatesInfos.length; i++) { %>
			<% if (! predicatesInfos[i].getPredicateName().equals("fnot")) { %>
				<option title='<%=i %>' value='<%=predicatesInfos[i].getPredicateName() %>'>
					<%=JspsUtils.getPrologNameInColloquialLanguage(predicatesInfos[i].getPredicateName()) %>
				</option>
			<% } %>	
		<% } %>
	</select>
<% } %>
