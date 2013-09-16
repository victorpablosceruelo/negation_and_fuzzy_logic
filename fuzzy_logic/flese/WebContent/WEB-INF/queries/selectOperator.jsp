
<%@page import="prologConnector.moreInfo.PredMoreInfoInterface"%>
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
	
	String lineIndexString = requestStoreHouse.getRequestParameter(KConstants.JspsDivs.counterId);
	// int lineIndex = Conversors.toInt(lineIndexString);
	String database = requestStoreHouse.getRequestParameter(KConstants.Request.databaseParam);
	String predicate = requestStoreHouse.getRequestParameter(KConstants.Request.predicateParam);
	
	String lineNumber = requestStoreHouse.getRequestParameter(KConstants.Request.lineNumberParam);
	String lineId = requestStoreHouse.getRequestParameter(KConstants.Request.lineIdParam);
	
	String [] type = {database, null};
	PredicateInfo predicateInfo = programIntrospection.getPredicateInfo(predicate);
	String [] [] predicateType = predicateInfo.getFullyDefinedTypes(type);
			
	PredicateInfo predicateInfoOperators = programIntrospection.getPredicateInfo("rfuzzy_compute_defined_operators");
	PredMoreInfoInterface predMoreInfo = predicateInfoOperators.getPredicateMoreInfoAs("defined_operators");
	String [][] operators = predMoreInfo.getOperatorsFor(predicateType);
	
	String [] neededType1 = {database, "rfuzzy_enum_type"};
	String [] neededType2 = {database, "rfuzzy_integer_type"};
	String [] neededType3 = {database, "rfuzzy_string_type"};
%>

<% if ((predicateInfo.hasType(neededType1)) || (predicateInfo.hasType(neededType2)) || (predicateInfo.hasType(neededType3))) { %>
	<select name="<%=lineId %>.operator" id="<%=lineId %>.operator">
		<%=JspsUtils.comboBoxDefaultValue() %>

		<% for (int i=0; i<operators.length; i++) {	%>
				<option value='<%=operators[i][0] %>'>
					<%= JspsUtils.getPrologNameInColloquialLanguage(operators[i][0])  %>
				</option>
		<% } %>
	</select>
<% } else { %>&nbsp;<% } %>
