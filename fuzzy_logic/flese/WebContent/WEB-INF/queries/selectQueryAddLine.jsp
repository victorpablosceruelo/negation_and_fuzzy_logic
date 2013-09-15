
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
	String lineNumber = requestStoreHouse.getRequestParameter(KConstants.Request.lineNumberParam);
	String lineId = requestStoreHouse.getRequestParameter(KConstants.Request.lineIdParam);
	
	String [] type = {database, null};
	PredicateInfo[] predicatesInfos = programIntrospection.getPredicatesInfosByType(type);
%>

<div id="<%=lineId %>.row" class="queryLinesTableRow">

	<!-- Predicate -->
	<div id="<%=lineId %>.predicate" class="queryLinesTableCell">
	
		<select name="<%=lineId %>.selectPredicate" id="<%=lineId %>.selectPredicate" 
				onchange="selectPredicateChanged(this, '<%=lineId %>', '<%=database %>');">
				<%=JspsUtils.comboBoxDefaultValue() %>

				<% for (int i=0; i<predicatesInfos.length; i++) { %>
					<option title='<%=i %>' value='<%=predicatesInfos[i].getPredicateName() %>'>
						<%=JspsUtils.getPrologNameInColloquialLanguage(predicatesInfos[i].getPredicateName()) %>
					</option>
				<% } %>
		</select>
	</div>
</div>


