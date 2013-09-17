
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
	String database = requestStoreHouse.getRequestParameter(KConstants.Request.databaseParam);
	String predicate = requestStoreHouse.getRequestParameter(KConstants.Request.predicateParam);
	String lineNumber = requestStoreHouse.getRequestParameter(KConstants.Request.lineNumberParam);
	String lineId = requestStoreHouse.getRequestParameter(KConstants.Request.lineIdParam);
	
	PredicateInfo predicatePredicateInfo = programIntrospection.getPredicateInfo(predicate);
	String [] neededType = {database, KConstants.PrologTypes.rfuzzy_enum_type};	

%>

<% if (predicatePredicateInfo.hasType(neededType)) { %>
	<% 
		PredMoreInfoInterface pmi = predicatePredicateInfo.getPredicateMoreInfoAs(KConstants.MoreInfoTypes.enumTypeValues);
		if (pmi != null) {
	%>
		<select name="<%=lineId %>.value" id="<%=lineId %>.value">
			<%=JspsUtils.comboBoxDefaultValue() %>

			<% 
				String [] values = pmi.getValuesFor(database); 
				for (int i=0; i<values.length; i++) {
					if (null != values[i]) { 
			%>
						<option title='<%=values[i] %>' value='<%=values[i] %>'>
							<%=JspsUtils.getPrologNameInColloquialLanguage(values[i]) %>
						</option>
			<%
					}
				}
			%>
		</select>
	<% } %>
<% } else { %>
	<input type='text' value='' name="<%=lineId %>.value" id="<%=lineId %>.value" />
<% } %>


