
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
	
	String lineIndexString = requestStoreHouse.getRequestParameter(KConstants.JspsDivsAndFields.counterFieldId);
	// int lineIndex = Conversors.toInt(lineIndexString);
	String database = requestStoreHouse.getRequestParameter(KConstants.Request.databaseParam);
	String lineNumber = requestStoreHouse.getRequestParameter(KConstants.Request.lineNumberParam);
	String lineId = requestStoreHouse.getRequestParameter(KConstants.Request.lineIdParam);
	
	String [] type = {database, null};
	PredicateInfo[] predicatesInfos = programIntrospection.getPredicatesInfosByType(type);
	
	String common = programIntrospection.getProgramFileInfo().getInfoForUrls() +
	"&" + KConstants.Request.lineNumberParam + "=" + lineNumber +
	"&" + KConstants.Request.lineIdParam + "=" + lineId +
	"&" + KConstants.Request.databaseParam + "=" + database + 
	"&" + KConstants.Request.predicateParam + "=";
	// JspsUtils.getValue(common);
	String negUrl = KUrls.Queries.SelectNegation.getUrl(true) + common;
	String quantUrl = KUrls.Queries.SelectQuantifier.getUrl(true) + common;
	String opUrl = KUrls.Queries.SelectOperator.getUrl(true) + common;
	String valueUrl = KUrls.Queries.SelectValue.getUrl(true) + common;
%>

<div id="<%=lineId %>.row" class="queryLinesTableRow">

	<!-- Negation -->
	<div id="<%=lineId %>.negationDiv" class="queryLinesTableCell">
	</div>

	<!-- Quantifier -->
	<div id="<%=lineId %>.quantifierDiv" class="queryLinesTableCell">
	</div>

	<!-- Predicate -->
	<div id="<%=lineId %>.predicateDiv" class="queryLinesTableCell">
	
		<select name="<%=lineId %>.predicate" id="<%=lineId %>.predicate" 
				onchange="selectPredicateChanged(this, '<%=lineId %>', '<%=negUrl %>', '<%=quantUrl %>', '<%=opUrl %>', '<%=valueUrl %>');">
				<%=JspsUtils.comboBoxDefaultValue() %>

				<% for (int i=0; i<predicatesInfos.length; i++) { %>
					<option title='<%=i %>' value='<%=predicatesInfos[i].getPredicateName() %>'>
						<%=JspsUtils.getPrologNameInColloquialLanguage(predicatesInfos[i].getPredicateName()) %>
					</option>
				<% } %>
		</select>
	</div>
	
	<!-- Operator -->
	<div id="<%=lineId %>.operatorDiv" class="queryLinesTableCell">
	</div>

	<!-- Value -->
	<div id="<%=lineId %>.valueDiv" class="queryLinesTableCell">
	</div>
	
</div>

	<script type="text/javascript">
		document.getElementById('<%= lineId %>.negationDiv').style.display='none';
		document.getElementById('<%= lineId %>.quantifierDiv').style.display='none';
		document.getElementById('<%= lineId %>.operatorDiv').style.display='none';
		document.getElementById('<%= lineId %>.valueDiv').style.display='none';
	</script>

