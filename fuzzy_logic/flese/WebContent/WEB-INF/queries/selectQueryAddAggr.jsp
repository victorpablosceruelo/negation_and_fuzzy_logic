
<%@page import="prologConnector.PredicateInfo"%>
<%@page import="prologConnector.ProgramIntrospection"%>
<%@page import="auxiliar.Conversors"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="constants.KUrls"%>
<%@page import="prologConnector.CiaoPrologQueryAnswer"%>
<%@page import="prologConnector.CiaoPrologProgramIntrospectionQuery"%>
<%@page import="storeHouse.RequestStoreHouse"%>

<%
	
	RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request, false);
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
	String queryLinesCounter = requestStoreHouse.getRequestParameter(KConstants.QueryParams.queryLinesCounter);
	// int lineIndex = Conversors.toInt(lineIndexString);
	
	ProgramIntrospection programIntrospection = resultsStoreHouse.getCiaoPrologProgramIntrospection();
	String database = requestStoreHouse.getRequestParameter(KConstants.Request.databaseParam);
	
	String selectQueryAddLineUrl = KUrls.Queries.SelectQueryAddLine.getUrl(true) + programIntrospection.getProgramFileInfo().getInfoForUrls() + 
	"&" + KConstants.Request.databaseParam + "=" + database;
	String selectQueryAddAggrUrl = KUrls.Queries.SelectQueryAddAggr.getUrl(true) + programIntrospection.getProgramFileInfo().getInfoForUrls() + 
	"&" + KConstants.Request.databaseParam + "=" + database;

	String queryLinesSelectAggregatorShowOptionsId="queryLinesSelectAggregatorShowOptions";
	String queryLinesSelectAggregatorHideOptionsId="queryLinesSelectAggregatorHideOptions";
	String chooseAgregatorInfoCellId="queryLines.chooseAgregatorInfoCell";
	String chooseAgregatorCellId = "queryLines.chooseAgregatorCell";
	
	String [] neededType = {KConstants.PrologTypes.rfuzzy_truth_value_type, KConstants.PrologTypes.rfuzzy_truth_value_type, KConstants.PrologTypes.rfuzzy_truth_value_type};
	PredicateInfo [] aggregators = programIntrospection.getPredicatesInfosByType(neededType);
	JspsUtils.getValue(queryLinesCounter);
%>

<% if ((queryLinesCounter == null) || ("".equals(queryLinesCounter)) || ("0".equals(queryLinesCounter))) { %>
	<div class='queryLinesAggregatorTableRow'>
		<div class='queryLinesAggregatorTableCell'>
			<a href="#" onClick="selectQueryAddLine('<%=selectQueryAddLineUrl %>', '<%=selectQueryAddAggrUrl %>');" >
				<img src="images/add.png" width="20" alt="Add more conditions to the query" 
						title="Add more conditions to the query" />
			</a>
		</div>
	</div>
<% } else { %>
	<div class='queryLinesAggregatorTableRow'>
		<div class='queryLinesAggregatorTableCell'>
			<a href="#" onClick="selectQueryAddLine('<%=selectQueryAddLineUrl %>', '<%=selectQueryAddAggrUrl %>');" >
				<img src="images/add.png" width="20" alt="Add more conditions to the query" 
						title="Add more conditions to the query" />
			</a>
		</div>
	</div>

	<div class='queryLinesAggregatorTableRow'>
		<div class='queryLinesAggregatorTableCell'>
			<a id='<%=queryLinesSelectAggregatorShowOptionsId %>' href='' 
				onclick="return aggregatorDetailsShow('<%= queryLinesSelectAggregatorShowOptionsId %>', 
						'<%= queryLinesSelectAggregatorHideOptionsId %>', 
						'<%= chooseAgregatorInfoCellId %>', '<%=chooseAgregatorCellId %>');"> show options</a>
			<a id='<%=queryLinesSelectAggregatorHideOptionsId %>' href='' 
				onclick="return aggregatorDetailsHide('<%=	queryLinesSelectAggregatorShowOptionsId %>', 
						'<%= queryLinesSelectAggregatorHideOptionsId %>', 
						'<%= chooseAgregatorInfoCellId %>', '<%=chooseAgregatorCellId %>');"> hide options</a>
		</div>
	</div>

	<div class='queryLinesAggregatorTableRow'>
		<div class='queryLinesAggregatorTableCell' id=<%= chooseAgregatorInfoCellId %>>
			The aggregator used to combine <br />the subqueries' truth values is:
		</div>
	</div>

	<div class='queryLinesAggregatorTableRow'>
		<div class='queryLinesAggregatorTableCell' id=<%= chooseAgregatorCellId %>>
			<select name='<%= KConstants.QueryParams.queryLinesAggregator %>'>
				<% 
					for (int i=0; i<aggregators.length; i++) {
						String name = aggregators[i].getPredicateName(); 
						if ((name != null) && (! "".equals(name))) {
				%>
							<option 
								<% if ("min".equals(name)) { %>
									selected
								<% } %>
									id='<%=name %>' value='<%=name %>'>
								<%= JspsUtils.getPrologNameInColloquialLanguage(name) %> 
							</option>
					<%	} %>			
				<%	} %>
			</select>
		</div>
	</div>
	
	<script type="text/javascript">
		document.getElementById('<%= queryLinesSelectAggregatorHideOptionsId %>').style.display='none';
		document.getElementById('<%= chooseAgregatorInfoCellId  %>').style.display='none';
		document.getElementById('<%= chooseAgregatorCellId  %>').style.display='none';
		aggregatorDetailsHide('<%=	queryLinesSelectAggregatorShowOptionsId %>', 
				'<%= queryLinesSelectAggregatorHideOptionsId %>', 
				'<%= chooseAgregatorInfoCellId %>', '<%=chooseAgregatorCellId %>');
	</script>
<% } %>

