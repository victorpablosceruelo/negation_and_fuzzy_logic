
<%@page import="prologConnector.PredicateInfo"%>
<%@page import="prologConnector.ProgramIntrospection"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="java.util.ArrayList"%>
<%@page import="java.util.HashMap"%>
<%@page import="prologConnector.CiaoPrologTermInJava"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="constants.KUrls"%>
<%@page import="prologConnector.CiaoPrologQueryAnswer"%>
<%@page import="prologConnector.CiaoPrologProgramIntrospectionQuery"%>
<%@page import="storeHouse.RequestStoreHouse"%>

<%
	RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request, false);
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
	ProgramIntrospection programIntrospection = resultsStoreHouse.getCiaoPrologProgramIntrospection();	
	PredicateInfo [] predicatesInfos = programIntrospection.getPredicatesInfosByMoreInfoKey(KConstants.MoreInfoTypes.database);
	String url1 = KUrls.Queries.SelectQuery.getUrl(true) + programIntrospection.getProgramFileInfo().getInfoForUrls() + 
			"&" + KConstants.Request.databaseParam + "=";
	String url2 = KUrls.Queries.Evaluate.getUrl(true) + programIntrospection.getProgramFileInfo().getInfoForUrls() + 
			"&" + KConstants.Request.databaseParam + "=";
	
%>


<!-- <form id='queryForm' action='' method='POST' accept-charset='utf-8'>  -->
<!-- 
// action='"+ urlMappingFor('RunQueryRequest') + "&fileName="+fileName+"&fileOwner="+fileOwner + "' ";
// target='" + runQueryTargetiFrameId+ "'>";
 -->
	<div id='<%=KConstants.JspsDivsAndFields.queryStartContainerId%>' class='queryStartContainerTable'>
	     <div class='queryStartContainerTableRow'>
	          <div class='queryStartContainerTableCell1'>Your query: I'm looking for a </div>
	          <div class='queryStartContainerTableCell2' id='chooseQueryStartTypeContainerId'>
					<select name="chooseQueryStartType" id="chooseQueryStartType" 
					        onchange='selectedQueryStartTypeChanged(this);' >
						<%=JspsUtils.comboBoxDefaultValue()%>
<%
	for (int i=0; i<predicatesInfos.length; i++) {
		String desc = predicatesInfos[i].getPredicateName();
		String value = url1 + desc;
		// JspsUtils.getValue(value);
%>	
						<option id='<%=desc%>' title='<%=desc%>' value='<%=value%>'><%=desc%></option>
<%
	}
%>					</select>
	          </div>
		 </div>
	</div>

	<!-- Initialize the query lines counter -->	          
	<input type="hidden" name='<%=KConstants.JspsDivsAndFields.counterFieldId %>' value="0" id='<%=KConstants.JspsDivsAndFields.counterFieldId%>'>
              
	<div id='<%=KConstants.JspsDivsAndFields.queryLinesContainerId%>' class='queryLinesContainerTable'>
	</div>
    
	<div id='<%=KConstants.JspsDivsAndFields.searchOrPersonalizeTableId%>' class='searchOrPersonalizeTable'>
		 <div class='searchOrPersonalizeTableRow'>
			  <div class='searchOrPersonalizeTableCell'>
					<input type='submit' value='Search' onclick="return runQueryAfterSoftTests('<%=url2 %>');" >
			  </div>
			  <div class='searchOrPersonalizeTableCell'>&nbsp; or &nbsp;
			  </div>
			  <div class='searchOrPersonalizeTableCell'>
					<INPUT type='submit' value='Personalize Program File' onclick='return personalizeProgramFile(fileName, fileOwner, basic);'>
			  </div>
		</div>
	</div>
	<!--  </form><br />  -->

	<script type="text/javascript">
		selectedQueryStartTypeChanged("");		
	</script>
