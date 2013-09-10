
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
%>


<!-- <form id='queryForm' action='' method='POST' accept-charset='utf-8'>  -->
<!-- 
// action='"+ urlMappingFor('RunQueryRequest') + "&fileName="+fileName+"&fileOwner="+fileOwner + "' ";
// target='" + runQueryTargetiFrameId+ "'>";
 -->
	<div id='queryStartContainer' class='queryStartContainerTable'>
	     <div class='queryStartContainerTableRow'>
	          <div class='queryStartContainerTableCell1'>Your query: I'm looking for a </div>
	          <div class='queryStartContainerTableCell2' id='chooseQueryStartTypeContainerId'>
					<select name="chooseQueryStartType" id="chooseQueryStartType" onchange='selectedQueryStartTypeChanged(this, "queryLinesContainer", "queryLinesCounterField");' >
						<%=JspsUtils.comboBoxDefaultValue() %>
<%
	for (int i=0; i<predicatesInfos.length; i++) {
		String value = predicatesInfos[i].getPredicateName();
%>	
						<option id='<%=value %>' title='<%=value %>' value='<%=value %>'><%=value %></option>
<%
	}
%>					</select>
	          </div>
		 </div>
	</div>

	<!-- Initialize the query lines counter -->	          
	<input type="hidden" name="queryLinesCounterField" value="0" id="queryLinesCounterField">
              
	<div id='queryLinesContainer' class='queryLinesContainerTable'>
	</div>
    
	<div class='searchOrPersonalizeTable'>
		 <div class='searchOrPersonalizeTableRow'>
			  <div class='searchOrPersonalizeTableCell'>
					<input type='submit' value='Search' onclick='return runQueryAfterSoftTests("parentDivId", "runQueryDivId", "chooseQueryStartTypeId", "queryLinesCounterFieldId", "fileName", "fileOwner");' >
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
