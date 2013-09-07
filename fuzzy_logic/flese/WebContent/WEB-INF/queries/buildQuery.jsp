
<%@page import="prologConnector.CiaoPrologQueryAnswer"%>
<%@page import="prologConnector.CiaoPrologProgramIntrospectionQuery"%>
<%@page import="storeHouse.RequestStoreHouse"%>

<%
	
	RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request, false);
	CiaoPrologQueryAnswer [] queryAnswers = requestStoreHouse.getResultsStoreHouse().getCiaoPrologQueryAnswers();	
	
%>


<!-- <form id='queryForm' action='' method='POST' accept-charset='utf-8'>  -->
<!-- 
// action='"+ urlMappingFor('RunQueryRequest') + "&fileName="+fileName+"&fileOwner="+fileOwner + "' ";
// target='" + runQueryTargetiFrameId+ "'>";
 -->
	<div id='queryStartContainer' class='queryStartContainerTable'>
	     <div class='queryStartContainerTableRow'>
	          <div class='queryStartContainerTableCell1'>Your query: I'm looking for a </div>
	               <div class='queryStartContainerTableCell2' id='chooseQueryStartTypeContainerId'></div>
	               </div>
	          </div>
	          
	          <!-- Initialize query lines counter -->
              <input type='hidden' name='queryLinesCounterFieldId' value='0' id='queryLinesCounterFieldId'>
              
              <div id='"+ queryLinesContainerId +"' class='"+queryLinesContainerId+"Table'></div>
                   <div class='searchOrPersonalizeTable'>
                        <div class='searchOrPersonalizeTableRow'>
                             <div class='searchOrPersonalizeTableCell'>
	                              <input type='submit' value='Search' 
	                              onclick='return runQueryAfterSoftTests("parentDivId", "runQueryDivId", "chooseQueryStartTypeId", "queryLinesCounterFieldId", "fileName", "fileOwner");' >
	</div>
	<div class='searchOrPersonalizeTableCell'>&nbsp; or &nbsp;</div>
	<div class='searchOrPersonalizeTableCell'>
	<INPUT type='submit' value='Personalize Program File' onclick='return personalizeProgramFile(\"" + fileName + "\", \"" + fileOwner + "\", \"basic\");'>
	</div>
	</div>
	</div>
	<!--  </form><br />  -->


