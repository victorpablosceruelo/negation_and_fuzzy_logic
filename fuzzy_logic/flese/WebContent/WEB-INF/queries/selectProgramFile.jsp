<%@page import="constants.KConstants"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="constants.KUrls"%>
<%@page import="storeHouse.RequestStoreHouse"%>
<%@page import="java.util.Iterator"%> 
<%@page import="managers.FilesManagerAux"%>
<%@page import="filesAndPaths.ProgramFileInfo"%>

<div id="selectDatabaseContainerDiv" class="selectDatabaseTable">
<%
	RequestStoreHouse requestStoreHouse = JspsUtils.getRequestStoreHouse(request);
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
	ProgramFileInfo[] filesList = resultsStoreHouse.getFilesList();

	String urlSelectQueryStartType = KUrls.Queries.SelectQueryStartType.getUrl(true);
	String urlProgramFileActions = KUrls.Queries.ProgramFileActions.getUrl(true);
	
	if (filesList.length == 0) {
%>
	<div class="selectDatabaseTableRow">
		<div class="selectDatabaseTableCell">
			No databases. Please upload one via your user options.
		</div>
	</div>
<%
	} else {
%>
	<div class="selectDatabaseTableRow">
		<div class="selectDatabaseTableCell1">
			Please, select a database to load: 
		</div>
		<div class="selectDatabaseTableCell2">
			<select name="<%=KConstants.Request.programParam %>" 
					id="<%=KConstants.Request.programParam %>" 
					onchange="selectedProgramDatabaseChanged(this, '<%=urlSelectQueryStartType %>', '<%=urlProgramFileActions %>')" >
			<%=JspsUtils.comboBoxDefaultValue()%>
<%
	for (int i=0; i<filesList.length; i++) { 
		String value = filesList[i].getInfoForUrls();
		String desc = filesList[i].getFileName() + " ( owned by " + filesList[i].getFileOwner() + " ) ";
%>	
				<option id='<%=value%>' title='<%=value%>' value='<%=value%>'><%=desc%></option>
<%
	}
%>			</select>
		</div>
		<div class='selectDatabaseTableCell3' id='<%=KConstants.JspsDivsIds.programFileActionsContainerId%>'>
		</div>
	</div>
<%
	}
%>
</div>

<div id="<%=KConstants.JspsDivsIds.selectQueryDivId%>" class="selectDatabaseTable">
</div>

<div id="<%=KConstants.JspsDivsIds.runQueryDivId%>" class="selectDatabaseTable">
</div>



<!--  EOF -->