<%@page import="auxiliar.JspsUtils"%>
<%@page import="org.apache.jasper.compiler.JspUtil"%>
<%@page import="constants.KUrls"%>
<%@page import="storeHouse.RequestStoreHouse"%>
<%@page import="java.util.Iterator"%> 
<%@page import="managers.FilesManagerAux"%>
<%@page import="filesAndPaths.ProgramFileInfo"%>

<div id="selectDatabaseContainerDiv" class="selectDatabaseTable">
<%
	
	RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request, false);
	ProgramFileInfo[] filesList = requestStoreHouse.getResultsStoreHouse().getFilesList();

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
			<select name="chooseProgramFileId" id="chooseProgramFileId" onchange='selectedProgramDatabaseChanged(this, "selectQueryDivId", "runQueryDivId")' >
			<%=JspsUtils.comboBoxDefaultValue() %>
<%
	for (int i=0; i<filesList.length; i++) { 
		String value = KUrls.Queries.BuildQuery.getUrl(true) + filesList[i].getInfoForUrls();
		String desc = filesList[i].getFileName() + " ( owned by " + filesList[i].getFileOwner() + " ) ";
%>	
				<option id='<%=value %>' title='<%=value %>' value='<%=value %>'><%=desc %></option>
<%
			}
%>			</select>
		</div>
	</div>
<%
	}
%>
</div>

<div id="selectQueryDivId" class="selectDatabaseTable">
</div>

<div id="runQueryDivId" class="selectDatabaseTable">
</div>
