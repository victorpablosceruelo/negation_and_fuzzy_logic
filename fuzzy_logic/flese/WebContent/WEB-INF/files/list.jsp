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
			Please, choose a database to load: 
		</div>
		<div class="selectDatabaseTableCell2">
			<select name="chooseProgramFileId" id="chooseProgramFileId" onchange='selectedProgramDatabaseChanged(this, "parentDivId")' >
			<option id='choose' title='choose' value=''>choose</option>
<%
			for (int i=0; i<filesList.length; i++) { 
				String name = filesList[i].getFileName() + "-owned-by-" + filesList[i].getFileOwner();
				String id = filesList[i].getFileName() + "-owned-by-" + filesList[i].getFileOwner();
				String desc = filesList[i].getFileName() + " ( owned by " + filesList[i].getFileOwner() + " ) ";
%>	
				<option id='<%=id %>' title='<%=name %>' value='<%=id %>'><%=desc %></option>
<%
			}
%>			</select>
		</div>
	</div>
<%
	}
%>

</div>