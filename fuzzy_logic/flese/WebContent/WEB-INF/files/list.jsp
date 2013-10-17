<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="filesAndPaths.ProgramFileInfo"%>
<%@page import="storeHouse.RequestStoreHouse"%>
<%@page import="java.util.*"%>
<%@page import="java.io.*"%>

<%
	RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request);
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
	ProgramFileInfo[] filesList = resultsStoreHouse.getFilesList();

	String urlFileView = KUrls.Files.View.getUrl(true);
	String urlListFuzzifications = KUrls.Fuzzifications.List.getUrl(true);
	String urlReloadPage = KUrls.User.Options.getUrl(true);
	String urlFileRemoval = KUrls.Files.Remove.getUrl(true);
	
	if (filesList.length > 0) { 
%>
	<div class='filesListTableRow'>
		<div class='filesListTableCell'>Program File Name</div>
		<div class='filesListTableCell'></div>
		<div class='filesListTableCell'></div>
	</div>		
<%	}
	else {
%>
You do not owe any program file. Upload one by using the facility below.
<%	
	}
	for(int i=0; i<filesList.length; i++) { 
		String params = "&" + KConstants.Request.fileOwnerParam + "=" + filesList[i].getFileOwner() +
								"&" + KConstants.Request.fileNameParam + "=" + filesList[i].getFileName() + 
								"&" + KConstants.Request.mode + "=" + KConstants.Request.modeAdvanced;
%>
	<div class='filesListTableRow'>
		<div class='filesListTableCell'>
		<a href='#' onclick='fileViewAction("<%=KConstants.JspsDivsIds.fileViewContentsDiv %>", "<%=urlFileView%>", "<%=params %>", "<%=filesList[i].getFileName() %>");'
					title='view program file <%= filesList[i].getFileName() %>'><%=filesList[i].getFileName() %></a>
		</div>
		<div class='filesListTableCell'>   					
		<a href='#' onclick='removeFileAction("<%= urlFileRemoval %>", "<%= params %>");' 
	   				title='remove program file <%= filesList[i].getFileName() %>' >
	   				<img src='images/remove-file.gif' width='20em'></a>
	   	</div>
	   	<div class='filesListTableCell'>	 
		<a href='#' onclick='return personalizeProgramFile("<%=urlListFuzzifications%>", "<%= params %>", "<%=filesList[i].getFileName() %>");' 
					title='personalize program file <%= filesList[i].getFileName() %>' >
					<img src='images/edit.png' width='20em'></a>
		</div>
	</div>
<%  }  %>

<div id='<%=KConstants.JspsDivsIds.fileViewContentsDiv %>' class='filesListTable' style='display:none;' >
</div>





<!-- END -->
