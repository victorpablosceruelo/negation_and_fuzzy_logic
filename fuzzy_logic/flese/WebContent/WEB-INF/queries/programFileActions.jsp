
<%@page import="filesAndPaths.ProgramFileInfo"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="storeHouse.ResultsStoreHouse"%>
<%@page import="constants.KUrls"%>
<%@page import="storeHouse.RequestStoreHouse"%>


<%
	RequestStoreHouse requestStoreHouse = JspsUtils.getRequestStoreHouse(request);
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(requestStoreHouse);
	ProgramFileInfo programFileInfo = resultsStoreHouse.getProgramFileInfo();
	
	if (programFileInfo != null) {
		String urlFileView = KUrls.Files.View.getUrl(true);
		String params = "&" + KConstants.Request.fileOwnerParam + "=" + programFileInfo.getFileOwner() +
				"&" + KConstants.Request.fileNameParam + "=" + programFileInfo.getFileName() + 
				"&" + KConstants.Request.mode + "=" + KConstants.Request.modeBasic;
	
%>
<a href='#'
	onclick='fileViewAction("<%=KConstants.JspsDivsIds.fileViewContentsDiv %>", "<%=urlFileView%>", "<%=params %>", "<%=programFileInfo.getFileName() %>");'
	title='view program file <%= programFileInfo.getFileName() %>'>View
</a>

<% } %>