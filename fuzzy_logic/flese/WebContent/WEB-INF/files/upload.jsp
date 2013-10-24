<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="storeHouse.RequestStoreHouse"%>

<%
	String urlList = KUrls.Files.ListMyFiles.getUrl(true);
	String msgsArray = JspsUtils.getResultMessageInJS(request);
%>

<script type="text/javascript">
<%
	
	if ((msgsArray != null) && (msgsArray.length() > 0)) { 
		out.print("window.parent.fileUploadShowResults('");
		out.print(KConstants.JspsDivsIds.uploadStatusDivId);
		out.print("', " + msgsArray + ");");
	}
	else {
%>
		// Update the files list.
		if (typeof(loadAjaxIn) == "function") {
			loadAjaxIn('<%=KConstants.JspsDivsIds.filesListDiv %>', '<%=urlList %>');
		}
		if (typeof(window.parent.loadAjaxIn) == "function") {
			window.parent.loadAjaxIn('<%=KConstants.JspsDivsIds.filesListDiv %>', '<%=urlList %>');
		}
		if (typeof(fileUploadCleanStatusDiv) == "function") {
			fileUploadCleanStatusDiv("<%=KConstants.JspsDivsIds.uploadStatusDivId%>");
		}
		if (typeof(window.parent.fileUploadCleanStatusDiv) == "function") {
			window.parent.fileUploadCleanStatusDiv("<%=KConstants.JspsDivsIds.uploadStatusDivId%>");
		}
<%	}  %>
</script>





<!-- END -->
