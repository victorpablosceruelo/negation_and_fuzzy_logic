<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="storeHouse.RequestStoreHouse"%>

<%
	String urlList = KUrls.Files.ListMyFiles.getUrl(true);
	String msgsArray = JspsUtils.getResultMessagesInJS(request);
%>

<script type="text/javascript">
	// Update the files list.
	if (typeof(loadAjaxIn) == "function") {
		loadAjaxIn('<%=KConstants.JspsDivsIds.filesListDiv %>', '<%=urlList %>');
	}
	if (typeof(window.parent.loadAjaxIn) == "function") {
		window.parent.loadAjaxIn('<%=KConstants.JspsDivsIds.filesListDiv %>', '<%=urlList %>');
	}
	// Clean the status div.
	if (typeof(fileUploadShowResults) == "function") {
		fileUploadShowResults("<%=KConstants.JspsDivsIds.uploadStatusDivId%>", <%= msgsArray %>);
	}
	if (typeof(window.parent.fileUploadShowResults) == "function") {
		window.parent.fileUploadShowResults("<%=KConstants.JspsDivsIds.uploadStatusDivId%>", <%= msgsArray %>);
	}
</script>





<!-- END -->
