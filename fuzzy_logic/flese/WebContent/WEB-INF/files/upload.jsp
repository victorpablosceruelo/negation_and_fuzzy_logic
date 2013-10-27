<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="storeHouse.RequestStoreHouse"%>

<%
	String urlList = KUrls.Files.ListMyFiles.getUrl(true);
	String [] msgs = JspsUtils.getResultMessages(request);
	String msgsArray = JspsUtils.getMessagesInJS(msgs);
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
	if (typeof(showMsgsArrayInDiv) == "function") {
		showMsgsArrayInDiv("<%=KConstants.JspsDivsIds.uploadStatusDivId%>", <%=msgsArray%>);
	}
	if (typeof(window.parent.showMsgsArrayInDiv) == "function") {
		window.parent.showMsgsArrayInDiv("<%=KConstants.JspsDivsIds.uploadStatusDivId%>", <%=msgsArray%>);
	}
	// Clean the messages div.
	if (typeof(showMsgsArray) == "function") {
		showMsgsArray(<%=JspsUtils.getEmptyArrayMessagesInJs()%>);
	}
	if (typeof(window.parent.showMsgsArray) == "function") {
		window.parent.showMsgsArray(<%=JspsUtils.getEmptyArrayMessagesInJs()%>);
	}	
</script>





<!-- END -->
