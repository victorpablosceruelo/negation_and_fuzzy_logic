<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="storeHouse.RequestStoreHouse"%>

<%
	String urlList = KUrls.Files.ListMyFiles.getUrl(true);
%>

<script type="text/javascript">
<%RequestStoreHouse requestStoreHouse = new RequestStoreHouse(request);
	ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);

	String [] msgs = resultsStoreHouse.getExceptionMessages();
	if ((msgs != null) && (msgs.length > 0)) { 

		out.print("window.parent.fileUploadShowResults('");
		out.print(KConstants.JspsDivsIds.uploadStatusDivId);
		out.print("', [");

		for (int i=0; i<msgs.length; i++) {
			out.print("'" + msgs[i] +"'");
			if (i+1 < msgs.length) {
				out.print(", ");
			}
		}
		
		out.print("]);");
	}
	else {%>
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
