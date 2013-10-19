<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="storeHouse.RequestStoreHouse"%>

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
			loadAjaxIn('<%=KConstants.JspsDivsIds.filesListDiv %>', '<%=KUrls.Files.List.getUrl(true) %>');
		}
		if (typeof(window.parent.loadAjaxIn) == "function") {
			window.parent.loadAjaxIn('<%=KConstants.JspsDivsIds.filesListDiv %>', '<%=KUrls.Files.List.getUrl(true) %>');
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
