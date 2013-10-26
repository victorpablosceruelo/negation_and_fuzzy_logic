<%@page import="auxiliar.JspsUtils"%>
<%@page import="java.util.ArrayList"%>
<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>

<%
	String urlList = KUrls.Files.ListMyFiles.getUrl(true);
%>


<script type="text/javascript">
	<%= JspsUtils.loadMessagesAjaxInItsDiv() %>
	loadAjaxIn('<%=KConstants.JspsDivsIds.filesListDiv %>', '<%=urlList %>');	
</script>



