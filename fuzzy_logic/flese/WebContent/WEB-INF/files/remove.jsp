<%@page import="auxiliar.JspsUtils"%>
<%@page import="java.util.ArrayList"%>
<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>

<%
	String urlList = KUrls.Files.ListMyFiles.getUrl(true);
	String msgsArray = JspsUtils.getResultMessagesInJS(request);
%>


<script type="text/javascript">
	<% if ((msgsArray != null) && (msgsArray.length() > 0)) { %>
		showMsgs(<%= msgsArray %>);
	<% } %>
	loadAjaxIn('<%=KConstants.JspsDivsIds.filesListDiv %>', '<%=urlList %>');	
</script>



