<%@page import="auxiliar.JspsUtils"%>
<%@page import="java.util.ArrayList"%>
<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>

<%
	String urlList = KUrls.Files.ListMyFiles.getUrl(true);
	String msgsArray = JspsUtils.getResultMessageInJS(request);
%>

<% if ((msgsArray != null) && (msgsArray.length() > 0)) { %>
	<script type="text/javascript">
		showMsgs(<%= msgsArray %>);
	</script>
<% } else { %>
	<script type="text/javascript">
		loadAjaxIn('<%=KConstants.JspsDivsIds.filesListDiv %>', '<%=urlList %>');
	</script>
<% } %>



