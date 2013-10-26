<%@page import="auxiliar.JspsUtils"%>
<%@page import="constants.KUrls"%>

<%	if (! JspsUtils.isAjax(request)) { %>
	<jsp:include page='../commonHtmlBody.jsp' />
<% } %>

<%
	String localUserInfoName = JspsUtils.getLocalUserInfoName(request);
	String msgsArray = JspsUtils.getResultMessagesInJS(request);
%>

<script type="text/javascript">
	<% if (! "".equals(localUserInfoName)) { %>
		loadAjaxIn('mainSecDiv', "<%=KUrls.Queries.SelectProgramFile.getUrl(true)%>");
	<% } %>		
	<% if ((msgsArray != null) && (msgsArray.length() > 0)) { %>
		showMsgs(<%= msgsArray %>);
	<% } %>
</script>
