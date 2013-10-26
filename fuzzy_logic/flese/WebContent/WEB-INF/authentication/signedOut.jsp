<%@page import="auxiliar.JspsUtils"%>
<%@page import="constants.KUrls"%>

<%	if (! JspsUtils.isAjax(request)) { %>
	<jsp:include page='../commonHtmlBody.jsp' />
<% } %>
<%
	String localUserInfoName = JspsUtils.getLocalUserInfoName(request);
%>

<script type="text/javascript">	
	<%= JspsUtils.loadMessagesInItsDiv() %>
	<% if ("".equals(localUserInfoName)) { %>
		loadAjaxIn('mainSecDiv', "<%=KUrls.Auth.Providers.getUrl(true)%>");
	<% } %>	
</script>

