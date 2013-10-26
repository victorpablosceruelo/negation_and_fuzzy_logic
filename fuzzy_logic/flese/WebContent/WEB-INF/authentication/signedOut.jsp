<%@page import="auxiliar.JspsUtils"%>
<%@page import="constants.KUrls"%>

<%	if (! JspsUtils.isAjax(request)) { %>
	<jsp:include page='../commonHtmlBody.jsp' />
<% } %>
<%
	String localUserInfoName = JspsUtils.getLocalUserInfoName(request);
	String [] msgs = JspsUtils.getResultMessages(request);
	String msgsArray = JspsUtils.getMessagesInJS(msgs);
%>

<script type="text/javascript">	
	<% if ((msgsArray != null) && (msgsArray.length() > 0)) { %>
		showMsgsArray(<%= msgsArray %>);
	<% } %>
	<% if ("".equals(localUserInfoName)) { %>
		loadAjaxIn('mainSecDiv', "<%=KUrls.Auth.Providers.getUrl(true)%>");
	<% } %>	
</script>

