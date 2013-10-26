<%@page import="constants.KConstants"%>
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
// $(window).load(function(){
	// alert("Pffff");
	<% if ((msgsArray != null) && (msgsArray.length() > 0)) { %>
		showMsgsArray(<%= msgsArray %>);
	<% } %>
	// alert("Do you see the message? It is loadAjaxIn the function that kills it !!!");
	<% if (! "".equals(localUserInfoName)) { %>
		loadAjaxIn('<%=KConstants.JspsDivsIds.mainSecDivId %>', "<%=KUrls.Queries.SelectProgramFile.getUrl(true)%>");
	<% } %>
	/*code goes here*/ 
// });
</script>
