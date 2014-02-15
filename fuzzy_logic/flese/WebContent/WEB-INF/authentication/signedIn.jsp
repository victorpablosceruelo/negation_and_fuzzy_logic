<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="constants.KUrls"%>

<%	if (! JspsUtils.isAjax(request)) { %>
<jsp:include page='../commonHtmlBody.jsp' />
<% } %>

<%
	String msgsArray = JspsUtils.getResultMessagesInJS(request);
%>

<script type="text/javascript">
	showMsgsArray(<%= msgsArray %>);
	loadAjaxIn('<%=KConstants.JspsDivsIds.mainSecDivId %>', "<%=KUrls.Queries.SelectProgramFile.getUrl(true)%>");

	// Window onload trigger to load contents after page load.
	// $(window).load(function(){
		/*code goes here*/ 
	// });
</script>
