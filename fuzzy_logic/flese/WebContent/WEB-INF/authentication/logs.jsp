<%@page import="java.util.ArrayList"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="constants.KUrls"%>

<%	if (! JspsUtils.isAjax(request)) { %>
	<jsp:include page='../commonHtmlBody.jsp' />
<% } %>

<h3>
	FleSe Logs
</h3>

<textarea id="logs1" cols="15" rows="15" name="logs1" readonly="readonly"> 
<%= JspsUtils.getLogsQueries() %> 
</textarea>

<textarea id="logs1" cols="15" rows="15" name="logs1" readonly="readonly"> 
<%= JspsUtils.getLogsSignedUsers() %>
</textarea>