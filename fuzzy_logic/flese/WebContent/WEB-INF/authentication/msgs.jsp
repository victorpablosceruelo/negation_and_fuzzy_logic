<%@page import="auxiliar.JspsUtils"%>
<%@page import="constants.KUrls"%>

<%	if (! JspsUtils.isAjax(request)) { %>
	<jsp:include page='../commonHtmlBody.jsp' />
<% } %>


<%
	String [] msgs = JspsUtils.getResultMessages(request);

	out.println("<BR>");
	for (int i=0; i<msgs.length; i++) {
		out.println(msgs[i]);
		out.println("<BR>");
	}
%>


