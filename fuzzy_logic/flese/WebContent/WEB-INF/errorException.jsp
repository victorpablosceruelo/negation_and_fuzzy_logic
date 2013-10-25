
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="java.util.ArrayList"%>

<%	if (! JspsUtils.isAjax(request)) { %>
	<jsp:include page='commonHtmlBody.jsp' />
<% } %>

<%
ArrayList<String> msgs = new ArrayList<String>();
msgs.add("Ups! An exception occurred.");
msgs.add("You can press the key F5 and try again or send a bug report to vpablos@babel.ls.fi.upm.es");
msgs.add("");
msgs.add(JspsUtils.getExceptionMsg(request));

String msgsArray = JspsUtils.getMessagesInJS(msgs);
%>



<script type="text/javascript">
if (typeof(clearMainSection) == "function") {
	clearMainSection();
}
if (typeof(window.parent.clearMainSection) == "function") {
	window.parent.clearMainSection();
}
if (typeof(clearMsgsSection) == "function") {
	clearMsgsSection();
}
if (typeof(window.parent.clearMsgsSection) == "function") {
	window.parent.clearMsgsSection();
}
if (typeof(showMsgs) == "function") {
	showMsgs(<%= msgsArray %>);
}
if (typeof(window.parent.showMsgs) == "function") {
	window.parent.showMsgs(<%= msgsArray %>);
}
</script>





<!--  END -->