
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="java.util.ArrayList"%>

<%	if (! JspsUtils.isAjax(request)) { %>
	<jsp:include page='commonHtmlBody.jsp' />
<% } %>

<%
ArrayList<String> msgs = new ArrayList<String>();
msgs.add(KConstants.AppMsgs.exception1);
msgs.add(KConstants.AppMsgs.exception2 + KConstants.Application.AppBugsEmail + KConstants.AppMsgs.exception3);
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
if (typeof(window.close) == "function") {
	window.close()
}
</script>





<!--  END -->