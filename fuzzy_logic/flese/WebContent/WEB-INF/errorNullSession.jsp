
<%@page import="auxiliar.JspsUtils"%>
<%@page import="java.util.ArrayList"%>
<%@page import="constants.KConstants"%>

<%
ArrayList<String> msgs = new ArrayList<String>();
msgs.add(KConstants.AppMsgs.errorSessionNull1);
msgs.add(KConstants.AppMsgs.errorSessionNull2);
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
if (typeof(showMsgsArray) == "function") {
	showMsgsArray(<%= msgsArray %>);
}
if (typeof(window.parent.showMsgsArray) == "function") {
	window.parent.showMsgsArray(<%= msgsArray %>);
}
</script>