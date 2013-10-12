
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="java.util.ArrayList"%>
<%
ArrayList<String> msgs = new ArrayList<String>();
msgs.add("Ups! An exception occurred.");
msgs.add("You can press the key F5 and try again or send a bug report to vpablos@babel.ls.fi.upm.es");
msgs.add("");

String msg = JspsUtils.getMessagesInJS(request, msgs);
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
	showMsgs(new Array(<%= msg %>));
}
if (typeof(window.parent.showMsgs) == "function") {
	window.parent.showMsgs(new Array(<%= msg %>));
}
</script>





<!--  END -->