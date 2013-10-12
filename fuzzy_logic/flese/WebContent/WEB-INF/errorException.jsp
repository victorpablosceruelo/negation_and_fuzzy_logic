
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="java.util.ArrayList"%>
<%
ArrayList<String> msgs = new ArrayList<String>();
msgs.add("Ups! An exception occurred.");
msgs.add("You can press the key F5 and try again or send a bug report to vpablos@babel.ls.fi.upm.es");
msgs.add("");
		
ResultsStoreHouse resultsStoreHouse = JspsUtils.getResultsStoreHouse(request);
String [] msgsError = resultsStoreHouse.getMessages();
for (int i=0; i<msgsError.length; i++) {
	msgs.add(msgsError[i]);
}

StringBuilder msg = new StringBuilder();
// msg.append("[");
for (int i=0; i<msgs.size(); i++) {
	msg.append("'");
	msg.append(msgs.get(i));
	msg.append("'");
	if (i+1 < msgs.size()) {
		msg.append(", ");
	}
}
// msg.append("]");
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
	window.parent.showMsgs(new Array(<%= msg.toString() %>));
}
</script>





<!--  END -->