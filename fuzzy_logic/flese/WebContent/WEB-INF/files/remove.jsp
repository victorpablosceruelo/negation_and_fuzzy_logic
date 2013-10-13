<%@page import="auxiliar.JspsUtils"%>
<%@page import="java.util.ArrayList"%>
<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>

<%
ArrayList<String> msgs = new ArrayList<String>();
String msg = JspsUtils.getMessagesInJS(request, msgs);
%>

<% if ((msg != null) && (msg.length() > 0)) { %>
	<script type="text/javascript">
		showMsgs(new Array(<%= msg %>));
	</script>
<% } else { %>
	<script type="text/javascript">
		loadAjaxIn('<%=KConstants.JspsDivsIds.filesListDiv %>', '<%=KUrls.Files.List.getUrl(true) %>');
	</script>
<% } %>



