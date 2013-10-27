<%@page import="auxiliar.JspsUtils"%>
<%@page import="java.util.ArrayList"%>
<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>

<%
	String urlList = KUrls.Files.ListMyFiles.getUrl(true);
	String msgsArray = JspsUtils.getResultMessagesInJS(request);
%>


<script type="text/javascript">
	showMsgsArray(<%= msgsArray %>);
	loadAjaxIn('<%=KConstants.JspsDivsIds.filesListDiv %>', '<%=urlList %>');	
</script>



