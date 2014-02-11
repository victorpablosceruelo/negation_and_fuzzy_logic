<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="results.ResultsStoreHouse"%>
<%@page import="storeHouse.RequestStoreHouse"%>

<%
	String ontologyUrlFieldId = KConstants.JspsDivsIds.ontologyUrlFieldId;
	String ontologyQueryResultsDivId = KConstants.JspsDivsIds.ontologyQueryResultsDivId;
	String queryUrl = KUrls.Ontologies.MainQuery.getUrl(true);
%>

<form action="#">

Enter the ontology url <br>
<input type="text" name="<%=ontologyUrlFieldId%>" id="<%=ontologyUrlFieldId%>">

<input type="button" value="Process Ontology"
	onclick="sendOntologyMainQuery('<%=ontologyUrlFieldId%>', '<%=queryUrl%>', '<%=ontologyQueryResultsDivId%>');">

</form>

<div id="<%=ontologyQueryResultsDivId%>">
</div>

<script type="text/javascript">
	// loadAjaxIn('<%=KConstants.JspsDivsIds.ontologyStartDivId %>', '<%=KUrls.Files.UploadDiv.getUrl(true) %>');
</script>

<!-- END -->
