<jsp:include page="commonHead.jsp" />
<!-- JavaScript Debugging Code and more -->
<jsp:include page="commonJavaScriptCode.jsp" />

<jsp:include page="commonProgramQuery.jsp" />
<jsp:include page="commonProgramQueryJavaScriptCode.jsp" />

<body>
    <div id="body">
    	<jsp:include page="commonBody.jsp" />
    	<%@page import="auxiliar.ServletsAuxMethodsClass"%>
		<h3><a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt; Perform a query </h3>
    	<jsp:include page="commonBodyProgramQuery.jsp" />

		<table id="queryStartTable">
			<tr>
				<td><h3>Your query: I'm looking for a </h3></td>
				<td id="queryStart"></td>
			</tr>
		</table>
		<br />		
		<form id="submitQuery" action="" method="POST" accept-charset="utf-8">
			<div id="simpleOrAdvancedQuery"></div>
			<div id="queryLinesDiv"></div>
	 		<br />
	 		<div id="hiddenNumOfVariables"></div>
     		<INPUT type="submit" value="Execute Query">
		</form>
		<br /><br /><br /><br /><br />
	</div>

	<script type="text/javascript">
		fillQueryStartupValues("queryStart");
		changeFormAction("submitQuery", "QueryServlet?op=runQuery&fileName="+currentProgramFileName+"&fileOwner="+currentProgramFileOwner);
	</script>
</body>
</html>
