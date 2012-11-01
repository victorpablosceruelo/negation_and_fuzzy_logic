<jsp:include page="commonHtmlHead.jsp" />
<jsp:include page="commonProgramQuery.jsp" />

<body>
    <div id="body">
    	<jsp:include page="commonBodyHead.jsp" />
    	<%@page import="auxiliar.ServletsAuxMethodsClass"%>
		<h3><a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt; Perform a query </h3>
    	<jsp:include page="commonBodyProgramQuery.jsp" />

		<form id="submitQuery" action="" method="POST" accept-charset="utf-8">
			<table id="queryStartTable" class="withoutBorder">
				<tr>
					<td><h3>Your query: I'm looking for a </h3></td>
					<td id="queryStart"></td>
				</tr>
			</table>
			<br />		
		
			<input type="hidden" name="queryLinesCounter" value="0" id="queryLinesCounter"> 
			<div id="queryLinesDiv"></div>
	 		<br />
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
