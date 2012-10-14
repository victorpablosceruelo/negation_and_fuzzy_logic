<jsp:include page="commonHead.jsp" />
<!-- JavaScript Debugging Code and more -->
<jsp:include page="commonJavaScriptCode.jsp" />

<jsp:include page="commonProgramQuery.jsp" />
<jsp:include page="commonProgramQueryJavaScriptCode.jsp" />

<body>
    <div id="body">
    	<jsp:include page="commonBody.jsp" />
    	<jsp:include page="commonBodyProgramQuery.jsp" />

		<table id="queryStartTable">
			<tr>
				<td><h3>Your query: I'm looking for a </h3></td>
				<td id="queryStart"></td>
			</tr>
		</table>		
		<form id="submitQuery" action="" method="POST" accept-charset="utf-8">
			<div id="simpleOrAdvancedQuery"></div>
     		<div id="queryLines"></div>
	 		<br />
	 		<div id="hiddenNumOfVariables"></div>
     		<input type="button" value="Add more conditions to the query" onClick="addQueryLine('queryLines', quantifiersArray, fuzzyRulesArray);">
     		<INPUT type="submit" value="Execute Query">
		</form>
		<br /><br />
		<h3><a id="complexQuery" href="">Advanced query form</a></h3>
		<br /><br /><br /><br /><br />
	</div>

	<script type="text/javascript">
		fillQueryStartupValues("queryStart");
		changeFormAction("submitQuery", "DataBaseQueryServlet?op=runquery&database="+currentProgramFileName+"&owner="+currentProgramFileOwner);
		changeAHrefLink("complexQuery", "DataBaseQueryServlet?op=advancedQuery&database="+currentProgramFileName+"&owner="+currentProgramFileOwner);
	</script>
</body>
</html>
