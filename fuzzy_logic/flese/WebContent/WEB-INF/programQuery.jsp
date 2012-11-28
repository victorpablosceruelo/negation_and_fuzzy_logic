<jsp:include page="commonHtmlHead.jsp" />

<body>
    <div id="body">
    	<jsp:include page="commonBodyHead.jsp" />
		<section id="selectDatabase" class="selectDatabase">
			Select a program file to perform your query: 
			<select name="selectedDatabase">
				<option id="----" value="----">----</option>
			</select>
		</section>
		
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
     		<INPUT type="submit" value="Execute Query" onclick='return testQueryValidity();'>
		</form>
		<br /><br /><br /><br /><br />
	</div>

	<script type="text/javascript">
		//fillQueryStartupValues("queryStart");
		//changeFormAction("submitQuery", "QueryServlet?op=runQuery&fileName="+currentProgramFileName+"&fileOwner="+currentProgramFileOwner);
	</script>
</body>
</html>
