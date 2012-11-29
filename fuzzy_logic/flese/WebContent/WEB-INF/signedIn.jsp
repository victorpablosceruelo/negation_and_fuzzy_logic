<jsp:include page="commonHtmlHead.jsp" />
<%@page import="java.util.Iterator"%>
<%@page import="auxiliar.FileInfoClass"%>

<%
	@SuppressWarnings("unchecked")
	Iterator<FileInfoClass> filesIterator = (Iterator<FileInfoClass>) request.getAttribute("filesIterator"); 
%>

<body>
    <div id="body">
    	<jsp:include page="commonBodyHead.jsp" />
		<section id="selectDatabase" class="selectDatabase">
			Select a program file to perform your query: 
			<select name="selectedDatabase" onchange='selectedProgramDatabaseChanged(this, "selectQuery")' >
				<option id="----" value="----">----</option>
				<%
					while (filesIterator.hasNext()) {
						FileInfoClass current = filesIterator.next();
				%>
				<option id="<%=current.getFileName() + "-owned-by-" + current.getFileOwner()%>"
						value="<%=current.getFileName() + "-owned-by-" + current.getFileOwner()%>">
						<%=current.getFileName() + " ( owned by " + current.getFileOwner() + " ) "%>
				</option>
				<%
					}
				%>
			</select>
		</section>
		<section id="selectQuery" class="selectQuery">
		</section>
		<section id="showResults" class="showResults">
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
