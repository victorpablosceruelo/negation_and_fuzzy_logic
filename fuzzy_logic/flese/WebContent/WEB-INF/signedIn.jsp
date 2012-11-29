<jsp:include page="commonHtmlHead.jsp" />
<%@page import="java.util.Iterator"%>
<%@page import="auxiliar.FileInfoClass"%>

<%
	@SuppressWarnings("unchecked")
	Iterator<FileInfoClass> filesIterator = (Iterator<FileInfoClass>) request.getAttribute("filesIterator"); 
%>

<body>
   	<jsp:include page="commonBodyHead.jsp" />

	<section id="mainSection" class="">
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

	<script type="text/javascript">
		insertProgramFileSelection('mainSection');
		
		//fillQueryStartupValues("queryStart");
		//changeFormAction("submitQuery", "QueryServlet?op=runQuery&fileName="+currentProgramFileName+"&fileOwner="+currentProgramFileOwner);
		
	</script>
</body>
</html>
