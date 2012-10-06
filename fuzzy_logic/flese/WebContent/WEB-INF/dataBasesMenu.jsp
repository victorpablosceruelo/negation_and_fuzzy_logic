<jsp:include page="commonHead.jsp" />
<!-- JavaScript Debugging Code and more -->
<jsp:include page="commonJavaScriptCode.jsp" />

<%@page import="servlets.*"%>
<%@page import="java.util.ArrayList"%>
<%@page import="java.util.Iterator"%>
<%@page import="auxiliar.FoldersUtilsClass"%>
<%@page import="auxiliar.DataBaseInfoClass"%>
<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>

<body>
    <div id="body">
    	<jsp:include page="commonBody.jsp" />
	
	<%
		String localUserName = (String) session.getAttribute("localUserName");

		FoldersUtilsClass workingFolder = new FoldersUtilsClass();
			Iterator<DataBaseInfoClass> databasesIterator = null;
			if (workingFolder != null) {
		databasesIterator = workingFolder.returnDatabasesIterator(localUserName);
			}
			/*
			else {
		out.print("<h4>Error in application: working folder should not be null</h4>");
			}*/
			if (databasesIterator != null) {
	%>
	<h3>Choose an existing program file for querying, viewing or remove</h3>
	<div id="infoBox">&nbsp;</div>
	<br />
		<table>
				<tr>
					<td>File Name</td>
					<td>File Owner</td>
					<td>&nbsp;</td>
					<td>&nbsp;</td>
				</tr>
	<%
			while (databasesIterator.hasNext()) {
				DataBaseInfoClass dataBaseInfo = databasesIterator.next();
				%>
				<tr>
					<td>
						<a onmouseover='showInfoMessage("infoBox", "view program file <%=dataBaseInfo.getDataBaseName() %>")'
							onmouseout='showInfoMessage("infoBox", "")'
							href="FilesMgmtServlet?op=view&database=<%=dataBaseInfo.getDataBaseName()%>&owner=<%=dataBaseInfo.getDataBaseOwner()%>">
						<%=dataBaseInfo.getDataBaseName() %></a>
					</td>
					<td><%=dataBaseInfo.getDataBaseOwner() %></td>
					<td><a onmouseover='showInfoMessage("infoBox", "remove program file <%=dataBaseInfo.getDataBaseName() %>")'
							onmouseout='showInfoMessage("infoBox", "")'
							href="FilesMgmtServlet?op=remove&database=<%=dataBaseInfo.getDataBaseName()%>&owner=<%=dataBaseInfo.getDataBaseOwner()%>">
							<img src="remove-file.gif" alt="remove" width="20"></img></a></td>
					<td><a onmouseover='showInfoMessage("infoBox", "query program file <%=dataBaseInfo.getDataBaseName() %>")'
							onmouseout='showInfoMessage("infoBox", "")'
							href="DataBaseQueryServlet?op=query&database=<%=dataBaseInfo.getDataBaseName()%>&owner=<%=dataBaseInfo.getDataBaseOwner()%>">
							Query</a></td>
				</tr>
				<%
			}
	%>
		</table>
	<% } %>

		<script type="text/javascript">
			function unhideUpload() {
				var html = "<INPUT TYPE=\'submit\' VALUE=\'Upload File\'>"
				document.getElementById('uploadButton').innerHTML = html;	
			}
		</script>

		<h3>Upload a new database</h3>
		<FORM ENCTYPE='multipart/form-data' method='POST' action="FilesMgmtServlet?op=upload">
			<INPUT TYPE='file' NAME='fuzzy-database' size="50" onchange="unhideUpload('uploadButton');">
			<DIV id='uploadButton'> 
			</DIV>
		</FORM>
	</div>	
</body>
</html>
