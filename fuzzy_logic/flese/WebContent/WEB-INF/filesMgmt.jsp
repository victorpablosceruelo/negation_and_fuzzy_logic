<jsp:include page="commonHead.jsp" />
<!-- JavaScript Debugging Code and more -->
<jsp:include page="commonJavaScriptCode.jsp" />

<%@page import="servlets.*"%>
<%@page import="java.util.ArrayList"%>
<%@page import="java.util.Iterator"%>
<%@page import="auxiliar.FoldersUtilsClass"%>
<%@page import="auxiliar.DataBaseInfoClass"%>
<%@page import="auxiliar.ServletsAuxMethodsClass"%>
<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>

<body>
    <div id="body">
    	<jsp:include page="commonBody.jsp" />
	
		<script type="text/javascript">
			var servlet="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>";
			function lineForProgramFile(tableId, fileName, fileOwner) {
				debug.info("lineForProgramFile: fileName: " + fileName);
				debug.info("lineForProgramFile: fileOwner: " + fileOwner);
				var table = document.getElementById(tableId);
				var rowCount = table.rows.length;
				var row = table.insertRow(rowCount);

				var cell1 = row.insertCell(0);
				var element1 = document.createElement('a');
				element1.setAttribute('href', servlet + "?op=view&database=" + fileName + "&owner=" + fileOwner);
				element1.setAttribute('title', fileName);
				element1.setAttribute('onmouseover', showInfoMessage('infoBox', 'view program file ' + fileName));
				element1.setAttribute('onmouseout', showInfoMessage('infoBox', ''));
				cell1.appendChild(element1);
				
				var cell2 = row.insertCell(1);
				cell2.innerHTML=fileOwner;
				
				var cell3 = row.insertCell(2);
				var element2 = document.createElement('a');
				element2.setAttribute('href', servlet + "?op=remove&database=" + fileName + "&owner=" + fileOwner);
				var element3 = document.createElement("image");
				element3.setAttribute('src', 'remove-file.gif');
				element3.setAttribute('alt', 'remove');
				element3.setAttribute('width', '20');
				element2.setAttribute('title', element3);
				element2.setAttribute('onmouseover', showInfoMessage('infoBox', 'remove program file ' + fileName));
				element2.setAttribute('onmouseout', showInfoMessage('infoBox', ''));
				cell3.appendChild(element2);
				
				var cell4 = row.insertCell(3);
				var element4 = document.createElement('a');
				element4.setAttribute('href', servlet + "?op=query&database=" + fileName + "&owner=" + fileOwner);
				element4.setAttribute('title', 'Query');
				element4.setAttribute('onmouseover', showInfoMessage('infoBox', 'query program file ' + fileName));
				element4.setAttribute('onmouseout', showInfoMessage('infoBox', ''));
				cell4.appendChild(element4);
			}
		</script>
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
		<table id='files'>
				<tr>
					<td>File Name</td>
					<td>File Owner</td>
					<td>&nbsp;</td>
					<td>&nbsp;</td>
				</tr>
		</table>
	<%			while (databasesIterator.hasNext()) {
					DataBaseInfoClass dataBaseInfo = databasesIterator.next();
				%>
				<script type="text/javascript">
					lineForProgramFile('files', "<%=dataBaseInfo.getDataBaseName()%>", "<%=dataBaseInfo.getDataBaseOwner()%>");
				</script>

				<%
				}
			}
	%>
	
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
