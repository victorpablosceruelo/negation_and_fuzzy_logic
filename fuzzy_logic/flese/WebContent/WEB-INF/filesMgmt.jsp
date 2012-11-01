<jsp:include page="commonHtmlHead.jsp" />

<%@page import="servlets.*"%>
<%@page import="java.util.ArrayList"%>
<%@page import="java.util.Iterator"%>
<%@page import="auxiliar.FoldersUtilsClass"%>
<%@page import="auxiliar.FileInfoClass"%>
<%@page import="auxiliar.ServletsAuxMethodsClass"%>
<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>

<body>
    <div id="body">
    	<jsp:include page="commonBodyHead.jsp" />
	
		<script type="text/javascript">
			var filesMgmtServlet="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null)%>";
			var queryServlet="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.QueryServletBuildQuery, request, null)%>";
			
			function lineForProgramFile(tableId, fileName, fileOwner) {
				debug.info("lineForProgramFile: fileName: " + fileName);
				debug.info("lineForProgramFile: fileOwner: " + fileOwner);
				var table = document.getElementById(tableId);
				table.className = "programFilesList";
				var rowCount = table.rows.length;
				var row = table.insertRow(rowCount);
				row.className = "programFilesList";

				var cell1 = row.insertCell(0);
				cell1.innerHTML = "<a href='"+filesMgmtServlet+
						"?op=view&fileName="+fileName+"&fileOwner="+fileOwner+"' "+
						"onmouseover='showInfoMessage(\"infoBox\", \"view program file "+fileName+"\")' "+
						"onmouseout='showInfoMessage(\"infoBox\", \"&nbsp;\")'>"+fileName+"</a> ";
				
				var cell2 = row.insertCell(1);
				cell2.innerHTML=fileOwner;
				
				var cell3 = row.insertCell(2);
				cell3.innerHTML = "<a href='"+filesMgmtServlet+
						"?op=remove&fileName="+fileName+"&fileOwner="+fileOwner+"' "+
						"onmouseover='showInfoMessage(\"infoBox\", \"remove program file "+fileName+"\")' "+
						"onmouseout='showInfoMessage(\"infoBox\", \"&nbsp;\")'"+
						"onclick='return confirm(\"Are you sure you want to delete program file "+fileName+"?\")'>"+
						"<img src=\"remove-file.gif\" alt=\"remove\" width=\"20\"></img></a> ";
				
				var cell4 = row.insertCell(3);
				cell4.innerHTML = "<a href='"+queryServlet+
						"&fileName="+fileName+"&fileOwner="+fileOwner+"' "+
						"onmouseover='showInfoMessage(\"infoBox\", \"query program file "+fileName+"\")' "+
						"onmouseout='showInfoMessage(\"infoBox\", \"&nbsp;\")'>Query</a> ";
			}
		</script>
	<%
		String localUserName = (String) session.getAttribute("localUserName");

			FoldersUtilsClass workingFolder = new FoldersUtilsClass();
		Iterator<FileInfoClass> filesIterator = null;
		if (workingFolder != null) {
			filesIterator = workingFolder.returnFilesIterator(localUserName);
		}
		/*
		else {
			out.print("<h4>Error in application: working folder should not be null</h4>");
		}*/
		if (filesIterator != null) {
	%>
	<h3>Choose an existing program file for querying, viewing or remove</h3>
	<br />
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
	<%
		while (filesIterator.hasNext()) {
				FileInfoClass fileInfo = filesIterator.next();
	%>
				<script type="text/javascript">
					lineForProgramFile('files', "<%=fileInfo.getFileName()%>", "<%=fileInfo.getFileOwner()%>");
				</script>

				<%
				}
			}
	%>
	
		<script type="text/javascript">
			function unhideUpload() {
				var html = "<INPUT TYPE=\'submit\' VALUE=\'Upload File\'>";
				document.getElementById('uploadButton').innerHTML = html;	
			}
		</script>

		<h3>Upload a new program file</h3>
		<FORM ENCTYPE='multipart/form-data' method='POST' action="FilesMgmtServlet?op=upload">
			<INPUT TYPE='file' NAME='fuzzy-database' size="50" onchange="unhideUpload('uploadButton');">
			<DIV id='uploadButton'> 
			</DIV>
		</FORM>
	</div>	
</body>
</html>
