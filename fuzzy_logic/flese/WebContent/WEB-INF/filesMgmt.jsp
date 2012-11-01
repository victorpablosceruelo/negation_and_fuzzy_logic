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
	
		<script language="JavaScript" src="qTip.js" type="text/JavaScript"></script>
		<script type="text/javascript">
			var filesMgmtServlet="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null)%>";
			var queryServlet="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.QueryServletBuildQuery, request, null)%>";
			
			function createProgramFilesWithMsgsTable (divId, programFilesTableId, msgsCellId) {
				var div = document.getElementById(divId);
				var programFilesWithMsgsTable = document.createElement('table');
				programFilesWithMsgsTable.id = 'programFilesWithMsgsTable';
				programFilesWithMsgsTable.className = 'programFilesWithMsgsTable';
				div.appendChild(programFilesWithMsgsTable);
				
				var row0 = programFilesWithMsgsTable.insertRow(-1);
				row0.className = 'programFilesWithMsgsTable';
				var row0cell0 = row0.insertCell(-1);
				row0cell0.className = 'programFilesWithMsgsTable';
				row0cell0.id = msgsCellId;
				row0cell0.innerHTML = "&nbsp;";
				
				var row1 = programFilesWithMsgsTable.insertRow(-1);
				row1.className = 'programFilesWithMsgsTable';
				var row1cell0 = row1.insertCell(-1);
				row1cell0.className = 'programFilesWithMsgsTable';
				var programFilesTable = document.createElement('table');
				programFilesTable.id = programFilesTableId;
				programFilesTable.className = 'programFilesTable';
				row1cell0.appendChild(programFilesTable); 
				
				var programFilesTableHead = programFilesTable.insertRow(-1);
				var programFilesTableHeadCell0 = document.createElement('th');
				programFilesTableHeadCell0.innerHTML = "File Name";
				programFilesTableHeadCell0.className = 'programFilesTable';
				programFilesTableHead.appendChild(programFilesTableHeadCell0);
				var programFilesTableHeadCell1 = document.createElement('th');
				programFilesTableHeadCell1.innerHTML = "File Owner";
				programFilesTableHeadCell1.className = 'programFilesTable';
				programFilesTableHead.appendChild(programFilesTableHeadCell1);
				var programFilesTableHeadCell2 = document.createElement('th');
				programFilesTableHeadCell2.innerHTML = "&nbsp;";
				programFilesTableHeadCell2.className = 'programFilesTable';
				programFilesTableHead.appendChild(programFilesTableHeadCell2);
				var programFilesTableHeadCell3 = document.createElement('th');
				programFilesTableHeadCell3.innerHTML = "&nbsp;";
				programFilesTableHeadCell3.className = 'programFilesTable';
				programFilesTableHead.appendChild(programFilesTableHeadCell3);
			}
			
			function lineForProgramFile(tableId, msgsCellId, fileName, fileOwner) {
				debug.info("lineForProgramFile: fileName: " + fileName);
				debug.info("lineForProgramFile: fileOwner: " + fileOwner);
				var table = document.getElementById(tableId);
				table.className = "programFilesList";
				var rowCount = table.rows.length;
				var row = table.insertRow(rowCount);
				row.className = "programFilesList";

				var cell1 = row.insertCell(0);
				cell1.className = 'programFilesTable';
				cell1.innerHTML = "<a href='"+filesMgmtServlet+
						"?op=view&fileName="+fileName+"&fileOwner="+fileOwner+"' "+
						"title=\"view program file "+fileName+"\""+
						">"+fileName+"</a> ";
				
				var cell2 = row.insertCell(1);
				cell2.className = 'programFilesTable';
				cell2.innerHTML=fileOwner;
				
				var cell3 = row.insertCell(2);
				cell3.className = 'programFilesTable';
				cell3.innerHTML = "<a href='"+filesMgmtServlet+
						"?op=remove&fileName="+fileName+"&fileOwner="+fileOwner+"' "+
						"title=\"remove program file "+fileName+"\""+
						"onclick='return confirm(\"Are you sure you want to delete program file "+fileName+"?\")'>"+
						"<img src=\"remove-file.gif\" alt=\"remove\" width=\"20\"></img></a> ";
				
				var cell4 = row.insertCell(3);
				cell4.className = 'programFilesTable';
				cell4.innerHTML = "<a href='"+queryServlet+
						"&fileName="+fileName+"&fileOwner="+fileOwner+"' "+
						"title=\"query program file "+fileName+"\">Query</a> ";
			}
						
			function unhideUpload() {
				var html = "<INPUT TYPE=\'submit\' VALUE=\'Upload File\'>";
				document.getElementById('uploadButton').innerHTML = html;	
			}
		</script>

	<h3>Choose an existing program file for querying, viewing or remove</h3>
	<br />
	<div id="programFilesTableWithInfo">&nbsp;</div>
	<br />

	

		<h3>Upload a new program file</h3>
		<FORM ENCTYPE='multipart/form-data' method='POST' action="FilesMgmtServlet?op=upload">
			<INPUT TYPE='file' NAME='fuzzy-database' size="50" onchange="unhideUpload('uploadButton');">
			<DIV id='uploadButton'> 
			</DIV>
		</FORM>
	</div>
	
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
			out.println("<script type=\"text/javascript\">\n");
			out.println("createProgramFilesWithMsgsTable('programFilesTableWithInfo', 'programFilesTable', 'msgsCell');");
			while (filesIterator.hasNext()) {
				FileInfoClass fileInfo = filesIterator.next();
				out.println("lineForProgramFile('programFilesTable', 'msgsCell', '" + fileInfo.getFileName() + "', '" + fileInfo.getFileOwner() + "');");
			}
			out.println("</script>");
		}
	%>
</body>
</html>

