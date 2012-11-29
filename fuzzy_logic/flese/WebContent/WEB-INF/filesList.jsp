<%@page import="servlets.*"%>
<%@page import="java.util.ArrayList"%>
<%@page import="java.util.Iterator"%> 
<%@page import="auxiliar.FilesMgmtClass"%>
<%@page import="auxiliar.FileInfoClass"%>
<%@page import="auxiliar.ServletsAuxMethodsClass"%>
<% // @ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>

	<%
		@SuppressWarnings("unchecked")
		Iterator<FileInfoClass> filesIterator = (Iterator<FileInfoClass>) request.getAttribute("filesIterator");
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

