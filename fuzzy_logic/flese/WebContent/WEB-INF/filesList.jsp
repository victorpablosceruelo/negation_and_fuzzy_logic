<%@page import="java.util.Iterator"%> 
<%@page import="auxiliar.FilesMgmtClass"%>
<%@page import="auxiliar.FileInfoClass"%>

// This file contains only JavaScript, and you should not see this.

<%
	@SuppressWarnings("unchecked")
	Iterator<FileInfoClass> filesListIterator = (Iterator<FileInfoClass>) request.getAttribute("filesListIterator");
	if (filesListIterator != null) {
		// out.println("<script type=\"text/javascript\">\n");
		out.println("filesList = new Array();");
		int i=0;
		while (filesListIterator.hasNext()) {
			FileInfoClass fileInfo = filesListIterator.next();
			out.println("filesList["+i+"] = new fileInfo('" + fileInfo.getFileName() + "', '" + fileInfo.getFileOwner() + "');");
			i++;
		}
		// out.println("</script>");
	}
%>

for (var i=0; i<filesList.length; i++) debug.info("filesList["+i+"]=("+filesList[i].fileName+", "+filesList[i].fileOwner+")");
debug.info('Loaded contents of filesList.jsp.');