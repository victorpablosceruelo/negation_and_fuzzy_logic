<%@page import="java.util.Iterator"%> 
<%@page import="filesAndPaths.FilesMgmt"%>
<%@page import="filesAndPaths.FileInfoClass"%>

// This file contains only JavaScript, and you should not see this.

<%
	@SuppressWarnings("unchecked")
	Iterator<FileInfoClass> filesListIterator = (Iterator<FileInfoClass>) request.getAttribute("filesListIterator");

	out.println("cleanUpFilesList();");
	if (filesListIterator != null) {
		int i=0;
		while (filesListIterator.hasNext()) {
			FileInfoClass fileInfo = filesListIterator.next();
			out.println("addToFilesList("+ i +", '" + fileInfo.getFileName() + "', '" + fileInfo.getFileOwner() + "');");
			i++;
		}
	}
%>