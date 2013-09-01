<%@page import="java.util.Iterator"%> 
<%@page import="managers.FilesManagerAux"%>
<%@page import="filesAndPaths.ProgramFileInfo"%>

// This file contains only JavaScript, and you should not see this.

<%
	@SuppressWarnings("unchecked")
	Iterator<ProgramFileInfo> filesListIterator = (Iterator<ProgramFileInfo>) request.getAttribute("filesListIterator");

	out.println("cleanUpFilesList();");
	if (filesListIterator != null) {
		int i=0;
		while (filesListIterator.hasNext()) {
	ProgramFileInfo fileInfo = filesListIterator.next();
	out.println("addToFilesList("+ i +", '" + fileInfo.getFileName() + "', '" + fileInfo.getFileOwner() + "');");
	i++;
		}
	}
%>