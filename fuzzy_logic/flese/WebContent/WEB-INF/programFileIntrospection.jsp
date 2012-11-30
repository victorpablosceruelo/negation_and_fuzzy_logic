<%@page import="java.util.Iterator"%> 
<%@page import="auxiliar.FilesMgmtClass"%>
<%@page import="auxiliar.FileInfoClass"%>
<%@page import="auxiliar.CiaoPrologConnectionClass"%>

// This file contains only JavaScript, and you should not see this.

<%
	// @SuppressWarnings("unchecked")
	CiaoPrologConnectionClass connection = (CiaoPrologConnectionClass) request.getAttribute("connection");
	if (connection != null) {
		Iterator <AnswerTermInJavaClass[]> programIntrospectionIterator = connection.getProgramIntrospectionIterator();
		if (programIntrospectionIterator != null) {
			// out.println("<script type=\"text/javascript\">\n");
			out.println("cleanUpProgramIntrospection();");
			int i=0;
			AnswerTermInJavaClass [] predInfo;
			while (programIntrospectionIterator.hasNext()) {
				predInfo = programIntrospectionIterator.next();
				out.print("addToProgramIntrospection("+counter+", new predInfo(");
				for (int i=0; i<predInfo.length; i++) {
					out.print(predInfo[i].toJavaScript(true));
					if (i+1 < predInfo.length) out.print(",");
				}
				out.print(");\n");
				i++;
			}
		// out.println("</script>");
	}
%>
