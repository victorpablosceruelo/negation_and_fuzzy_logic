<%@page import="auxiliar.CiaoPrologConnectionClass"%>

// This file contains only JavaScript, and you should not see this.

<%
	// @SuppressWarnings("unchecked")
	CiaoPrologConnectionClass connection = (CiaoPrologConnectionClass) session.getAttribute("connection");
	out.println("cleanUpProgramIntrospection();");
	if (connection != null) {
		String [] jsLines = connection.getProgramIntrospectionInJS();
		if (jsLines != null) {
			for (int i=0; i<jsLines.length; i++) {
				out.println(jsLines[i]);
			}
		}
		else {
			out.println("addMsgToTheUser('ERROR: jsLines is null.');");
		}
	}
	else {
		out.println("addMsgToTheUser('ERROR: connection is null.');");
	}
%>

showMsgsToTheUser();

