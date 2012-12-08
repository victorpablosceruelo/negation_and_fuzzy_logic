<%@page import="auxiliar.CiaoPrologConnectionClass"%>

// This file contains only JavaScript, and you should not see this.

<% 
	CiaoPrologConnectionClass connection = (CiaoPrologConnectionClass) session.getAttribute("connection");
	out.println("cleanUpQueryAnswers();");
	if (connection != null) {
		String [] queryAnswers = connection.getQueryAnswersInJS();
		if (queryAnswers != null) {
			for (int i=0; i<queryAnswers.length; i++) {
				out.println(queryAnswers[i]);
			}
		}
		else {
			out.println("addMsgToTheUser('ERROR: queryAnswers is null.');");
		}
	}
	else {
		out.println("addMsgToTheUser('ERROR: connection is null.');");
	}
%>
