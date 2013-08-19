<%@page import="java.util.Iterator"%> 
<%@page import="filesAndPaths.FilesMgmt"%>
<%@page import="auxiliar.FileInfoClass"%>

// This file contains only JavaScript, and you should not see this.

var mainSection = document.getElementById('mainSection');
mainSection.innerHTML = "";

addMsgToTheUser("Ups! An exception occurred.");
addMsgToTheUser("You can press the key F5 and try again or send a bug report to vpablos@babel.ls.fi.upm.es");

<%
	if (request != null) {
		if (request.getAttribute("msgs") != null) {
			String [] msgs = (String []) request.getAttribute("msgs");
			for (int i=0; i<msgs.length; i++) {
				out.write("    addMsgToTheUser('"+msgs[i]+"');");
			}
			request.removeAttribute("msgs");
		}			
	}
	else {
		out.write("    addMsgToTheUser('ERROR: request is null.');");
	}
%>

showMsgsToTheUser();
