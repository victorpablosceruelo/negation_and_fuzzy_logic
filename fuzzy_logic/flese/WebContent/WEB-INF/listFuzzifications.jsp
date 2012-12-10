<%@page import="java.util.*"%>
<%@page import="java.io.*"%>
<%@page import="java.io.InputStreamReader"%>
<%@page import="auxiliar.ServletsAuxMethodsClass"%>
<%@page import="auxiliar.ProgramAnalizedClass"%>

<%
	String localUserName = (String) request.getAttribute("localUserName");
	String fileName = (String) request.getAttribute("fileName");
	String fileOwner = (String) request.getAttribute("fileOwner");
	String filePath = (String) request.getAttribute("filePath");
	
	ProgramAnalizedClass programAnalized = new ProgramAnalizedClass(localUserName, fileName, fileOwner, filePath);
	out.println("cleanUpFuzzificationFunctionsDefinitions();");
	if (programAnalized != null) {
		String [] fuzzifications = programAnalized.getProgramFuzzificationsInJS();
		if (fuzzifications != null) {
			for (int i=0; i<fuzzifications.length; i++) {
				out.println(fuzzifications[i]);
			}
		}
		else {
			out.println("addMsgToTheUser('ERROR: fuzzifications is null.');");
		}
	}
/*	else {
		out.println("addMsgToTheUser('ERROR: programAnalized is null.');");
	}
*/
%>