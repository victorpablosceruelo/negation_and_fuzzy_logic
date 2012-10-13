
<%@page import="java.util.ArrayList"%>
<%@page import="java.util.Iterator"%>
<%@page import="auxiliar.CiaoPrologConnectionClass"%>
<%@page import="auxiliar.FileInfoClass"%>
<%@page import="auxiliar.AnswerTermInJava"%>

<% CiaoPrologConnectionClass connection = (CiaoPrologConnectionClass) session.getAttribute("connection"); %>
<% Iterator<AnswerTermInJava []> loadedProgramIntrospectionIterator = connection.getProgramIntrospectionIterator(); %>

<script type="text/javascript">
	function predInfo(predClass, predName, predArity, predType) {
		this.predClass = predClass;
		this.predName = predName;
		this.predArity = predArity;
		this.predType = predType;
	}
	
	var programIntrospectionArray = new Array();
	<%
		int counter = 0;
		if (loadedProgramIntrospectionIterator != null) {
			AnswerTermInJava [] predInfo;
			while (loadedProgramIntrospectionIterator.hasNext()) {
				predInfo = loadedProgramIntrospectionIterator.next();
				out.print("    programIntrospectionArray["+counter+"] = new predInfo(");
				for (int i=0; i<predInfo.length; i++) {
					out.print(predInfo[i].toJavaScript(true));
					if (i+1 < predInfo.length) out.print(",");
				}
				out.print(");\n");
				counter++;
			}
		}
	%>
	debug.info("Added a total of <%=counter%> elements from program introspection.");
	
	var currentProgramFileName = "<%=connection.getLatestEvaluatedQueryProgramFileName() %>";
	var currentProgramFileOwner = "<%=connection.getLatestEvaluatedQueryProgramFileOwner() %>";
	
	function changeFormAction(formId, url) {
		debug.info("Adding to the form with id " + formId + " the action url ");
		debug.info(url);
		document.getElementById(formId).action = url;
	}
	
	function changeAHrefLink(linkId, url) {
		debug.info("Adding to the a href with id " + linkId + " the href url " + url);
		debug.info(url);
		document.getElementById(linkId).href = url;
	}
	
</script>
