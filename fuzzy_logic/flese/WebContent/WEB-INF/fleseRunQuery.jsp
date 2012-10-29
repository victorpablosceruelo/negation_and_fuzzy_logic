<jsp:include page="commonHead.jsp" />
<!-- JavaScript Debugging Code and more -->
<jsp:include page="commonJavaScriptCode.jsp" />

<body>
<%@page import="java.util.ArrayList"%>
<%@page import="java.util.Iterator"%>
<%@page import="auxiliar.CiaoPrologConnectionClass"%>
<%@page import="auxiliar.FileInfoClass"%>
<%@page import="auxiliar.AnswerTermInJavaClass"%>

<% CiaoPrologConnectionClass connection = (CiaoPrologConnectionClass) session.getAttribute("connection"); %>
<%  Iterator<AnswerTermInJavaClass []> answersIterator = connection.getLatestEvaluatedQueryAnswersIterator(); %>

<body>
    <div id="body">
    	<jsp:include page="commonBody.jsp" />
    	<%@page import="auxiliar.ServletsAuxMethodsClass"%>
		<h3><a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> 
			&gt; 
			<a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.QueryServletBuildQuery, request, null) %>&fileName=<%=connection.getLatestEvaluatedQueryProgramFileName()%>&fileOwner=<%=connection.getLatestEvaluatedQueryProgramFileOwner() %>"></a>
		</h3>
    	<jsp:include page="commonBodyProgramQuery.jsp" />
    	
    	<h3>Query Results for the query &nbsp;&nbsp; <%=connection.getLatestEvaluatedQuery() %> </h3>
		<br /><br /><br /><br /><br />

<%
	if (answersIterator != null) {
		out.print("<table>");
		String [] variablesNames = (String []) request.getAttribute("variablesNames");
		if (variablesNames != null) {
			out.print("<tr>");
			for (int i=0; i<variablesNames.length; i++) {
				out.print("<th>" + variablesNames[i] + "</th>");
			}
			out.print("</tr>");
		}
		AnswerTermInJavaClass [] answer;
		while (answersIterator.hasNext()) {
			out.print("<tr>");
			answer = answersIterator.next();
			for (int i=0; i<answer.length; i++) {
				out.print("<td>" + answer[i].toJavaScript(true) + "</td>");
			}
			out.print("</tr>");
		}
		out.print("</table>");
	}
%>

	<br /><br /><br /><br />
	</div>
</body>
</html>