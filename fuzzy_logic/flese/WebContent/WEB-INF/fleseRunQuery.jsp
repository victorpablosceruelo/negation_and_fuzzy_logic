<jsp:include page="commonHead.jsp" />
<!-- JavaScript Debugging Code and more -->
<jsp:include page="commonJavaScriptCode.jsp" />

<jsp:include page="commonProgramQuery.jsp" />
<jsp:include page="commonProgramQueryJavaScriptCode.jsp" />

<body>
<%@page import="java.util.ArrayList"%>
<%@page import="java.util.Iterator"%>
<%@page import="auxiliar.CiaoPrologConnectionClass"%>
<%@page import="auxiliar.FileInfoClass"%>
<%@page import="auxiliar.AnswerTermInJavaClass"%>

<% CiaoPrologConnectionClass connection = (CiaoPrologConnectionClass) session.getAttribute("connection"); %>
<%  Iterator<AnswerTermInJavaClass []> answersIterator = connection.getLatestEvaluatedQueryAnswersIterator(); %>

<script type="text/javascript">
	var answers = new Array();
<%
	int answersCounter = 0;
	if (answersIterator != null) {
		String [] variablesNames = (String []) request.getAttribute("variablesNames");
		if (variablesNames != null) {
			out.print("answers["+answersCounter+"] = new Array(");
			for (int i=0; i<variablesNames.length; i++) {
				out.print("'" + variablesNames[i] + "'");
				if ((i+1) < variablesNames.length) out.print(", ");
			}
			out.print("); \n");
		}
		answersCounter++;
		AnswerTermInJavaClass [] answer;
		while (answersIterator.hasNext()) {
			answer = answersIterator.next();
			out.print("answers["+answersCounter+"] = new Array(");
			for (int i=0; i<answer.length; i++) {
				out.print(answer[i].toJavaScript(true));
				if ((i+1) < answer.length) out.print(", ");
			}
			out.print("); \n");
			answersCounter++;
		}
		out.print("\n\n\n");
	}
%>
</script>
<body>
    <div id="body">
    	<jsp:include page="commonBody.jsp" />
    	<%@page import="auxiliar.ServletsAuxMethodsClass"%>
		<h3><a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> 
			&gt; 
			<a href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.QueryServletBuildQuery, request, null) %>&fileName=<%=connection.getLatestEvaluatedQueryProgramFileName()%>&fileOwner=<%=connection.getLatestEvaluatedQueryProgramFileOwner() %>">Query again the database</a>
			&gt;
			Query results 
		</h3>
    	<jsp:include page="commonBodyProgramQuery.jsp" />
    	
    	<h3>Query Results for the query &nbsp;&nbsp; <%=(String) request.getAttribute("querySimpleInfoString") %></h3>
		<br /><br /><br /><br /><br />

		<div id="results"></div>
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
	<table>
		<tr>
			<th>Query Format</th>
			<th>Query</th>
		</tr>
		<tr>
			<td>Basic</td>
			<td> <%=(String) request.getAttribute("querySimpleInfoString") %> </td>
		</tr>
		<tr>
			<td>Complex</td>
			<td><%=request.getAttribute("queryComplexInfoString") %></td>
		</tr>
		<tr>
			<td>Prolog</td>
			<td><%=connection.getLatestEvaluatedQuery() %> </td>
		</tr>
	</table>
	<br /><br /><br /><br />
	</div>
	
	<script type="text/javascript">
		var div = document.getElementById('results');
		if ((answers.length == 1) || (answers.length == 0)) {
			div.innerHTML = "no answers";
		}
		else {
			var table = document.createElement('table');
			table.id = 'resultsTable';
			div.appendChild(table);
		
			for (var i=0; i<answers.length; i++) {
				var row = table.insertRow(-1);
				for (var j=0; j<answers[i].length; j++) {
					var cell = row.insertCell(-1);
					cell.innerHTML = answers[i][j];
				}
			}
		}
	</script>
</body>
</html>


