<jsp:include page="commonHead.jsp" />
<!-- JavaScript Debugging Code and more -->
<jsp:include page="commonJavaScriptCode.jsp" />

<body>
<%@page import="java.util.ArrayList"%>
<%@page import="java.util.Iterator"%>
<%@page import="auxiliar.CiaoPrologConnectionClass"%>
<%@page import="auxiliar.DataBaseInfoClass"%>

<% CiaoPrologConnectionClass connection = (CiaoPrologConnectionClass) session.getAttribute("connection"); %>
<%  ArrayList<String []> answers = connection.getLastAnswers(); %>
<%  Iterator<String []> answersIterator = answers.iterator(); %>

<body>
    <div id="body">
    	<jsp:include page="commonBody.jsp" />
    	
    	<h3><a href="DataBasesMenuServlet">Program Files Menu</a> &gt; 
    		<a href="DataBaseQueryServlet?op=query&database=<%=connection.getCurrentDatabase() %>&owner=<%=connection.getCurrentDatabaseOwner() %>">Perform a query</a> &gt; 
    		Results after evaluating the query </h3>
		<br /><br />
    	
		<h3>This are the results to your query <br />
			<%=connection.getLastQuery() %>
			<br /> 
		 	to the database <%=connection.getCurrentDatabase() %> 
			with owner <%=connection.getCurrentDatabaseOwner() %>
		</h3>

		<br /><br /><br /><br /><br />

<%
	if (answersIterator != null) {
		%><table><%
		String [] answer;
		while (answersIterator.hasNext()) {
			%><tr><%
			answer = answersIterator.next();
			for (int i=0; i<answer.length; i++) {
				%><td><%=answer[i] %></td><%
			}
			%></tr><%
		}
		%></table><%
	}
%>
	<br /><br /><br /><br />
	</div>
</body>
</html>