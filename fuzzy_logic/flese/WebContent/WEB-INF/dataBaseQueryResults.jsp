<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Fuzzy Search App</title>
<script type="text/javascript" src="js/ba-debug.js"></script>
</head>
<body>
<%@page import="java.util.ArrayList"%>
<%@page import="java.util.Iterator"%>
<%@page import="auxiliar.CiaoPrologConnectionClass"%>
<%@page import="auxiliar.DataBaseInfoClass"%>

<% CiaoPrologConnectionClass connection = (CiaoPrologConnectionClass) session.getAttribute("connection"); %>

<!-- 
<script src="/fuzzy-search/addQueryLine.js" language="Javascript" type="text/javascript"></script>
 -->
 
<%  ArrayList<String []> answers = connection.getLastAnswers(); %>
<%  Iterator<String []> answersIterator = answers.iterator(); %>

<!-- JavaScript Debugging Code and more -->
<jsp:include page="commonHeader.jsp" />

	<h1>Fuzzy search application</h1>
		<h2><a href="DataBaseQueryServlet?op=query&database=<%=connection.getCurrentDatabase()%>&owner=<%=connection.getCurrentDatabaseOwner()%>">Query again the database</a>.
			<a href="DataBasesMenuServlet">Back to the databases menu</a>. 
			<a href="SocialAuthServlet?mode=signout">Signout</a>.</h2>
		<jsp:include page="showErrors.jsp" />
		<h2>This are the results to your query <br />
			<%=connection.getLastQuery() %>
			<br /> 
		 	to the database <%=connection.getCurrentDatabase() %> 
			with owner <%=connection.getCurrentDatabaseOwner() %></h2>

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





</body>
</html>