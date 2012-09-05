<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Fuzzy Search App</title>
</head>

<%@page import="java.util.ArrayList"%>
<%@page import="java.util.Iterator"%>
<%@page import="auxiliar.CiaoPrologConnectionClass"%>
<%@page import="auxiliar.CiaoPrologProgramElementInfoClass"%>
<%@page import="auxiliar.DataBaseInfoClass"%>

<% CiaoPrologConnectionClass connection = (CiaoPrologConnectionClass) session.getAttribute("connection"); %>

<body>
	<h1>Fuzzy search application</h1>
		<h2><a href="DatabasesMenu">Back to the databases menu</a>. <a href="SocialAuthServlet?mode=signout">Signout</a>.</h2>
		<jsp:include page="showErrors.jsp" />
		<h2>Perform your query to the database <%=connection.getCurrentDatabase() %> 
			property of <%=connection.getCurrentDatabaseOwner() %></h2>

		<h2>Available predicates at database: </h2>
		<%  Iterator<CiaoPrologProgramElementInfoClass> programInfoIterator = connection.getProgramInfoIterator();
			if (programInfoIterator != null) {
				while (programInfoIterator.hasNext()) {
					CiaoPrologProgramElementInfoClass element = programInfoIterator.next();
		%>
					<h3>&nbsp;
		<%= element.getPredicateType()	%> &nbsp;
		<%= element.getPredicateName()	%> &nbsp;
		<%= element.getPredicateArity()	%>
					</h3>
		<%
				}
			}
		%>
		

</body>
</html>