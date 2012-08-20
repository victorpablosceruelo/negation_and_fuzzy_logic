<%@page import="servlets.*"%>
<%@page import="java.util.ArrayList"%>
<%@page import="java.util.Iterator"%>
<%@page import="auxiliar.FoldersUtilsClass"%>
<%@page import="auxiliar.DataBaseInfoClass"%>
<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>

<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Fuzzy Search App</title>
</head>
<body>
	
	<h1>Fuzzy search application</h1>
	<h2>Check your user information <a href="UserInfoServlet">here</a>. <a href="SocialAuthServlet?mode=signout">Signout</a>.</h2>
	<jsp:include page="showErrors.jsp" />
	<h2>Please choose between one of the following tasks:</h2>
	<h3>Upload a new database</h3>
	<FORM ENCTYPE='multipart/form-data' method='POST'
		action="UploadServlet">
		<INPUT TYPE='file' NAME='fuzzy-database' size="50"> <INPUT
			TYPE='submit' VALUE='Upload File'>
	</FORM>
	<%
		String localUserName = (String) session.getAttribute("localUserName");

		FoldersUtilsClass workingFolder = new FoldersUtilsClass();
			Iterator<DataBaseInfoClass> databasesIterator = null;
			if (workingFolder != null) {
		databasesIterator = workingFolder.returnDatabasesIterator(localUserName);
			}
			/*
			else {
		out.print("<h4>Error in application: working folder should not be null</h4>");
			}*/
			if (databasesIterator != null) {
	%>
	<h3>Choose an existing database for querying, viewing or remove</h3>
		<table>
				<tr>
					<td>DataBaseOwner</td>
					<td>DataBaseName</td>
					<td>Query the database</td>
					<td>Remove</td>
				</tr>
	<%
			while (databasesIterator.hasNext()) {
				DataBaseInfoClass dataBaseInfo = databasesIterator.next();
				%>
				<tr>
					<td><%=dataBaseInfo.getDataBaseOwner() %></td>
					<td><%=dataBaseInfo.getDataBaseName() %></td>
					<td><a href="DataBaseQueryServlet?op=query&database=<%=dataBaseInfo.getDataBaseName()%>&owner=<%=dataBaseInfo.getDataBaseOwner()%>">
							Query</a></td>
					<td><a href="DataBasesMenuServlet?op=remove&database=<%=dataBaseInfo.getDataBaseName()%>&owner=<%=dataBaseInfo.getDataBaseOwner()%>">
							Remove</a></td>
				</tr>
				<%
			}
	%>
		</table>
	<% } %>
	
</body>
</html>
