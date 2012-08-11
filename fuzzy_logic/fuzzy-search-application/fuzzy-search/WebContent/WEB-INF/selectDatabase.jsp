<%@page import="servlets.*"%>
<%@page import="java.util.ArrayList"%>
<%@page import="java.util.Iterator"%>
<%@page import="auxiliar.WorkingFolderClass"%>
<%@page import="auxiliar.DataBaseInfoClass"%>
<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>

<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Fuzzy Search App</title>

</head>
<body>

	<h1>Welcome to the fuzzy search application. Your username here is <%= session.getAttribute("user_display_name") %></h1>
	<%	
		if (session != null) {
			if (session.getAttribute("msg1") != null) {
				%> <h2><%=session.getAttribute("msg1") %></h2>
				<%
			}
			if (session.getAttribute("msg2") != null) {
				%> <h2><%=session.getAttribute("msg2") %></h2>
				<%
			}			
		}
	%>
	<h1>Please choose between one of the following tasks:</h1>
	<h2>Upload a new database</h2>
	<FORM ENCTYPE='multipart/form-data' method='POST'
		action="UploadServlet">
		<INPUT TYPE='file' NAME='fuzzy-database' size="50"> <INPUT
			TYPE='submit' VALUE='Upload File'>
	</FORM>
	<h2>Choose an existing database for querying, viewing or remove</h2>
	<%
		WorkingFolderClass workingFolder = new WorkingFolderClass();
		ArrayList<DataBaseInfoClass> databasesList = workingFolder.listDatabases((String) session.getAttribute("user_display_name"));
		if (!databasesList.isEmpty()) {
	%><table>
				<tr>
					<td>DataBaseOwner</td>
					<td>DataBaseName</td>
					<td>Execute</td>
					<td>Remove</td>
				</tr>
	<%     
			Iterator<DataBaseInfoClass> databasesIterator = databasesList.iterator(); 
			while (databasesIterator.hasNext()) {
				DataBaseInfoClass dataBaseInfo = databasesIterator.next();
				%>
				<tr>
					<td><%=dataBaseInfo.getDataBaseOwner() %></td>
					<td><%=dataBaseInfo.getDataBaseName() %></td>
					<td></td>
					<td></td>
				</tr>
				<%
			}
	%>
	</table>
	<% } %>
	
</body>
</html>
