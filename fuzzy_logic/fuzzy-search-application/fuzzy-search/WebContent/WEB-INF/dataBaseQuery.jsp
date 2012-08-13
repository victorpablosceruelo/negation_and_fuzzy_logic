<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Fuzzy Search</title>
</head>
<body>
		<H1>Perform your query.</H1>

		<% if (request.getAttribute("msg1") != null) { %>
			<h1>MSG: <%=request.getAttribute("msg1") %>
			</h1>
		<% } %>
		
		<% if (request.getAttribute("msg2") != null) { %>
			<h1>MSG: <%=request.getAttribute("msg2") %>
			</h1>
		<% } %>




</body>
</html>