<%@page import="socialAuth.*"%>
<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<%
	// Not necessary: jsp pages know about the session.
	// HttpSession session = request.getSession(false);
	if (session==null) { 
    	String url = ServletsAuxMethodsClass.getAppUrlFromRequest(request, null) + "index-authentication.jsp";
    	//response.sendRedirect( url );
%>
<meta http-equiv="Refresh" content="5; url=<%=url %>">
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Redirecting to <%=url %></title>
<%
	}
	else {
%>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Query the fuzzy database</title>

<%
}
%>
</head>
<body>

<%
	AuthForm authForm = (AuthForm) session.getAttribute("authForm"); 
%>

	Testing the jsp web page !!!

</body>
</html>