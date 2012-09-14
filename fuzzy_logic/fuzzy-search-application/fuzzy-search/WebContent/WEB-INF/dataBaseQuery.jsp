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
<%@page import="auxiliar.DataBaseInfoClass"%>

<% CiaoPrologConnectionClass connection = (CiaoPrologConnectionClass) session.getAttribute("connection"); %>

<script src="/fuzzy-search/addQueryLine.js" language="Javascript" type="text/javascript"></script>

<%  Iterator<String []> loadedProgramQuantifiersIterator = connection.getLoadedProgramQuantifiersIterator(); %>
<%  Iterator<String []> loadedProgramCrispPredicatesIterator = connection.getLoadedProgramCrispPredicatesIterator(); %>
<%  Iterator<String []> loadedProgramFuzzyRulesIterator = connection.getLoadedProgramFuzzyRulesIterator(); %>

<script language="javascript">
	var quantifiersArray = new Array();
	quantifiersArray[0] = new Array("-select-");
	<%
	int counter = 0;
	if (loadedProgramQuantifiersIterator != null) {
		String [] quantifierInfo;
		while (loadedProgramQuantifiersIterator.hasNext()) {
			counter++;
			quantifierInfo = loadedProgramQuantifiersIterator.next();
			%>
			quantifiersArray[<%=counter%>] = <%=quantifierInfo[1]%>;
			<%
		}
	}
	%>
	
	var fuzzyRulesArray = new Array();
	fuzzyRulesArray[0] = new Array("-select-");
	<%
	counter = 0;
	if (loadedProgramFuzzyRulesIterator != null) {
		String [] fuzzyRuleInfo;
		while (loadedProgramFuzzyRulesIterator.hasNext()) {
			counter++;
			fuzzyRuleInfo = loadedProgramFuzzyRulesIterator.next();
			%>
			fuzzyRulesArray[<%=counter%>] = <%=fuzzyRuleInfo[1]%>;
			<%
		}
	}
	%>
</script>

<body onload="addQueryLine(dynamicInput, quantifiersArray, fuzzyRulesArray)">
<body>
	<h1>Fuzzy search application</h1>
		<h2><a href="DatabasesMenu">Back to the databases menu</a>. <a href="SocialAuthServlet?mode=signout">Signout</a>.</h2>
		<jsp:include page="showErrors.jsp" />
		<h2>Perform your query to the database <%=connection.getCurrentDatabase() %> 
			property of <%=connection.getCurrentDatabaseOwner() %></h2>

		
<form method="POST">
     <div id="dynamicInput">
          
     </div>
     <input type="button" value="Add another text input" onClick="addQueryLine('dynamicInput');">
</form>
		<h2>Available predicates at database: </h2>

		

</body>
</html>