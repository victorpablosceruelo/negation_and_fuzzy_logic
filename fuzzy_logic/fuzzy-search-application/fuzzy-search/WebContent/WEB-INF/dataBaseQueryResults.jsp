<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Fuzzy Search App</title>
<script type="text/javascript" src="js/ba-debug.js"></script>
</head>

<%@page import="java.util.ArrayList"%>
<%@page import="java.util.Iterator"%>
<%@page import="auxiliar.CiaoPrologConnectionClass"%>
<%@page import="auxiliar.DataBaseInfoClass"%>

<% CiaoPrologConnectionClass connection = (CiaoPrologConnectionClass) session.getAttribute("connection"); %>

<!-- 
<script src="/fuzzy-search/addQueryLine.js" language="Javascript" type="text/javascript"></script>
 -->
 
<%  Iterator<String []> loadedProgramQuantifiersIterator = connection.getLoadedProgramQuantifiersIterator(); %>
<%  Iterator<String []> loadedProgramCrispPredicatesIterator = connection.getLoadedProgramCrispPredicatesIterator(); %>
<%  Iterator<String []> loadedProgramFuzzyRulesIterator = connection.getLoadedProgramFuzzyRulesIterator(); %>

<!-- JavaScript Debugging Code and more -->
<jsp:include page="commonHeader.jsp" />

<br /><br /><br /><br /><br />
		

</body>
</html>