<%@page import="urls.AppUrl"%>
<%@page import="auxiliar.Dates"%>
<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%
	// This sets the app path for all the application.
	AppUrl.getAppUrl(request);
%>

<!DOCTYPE html>
<!-- beginning of commonHtmlHead -->
<html>
<head>
	<meta charset="utf-8">
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
	<title>FleSe: Flexible Searches in Databases</title>
	
	<link rel="stylesheet" type="text/css" href="js_and_css/style.css" />
	<link rel="stylesheet" type="text/css" href="js_and_css/jquery-ui-1.9.2.custom.css" />
	
	<script type="text/javascript" src="js_and_css/ba-debug.js"></script>
	<script type="text/javascript" src="js_and_css/ba-debug-init.js"></script>
	<script type="text/javascript" src="js_and_css/jquery-1.8.3.js"></script>
	<script type="text/javascript" src="js_and_css/jquery-ui-1.9.2.custom.js"></script>
	
	<script type="text/javascript" src="js_and_css/clientSoftware.js"></script>
	<script type="text/javascript" src="js_and_css/highcharts.js" ></script>
	<script type="text/javascript" src="js_and_css/auxiliarJS.jsp?date=<%=Dates.getCurrentDate()%>"></script>
	<script type="text/javascript" src="js_and_css/answersJS.jsp?date=<%=Dates.getCurrentDate()%>"></script>
	<script type="text/javascript" src="js_and_css/fuzzificationsJS.jsp?date=<%=Dates.getCurrentDate()%>"></script>
	<script type="text/javascript" src="js_and_css/userOpsJS.jsp?date=<%=Dates.getCurrentDate()%>"></script>
</head>
<!-- end of commonHtmlHead -->

