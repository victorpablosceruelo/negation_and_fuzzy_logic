<jsp:include page="commonHead.jsp" />
<!-- JavaScript Debugging Code and more -->
<jsp:include page="commonJavaScriptCode.jsp" />

<%@page import="java.util.ArrayList"%>
<%@page import="java.util.Iterator"%>
<%@page import="auxiliar.CiaoPrologConnectionClass"%>
<%@page import="auxiliar.DataBaseInfoClass"%>

<% CiaoPrologConnectionClass connection = (CiaoPrologConnectionClass) session.getAttribute("connection"); %>
<%  Iterator<String []> loadedProgramQuantifiersIterator = connection.getLoadedProgramQuantifiersIterator(); %>
<%  Iterator<String []> loadedProgramCrispPredicatesIterator = connection.getLoadedProgramCrispPredicatesIterator(); %>
<%  Iterator<String []> loadedProgramFuzzyRulesIterator = connection.getLoadedProgramFuzzyRulesIterator(); %>

<script type="text/javascript">
	function predInfo(predName, predArity) {
		this.predName = predName;
		this.predArity = predArity;
	}
	
	var quantifiersArray = new Array();
	<%
		int counter = 0;
		if (loadedProgramQuantifiersIterator != null) {
			String [] quantifierInfo;
			while (loadedProgramQuantifiersIterator.hasNext()) {
				quantifierInfo = loadedProgramQuantifiersIterator.next();
				%>
				quantifiersArray[<%=counter%>] = new predInfo("<%=quantifierInfo[1]%>", <%=quantifierInfo[2]%>);
				<%
				counter++;
			}
		}
	%>
		
	var fuzzyRulesArray = new Array();
	<%
		counter = 0;
		if (loadedProgramFuzzyRulesIterator != null) {
			String [] fuzzyRuleInfo;
			while (loadedProgramFuzzyRulesIterator.hasNext()) {
				fuzzyRuleInfo = loadedProgramFuzzyRulesIterator.next();
				%>
				fuzzyRulesArray[<%=counter%>] = new predInfo("<%=fuzzyRuleInfo[1]%>", <%=fuzzyRuleInfo[2]%>);
				<%
				counter++;
			}
		}
	%>
</script>

<jsp:include page="dataBaseQueryJavaScriptCode.jsp" />

<body>
    <div id="body">
    	<jsp:include page="commonBody.jsp" />
    	
    	<h3><a href="DataBasesMenuServlet">Program Files Menu</a> &gt; Perform a query </h3>
		<br /><br />
    	
		<h3>Perform your query to the database <%=connection.getCurrentDatabase() %> 
			with owner <%=connection.getCurrentDatabaseOwner() %></h3>

		
		<form action="DataBaseQueryServlet?op=runquery&database=<%=connection.getCurrentDatabase()%>&owner=<%=connection.getCurrentDatabaseOwner()%>" method="POST">
     		<div id="queryLines">
          
     		</div>
     		<script type="text/javascript">
     			addQueryLine('queryLines', quantifiersArray, fuzzyRulesArray);
	 		</script>
	 		<br />
	 		<div id="hiddenNumOfVariables"></div>
     		<input type="button" value="Add more conditions to the query" onClick="addQueryLine('queryLines', quantifiersArray, fuzzyRulesArray);">
     		<INPUT type="submit" value="Execute Query">
		</form>
		<br /><br /><br /><br /><br />
	</div>
</body>
</html>