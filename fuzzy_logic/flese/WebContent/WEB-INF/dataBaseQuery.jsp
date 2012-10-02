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
	<h1>Fuzzy search application</h1>
		<h2><a href="DataBasesMenuServlet">Back to the databases menu</a>. <a href="SocialAuthServlet?mode=signout">Signout</a>.</h2>
		<jsp:include page="showErrors.jsp" />
		<h2>Perform your query to the database <%=connection.getCurrentDatabase() %> 
			with owner <%=connection.getCurrentDatabaseOwner() %></h2>

		
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
		

</body>
</html>