<jsp:include page="commonHtmlHead.jsp" />
<jsp:include page="commonProgramQuery.jsp" />

<body>
<%@page import="java.util.ArrayList"%>
<%@page import="java.util.Iterator"%>
<%@page import="auxiliar.CiaoPrologConnectionClass"%>
<%@page import="auxiliar.FileInfoClass"%>
<%@page import="auxiliar.AnswerTermInJavaClass"%>

<% CiaoPrologConnectionClass connection = (CiaoPrologConnectionClass) session.getAttribute("connection"); %>
<%  Iterator<AnswerTermInJavaClass []> answersIterator = connection.getLatestEvaluatedQueryAnswersIterator(); %>

<script type="text/javascript">
	var answers = new Array();
<%
	int answersCounter = 0;
	if (answersIterator != null) {
		String [] variablesNames = (String []) request.getAttribute("variablesNames");
		if (variablesNames != null) {
			out.print("answers["+answersCounter+"] = new Array(");
			for (int i=0; i<variablesNames.length; i++) {
				out.print("'" + variablesNames[i] + "'");
				if ((i+1) < variablesNames.length) out.print(", ");
			}
			out.print("); \n");
		}
		answersCounter++;
		AnswerTermInJavaClass [] answer;
		while (answersIterator.hasNext()) {
			answer = answersIterator.next();
			out.print("answers["+answersCounter+"] = new Array(");
			for (int i=0; i<answer.length; i++) {
				out.print(answer[i].toJavaScript(true));
				if ((i+1) < answer.length) out.print(", ");
			}
			out.print("); \n");
			answersCounter++;
		}
		out.print("\n\n\n");
	}
%>
</script>

<body class='body'>
   	<jsp:include page="commonBodyHead.jsp" />
   	<%@page import="auxiliar.ServletsAuxMethodsClass"%>
	<h3><a title="Back to the program files menu" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> 
		&gt; 
		<a title="Back to perform a query to program <%=connection.getLatestEvaluatedQueryProgramFileName()%>" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.QueryServletBuildQuery, request, null) %>&fileName=<%=connection.getLatestEvaluatedQueryProgramFileName()%>&fileOwner=<%=connection.getLatestEvaluatedQueryProgramFileOwner() %>">Query the database</a>
		&gt;
		Query results 
	</h3>
   	<jsp:include page="commonBodyProgramQuery.jsp" />
    	
   	<h3>Query Results for the query &nbsp;&nbsp; <%=(String) request.getAttribute("querySimpleInfoString") %></h3>
	<br /><br />

	<div id="tabs">
		<ul>
			<li><a href="#tabs-1">10 best results</a></li>
			<li><a href="#tabs-2">Results over 70%</a></li>
			<li><a href="#tabs-3">Results over 50%</a></li>
			<li><a href="#tabs-4">Results over 0%</a></li>
			<li><a href="#tabs-5">All results</a></li>
			<li><a href="#tabs-6">Debug information</a></li>
		</ul>
		
		<div id="tabs-1">
			<div id="queryResultsBest10"></div>
		</div>
		<div id="tabs-2">
			<div id="queryResultsOver70"></div>
		</div>
		<div id="tabs-3">
			<div id="queryResultsOver50"></div>
		</div>
		<div id="tabs-4">
			<div id="queryResultsOver0"></div>
		</div>
		<div id="tabs-5">
			<div id="queryResultsAll"></div>
		</div>
		
	
    	<div id="tabs-6">
				<h3>Debug information about the query run in the Prolog system</h3>
				<table class='queryResults'>
					<tr class='queryResults'>
						<th class='queryResults'>Query Format</th>
						<th class='queryResults'>Query</th>
					</tr>
					<tr class='queryResults'>
						<td class='queryResults'>Basic</td>
						<td class='queryResults'> <%=(String) request.getAttribute("querySimpleInfoString") %> </td>
					</tr>
					<tr class='queryResults'>
						<td class='queryResults'>Complex</td>
						<td class='queryResults'><%=request.getAttribute("queryComplexInfoString") %></td>
					</tr>
					<tr class='queryResults'>
						<td class='queryResults'>Prolog</td>
						<td class='queryResults'><%=connection.getLatestEvaluatedQuery() %> </td>
					</tr>
				</table>
		</div>
	</div>
	<br /><br /><br /><br />
	

	
	<script type="text/javascript">
		function createTable(divId) {
			var div = document.getElementById(divId);
			var table = document.createElement('table');
			table.id = divId + 'Table';
			table.className = 'queryResults';
			div.appendChild(table);
		}
		if ((answers.length == 1) || (answers.length == 0)) {
			document.getElementById('queryResultsBest10').innerHTML = "no answers";
			document.getElementById('queryResultsOver70').innerHTML = "no answers";
			document.getElementById('queryResultsOver50').innerHTML = "no answers";
			document.getElementById('queryResultsOver0').innerHTML = "no answers";
			document.getElementById('queryResultsAll').innerHTML = "no answers";
		}
		else {
		
			var row = null;
			var cell = null;
			for (var i=0; i<answers.length; i++) {
				row = table.insertRow(-1);
				row.className = 'queryResults';
				for (var j=1; j<answers[i].length; j++) {
					if (i==0) {
						cell = document.createElement('th');
						cell.className = 'queryResults';
						cell.innerHTML = answers[i][j];
						row.appendChild(cell);
					}
					else {
						cell = row.insertCell(-1);
						cell.className = 'queryResults';
						cell.innerHTML = answers[i][j];
					}
				}
			}
		}
	</script>
</body>
</html>


