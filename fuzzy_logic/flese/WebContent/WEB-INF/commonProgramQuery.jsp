
<%@page import="java.util.ArrayList"%>
<%@page import="java.util.Iterator"%>
<%@page import="auxiliar.CiaoPrologConnectionClass"%>
<%@page import="auxiliar.FileInfoClass"%>

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
	debug.info("Added a total of <%=counter%> program quantifiers.");
		
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
	debug.info("Added a total of <%=counter%> program rules.");
	
	var currentProgramFileName = "<%=connection.getCurrentProgramFileName() %>";
	var currentProgramFileOwner = "<%=connection.getCurrentProgramFileOwner() %>";
	
	function changeFormAction(formId, url) {
		debug.info("Adding to the form with id " + formId + " the action url ");
		debug.info(url);
		document.getElementById(formId).action = url;
	}
	
	function changeAHrefLink(linkId, url) {
		debug.info("Adding to the a href with id " + linkId + " the href url " + url);
		debug.info(url);
		document.getElementById(linkId).href = url;
	}
	
</script>
