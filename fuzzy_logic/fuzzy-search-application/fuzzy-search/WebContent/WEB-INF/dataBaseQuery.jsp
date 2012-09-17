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
<%  Iterator<String []> loadedProgramQuantifiersIterator = connection.getLoadedProgramQuantifiersIterator(); %>
<%  Iterator<String []> loadedProgramCrispPredicatesIterator = connection.getLoadedProgramCrispPredicatesIterator(); %>
<%  Iterator<String []> loadedProgramFuzzyRulesIterator = connection.getLoadedProgramFuzzyRulesIterator(); %>

<!-- JavaScript Debugging Code and more -->
<jsp:include page="commonHeader.jsp" />

<script language="javascript">
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

	var fuzzyRulesCounter = 0;
	var limit = 50;
	var fuzzyVarsCounter = 0;

	function chooseQuantifierCode(fuzzyRuleIndex, fuzzyRuleQuantifierIndex) {
		var html = "<select name=\'fuzzyRuleQuantifier[" + fuzzyRuleIndex + "][" + fuzzyRuleQuantifierIndex + "]\'>";
		html += "<option name=\'none\' value=\'none\'>none</option>";
		for (var i=0; i<quantifiersArray.length; i++){
			html += "<option name=\'" + quantifiersArray[i].predName + 
						"\' value=\'" + quantifiersArray[i].predName + "\'>"+
						quantifiersArray[i].predName + "</option>"
		}
		html += "</select>";
		return html;
	}
	function chooseFuzzyRuleCode(fuzzyRuleIndex) {
		var html = "<select name=\'fuzzyRule[" + fuzzyRuleIndex + 
		           "]\' onchange=\"fuzzyRuleChange(this, " + fuzzyRuleIndex + ");\">";
		html += "<option name=\'none\' value=\'none\''>none</option>";
		for (var i=0; i<fuzzyRulesArray.length; i++){
			html += "<option name=\'" + fuzzyRulesArray[i].predName + 
						"\' value=\'" + fuzzyRulesArray[i].predName + "\'>"+
						fuzzyRulesArray[i].predName + "</option>";
		}
		html += "</select>";
		return html;
	}
	function fuzzyRuleArgsCode(fuzzyRuleIndex) {
		return "<div id=\'fuzzyRuleArgs[" + fuzzyRuleIndex + "]\'> </div>";
	}
	
	function addFuzzyRuleArgumentFields(fuzzyRuleIndex, fuzzyVarIndex) {
		debug.info("addFuzzyRuleArgumentFields: fuzzyRuleIndex:" + fuzzyRuleIndex + " fuzzyVarIndex:" + fuzzyVarIndex);
		var html = "";
		html += "<select name=\'fuzzyRuleArgument[" + fuzzyRuleIndex + "]["+ fuzzyVarIndex+"]\'" + 
				"onchange=\"fuzzyRuleArgumentChange(this, " + fuzzyRuleIndex + ", " + fuzzyVarIndex +");\">";
		for (var k=fuzzyVarIndex; k<fuzzyVarsCounter; k++){
			html += "<option name=\'var_" + k + "\' value=\'var_" + k + "\'>variable_"+ k + "</option>";
		}
		for (var k=0; k<fuzzyVarIndex; k++){
			html += "<option name=\'var_" + k + "\' value=\'var_" + k + "\'>variable_"+ k + "</option>";
		}
		html += "<option name=\'constant' value=\'constant\'>constant</option>";
		html += "</select>";
		html += "<div id=\'fuzzyRuleArgumentConstant[" + fuzzyRuleIndex + "]["+ fuzzyVarIndex+"]\'> </div>";
		return html;
	}
	function fuzzyRuleArgumentChange(comboBox, fuzzyRuleIndex, fuzzyVarIndex) {
		var comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
		var divName = "fuzzyRuleArgumentConstant[" + fuzzyRuleIndex + "]["+ fuzzyVarIndex+"]";
		if (comboBoxValue == "constant") {
			var html = "<input type=\'text\' name=\'fuzzyRuleArgumentConstant["+ fuzzyRuleIndex + "][" + fuzzyVarIndex + "]\'>";
			document.getElementById(divName).innerHTML = html;
		}
		else {
			document.getElementById(divName).innerHTML = "";
		}
	}
	
	function fuzzyRuleChange(comboBox, fuzzyRuleIndex) {
		var elementId = "";
		var divName = "fuzzyRuleArgs[" + fuzzyRuleIndex + "]";
		debug.info("fuzzyRuleChange(comboBox, " + fuzzyRuleIndex + ") -> divName: " + divName);
		// var comboBox = document.getElementById('fuzzyRule[' + fuzzyRuleIndex + ']');
		var comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
		debug.info("comboBoxValue: " + comboBoxValue);
		
		var found = false;
		var i = 0;
		var predArity = 0;
		while ((! found) && (i < fuzzyRulesArray.length)) {
			debug.info("fuzzyRulesArray["+i+"]: " + fuzzyRulesArray[i].predName);
			if (comboBoxValue == fuzzyRulesArray[i].predName) {
				found = true;
				predArity = fuzzyRulesArray[i].predArity;
			}
			i++;
		}
		debug.info("Predicate Arity: " + predArity);
		
		var fuzzyVarIndex = fuzzyVarsCounter;
		fuzzyVarsCounter += predArity;
		var html = "";
		while (fuzzyVarIndex < fuzzyVarsCounter) {
			if (fuzzyVarIndex +1 == fuzzyVarsCounter) html+= "result: ";
			html += addFuzzyRuleArgumentFields(fuzzyRuleIndex, fuzzyVarIndex);
			fuzzyVarIndex += 1;
		}
		document.getElementById(divName).innerHTML = html;
		document.getElementById('hiddenNumOfVariables').innerHTML = " " + 
					"<input type=\"hidden\" name=\"fuzzyVarsCounter\" id=\"fuzzyVarsCounter\" value=\""+ fuzzyVarsCounter + "\" />";
	}
	
	function addQueryLine(divName) {
		if (fuzzyRulesCounter == limit) {
			alert("You have reached the limit of adding " + fuzzyRulesCounter + " subqueries.");
		} else {
			var newdiv = document.createElement('div');
			newdiv.innerHTML = " " +  
					chooseQuantifierCode(fuzzyRulesCounter, 0) + " &nbsp; " +
					chooseQuantifierCode(fuzzyRulesCounter, 1) + " &nbsp; " +
					chooseFuzzyRuleCode(fuzzyRulesCounter) + " &nbsp; " +
					fuzzyRuleArgsCode(fuzzyRulesCounter) + " &nbsp; ";
			
			document.getElementById(divName).appendChild(newdiv);
			fuzzyRulesCounter++;
		}
	}
</script>

<!--   <body onload=""> -->
<body>
	<h1>Fuzzy search application</h1>
		<h2><a href="DataBasesMenuServlet">Back to the databases menu</a>. <a href="SocialAuthServlet?mode=signout">Signout</a>.</h2>
		<jsp:include page="showErrors.jsp" />
		<h2>Perform your query to the database <%=connection.getCurrentDatabase() %> 
			with owner <%=connection.getCurrentDatabaseOwner() %></h2>

		
<form action="DataBaseQueryServlet?op=runquery&database=<%=connection.getCurrentDatabase()%>&owner=<%=connection.getCurrentDatabaseOwner()%>"" method="POST">
     <div id="queryLines">
          
     </div>
     <script type="text/javascript" language="JavaScript">
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