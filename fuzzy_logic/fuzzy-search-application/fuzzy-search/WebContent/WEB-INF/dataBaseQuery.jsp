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

<script language="javascript">
function init_callback_inpage() {
  function debug_callback( level ) { 
    var args = Array.prototype.slice.call( arguments, 1 ); 
    $('#debug').length || $('<div id="debug"><h2>debug output<\/h2><\/div>').appendTo( 'body' ); 
    $('<div/>') 
      .addClass( 'debug-' + level ) 
      .html( '[' + level + '] ' + args ) 
      .appendTo( '#debug' ); 
  };
  debug.setCallback( debug_callback, true );
}

function init_callback_firebuglite() {
  if ( !window.firebug ) {
    
    // from firebug lite bookmarklet
    window.firebug = document.createElement('script');
    firebug.setAttribute( 'src', 'http://getfirebug.com/releases/lite/1.2/firebug-lite-compressed.js' );
    document.body.appendChild( firebug );
    (function(){
      if ( window.firebug.version ) {
        firebug.init();
      } else {
        setTimeout( arguments.callee );
      }
    })();
    void( firebug );
    
    if ( window.debug && debug.setCallback ) {
      (function(){
        if ( window.firebug && window.firebug.version ) {
          debug.setCallback(function( level ) {
            var args = Array.prototype.slice.call( arguments, 1 );
            firebug.d.console.cmd[level].apply( window, args );
          }, true);
        } else {
          setTimeout( arguments.callee, 100);
        }
      })();
    }
  }
}
</script>

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

	var counter = 1;
	var limit = 50;

	function chooseQuantifierCode(counter, index) {
		var html = "<select name=\'quantifiers[" + counter + "][" + index + "]\'>";
		html += "<option name=\'none\' value=\'none\'>none</option>";
		for (var i=0; i<quantifiersArray.length; i++){
			html += "<option name=\'" + quantifiersArray[i].predName + 
						"\' value=\'" + quantifiersArray[i].predName + "\'>"+
						quantifiersArray[i].predName + "</option>"
		}
		html += "</select>";
		return html;
	}
	function chooseFuzzyRuleCode(counter) {
		var html = "<select name=\'fuzzyRule[" + counter + 
		           "]\' onchange=\"fuzzyRuleChange(this, \'dynamicArgs[" + counter + "]\', " + counter + ");\">";
		html += "<option name=\'none\' value=\'none\''>none</option>";
		for (var i=0; i<fuzzyRulesArray.length; i++){
			html += "<option name=\'" + fuzzyRulesArray[i].predName + 
						"\' value=\'" + fuzzyRulesArray[i].predName + "\'>"+
						fuzzyRulesArray[i].predName + "</option>"
		}
		html += "</select>";
		return html;
	}
	function fuzzyRuleArgsCode(counter) {
		var html = "<div id=\'dynamicArgs[" + counter + "]\'> </div>";
		return html;
	}
	
	function addFuzzyRuleArgumentBox(index) {
		html = "teto_" + index + " ";
	}
	
	function fuzzyRuleChange(comboBox, divName, counter) {
		var elementId = "";
		debug.info("fuzzyRuleChange(comboBox, " + divName + ", " + counter + ")");
		// var comboBox = document.getElementById('fuzzyRule[' + counter + ']');
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
		var html = "";
		for (var j=0; j<predArity; j++){
			html += addFuzzyRuleArgumentBox(j);
		}
		document.getElementById(divName).innerHTML = html;
	}
	
	function addQueryLine(divName) {
		if (counter == limit) {
			alert("You have reached the limit of adding " + counter + " subqueries.");
		} else {
			var newdiv = document.createElement('div');
			newdiv.innerHTML = chooseQuantifierCode(quantifiersArray, counter, 0) + " &nbsp; " +
					chooseQuantifierCode(counter, 1) + " &nbsp; " +
					chooseFuzzyRuleCode(counter) + " &nbsp; " +
					fuzzyRuleArgsCode(counter) + " &nbsp; ";
			
			document.getElementById(divName).appendChild(newdiv);
			counter++;
		}
	}
</script>

<!--   <body onload=""> -->
<body>
	<h1>Fuzzy search application</h1>
		<h2><a href="DataBasesMenuServlet">Back to the databases menu</a>. <a href="SocialAuthServlet?mode=signout">Signout</a>.</h2>
		<jsp:include page="showErrors.jsp" />
		<h2>Perform your query to the database <%=connection.getCurrentDatabase() %> 
			at <%=connection.getCurrentDatabaseOwner() %></h2>

		
<form method="POST">
     <div id="dynamicInput">
          
     </div>
     <script type="text/javascript" language="JavaScript">
     addQueryLine('dynamicInput', quantifiersArray, fuzzyRulesArray);
	 </script>
	 <br />
     <input type="button" value="Add more conditions to the query" onClick="addQueryLine('dynamicInput', quantifiersArray, fuzzyRulesArray);">
</form>
		<h2>Available predicates at database: </h2>

		

</body>
</html>