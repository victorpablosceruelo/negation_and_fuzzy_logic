<jsp:include page="commonHtmlHead.jsp" />

<%@page import="java.util.*"%>
<%@page import="java.io.*"%>
<%@page import="java.io.InputStreamReader"%>
<%@page import="auxiliar.ServletsAuxMethodsClass"%>

<jsp:include page="commonPersonalization.jsp" />

<body>
    <div id="bodyContainer">
    	<jsp:include page="commonBodyHead.jsp" />
    	<h3><a title="Back to the program files menu" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt;
    		<a title="Back to personalize program file <%= (String) request.getAttribute("fileName") %>" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.PersonalizeServlet, request, null) %>?fileName=<%= (String) request.getAttribute("fileName") %>&fileOwner=<%= (String) request.getAttribute("fileOwner") %>">Personalize program file <%= (String) request.getAttribute("fileName") %></a> &gt; 
			Personalize fuzzification <%= (String) request.getAttribute("fuzzification") %>
    	</h3>
    	<div class="tableWithBorderWidth50">
			<div class="tableHeadWithOutBorderWidth100">
					Information about the fuzzification chosen
			</div>

			<div class="tableRowWithBorderWidth100">
				<div class="tableCellWithBorderWidth50">Program file name</div>
				<div class="tableCellWithBorderWidth50"><%= (String) request.getAttribute("fileName") %></div>
			</div>
			<div class="tableRowWithBorderWidth100">
				<div class="tableCellWithBorderWidth50">Owner</div>
				<div class="tableCellWithBorderWidth50"><%= (String) request.getAttribute("fileOwner") %></div>
			</div>
			<div class="tableRowWithBorderWidth100">
				<div class="tableCellWithBorderWidth50">Fuzzification</div>
				<div class="tableCellWithBorderWidth50" id="predDefined"></div>
			</div>
			<div class="tableRowWithBorderWidth100">
				<div class="tableCellWithBorderWidth50">Depends on the values of</div>
				<div class="tableCellWithBorderWidth50" id="predNecessary"></div>
			</div>			
		</div>
    	
		<br />
		<br />
		<div id="personalizationTableDiv"></div>
		<br />
		<br />
		<div class="personalizationBarsTitle">Your personalized fuzzification</div>
		<br />
		<div id="myPersonalizationBarsDiv">
			You have not defined your personalization for this fuzzification yet. <br>
			Choose one between the existing personalizations or the default function in the selecion box below as the starting point. 
			<div id="myPersonalizationStartingPoint"></div>
		</div>
		<br />
		<br />		
    	<h3><a title="Back to the program files menu" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.FilesMgmtServlet, request, null) %>">Program Files Menu</a> &gt;
    		<a title="Back to personalize program file <%= (String) request.getAttribute("fileName") %>" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.PersonalizeServlet, request, null) %>?fileName=<%= (String) request.getAttribute("fileName") %>&fileOwner=<%= (String) request.getAttribute("fileOwner") %>">Personalize program file <%= (String) request.getAttribute("fileName") %></a> &gt; 
			Personalize fuzzification <%= (String) request.getAttribute("fuzzification") %>
    	</h3>
    	<br /><br />
	</div>
	
	<%
		String localUserName = null;
		if (session != null) {
			localUserName = (String) session.getAttribute("localUserName");
		}
	%>
	<script type="text/javascript">

		var localUserName = "<%=localUserName %>";
		var myPersonalizedFunction = new Array();
		
		function barValueChanged(barObject, kIndex, kName)
		{
			var value=barObject.value;
			value = parseFloat(value);
			if ((value != null) && (value != NaN)) {
				value = value.toFixed(2);
			}
			else value = 0;
			document.getElementById("personalizationBarValue["+kIndex+"]").innerHTML=value;
			
			// Update the values in the function.
			var i = myPersonalizationIndexI;
			var j = myPersonalizationIndexJ;			
			personalizePredInfo[i][2][j][1][kIndex][1] = barObject.value;
			
			updateFunctionGraphic(i);
		}
		
		function updateFunctionBars() {
			
			var i = myPersonalizationIndexI;
			var j = myPersonalizationIndexJ;
			
			var divContainer = document.getElementById("myPersonalizationBarsDiv");
			divContainer.innerHTML = ""; // clean up !!!
			
			var html = null;
			for (var k=0; k<personalizePredInfo[i][2][j][1].length; k++) {
				var divChild = document.createElement('div');
				divChild.className = "personalizationBar";
				html = "<input type='range'  min='0' max='1' step='0.01'"+
					" name='"+"personalizationBar["+k+"]'" + 
					" value='"+personalizePredInfo[i][2][j][1][k][1]+
					"' onchange='barValueChanged(this, "+k+", "+personalizePredInfo[i][2][j][1][k][0]+")'/>" +
					"<span id='personalizationBarValue["+k+"]'>"+personalizePredInfo[i][2][j][1][k][1]+"</span>";
				divChild.innerHTML = html; 
				divContainer.appendChild(divChild);
				// alert("appended: " + html);
			}
		}
		
		function updateFunctionGraphic(i) {
			drawChart("functionGraphic_" + i, i);
		}
		
		function copyFunctionValues(i, j) {
			var newIndex = personalizePredInfo[i][2].length;
			personalizePredInfo[i][2][newIndex] = new Array();
			personalizePredInfo[i][2][newIndex][0] = localUserName;
			personalizePredInfo[i][2][newIndex][1] = new Array();
			
			for (var k=0; k<personalizePredInfo[i][2][j][1].length; k++) {
				
				//debug.info(personalizePredInfo[i][2][j][1][k] + " " + personalizePredInfo[i][2][j][1][k].length);
				//debug.info(personalizePredInfo[i][2][j][1][k][0] + " " + personalizePredInfo[i][2][j][1][k][1]);
				
				personalizePredInfo[i][2][newIndex][1][k] = new Array();
				personalizePredInfo[i][2][newIndex][1][k][0] = personalizePredInfo[i][2][j][1][k][0];
				personalizePredInfo[i][2][newIndex][1][k][1] = personalizePredInfo[i][2][j][1][k][1];
			}

			myPersonalizationIndexI = i;
			myPersonalizationIndexJ = newIndex;

			updateFunctionBars();
			updateFunctionGraphic(myPersonalizationIndexI);
		}
		
		function copyFunction(i, j) {
			var functionOwner = personalizePredInfo[i][2][j][0];
			var confirmationText="";
			if (functionOwner == '') {
				confirmationText = "predefined function";
			}
			else {
				confirmationText = "function defined by " + functionOwner;
			}
			if (confirm("Do you want to take the " + confirmationText + " as your personalized function for the fuzzification " + personalizePredInfo[i][0])) {
				copyFunctionValues(i, j);
			}
			return false;
		}
	
		function chosenStartingPoint (comboBox) {
			var comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
			// alert(comboBoxValue);
			for (var i=0; i<personalizePredInfo.length; i++){
				if (personalizePredInfo[i].length >= 3) {
					for (var j=0; j<personalizePredInfo[i][2].length; j++) {
						if (((comboBoxValue == "default definition") && (personalizePredInfo[i][2][j][0] == '')) ||
							((comboBoxValue != "default definition") && (personalizePredInfo[i][2][j][0] == comboBoxValue))) {
							copyFunction(i, j);
						}
					}
				}
			}
		}
	
		var personalizeServletEditAction = "<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.PersonalizeServletEditAction, request, null) %>";
		var fileName = "<%= (String) request.getAttribute("fileName") %>";
		var fileOwner = "<%= (String) request.getAttribute("fileOwner") %>";
		var myPersonalizationIndexI = null;
		var myPersonalizationIndexJ = null;
		
		if (personalizePredInfo.length > 0) {
			var divContainer = document.getElementById("personalizationTableDiv");
			var table = document.createElement('div');
			table.id = "personalizationTable";
			table.className = "personalizationTable";
			divContainer.appendChild(table);

			/*
			var row = document.createElement('div');
			row.className = "personalizationTableRow";
			table.appendChild(row);
			
			var cell = null;
			cell = document.createElement('div');
			cell.className = "personalizationTableCell3";
			cell.innerHTML = "function applied";
			row.appendChild(cell);
			*/

			for (var i=0; i<personalizePredInfo.length; i++) {
				row = document.createElement('div');
				row.className = "personalizationTableRow";
				table.appendChild(row);
				
				if (personalizePredInfo[i].length >= 3) {
					for (var j=0; j<personalizePredInfo[i][2].length; j++) {
						if (personalizePredInfo[i][2][j][0] == fileOwner) {
							myPersonalizationIndexI = i;
							myPersonalizationIndexJ = j;
						}
					}
					
					cell = document.createElement('div');
					cell.className = "personalizationTableCell3";
					cell.id = "functionGraphic_" + i;
					row.appendChild(cell);
					// cell.innerHTML=personalizePredInfo[i][2];
					drawChart(cell.id, i);
						
					document.getElementById("predDefined").innerHTML = personalizePredInfo[i][0];
					document.getElementById("predNecessary").innerHTML = personalizePredInfo[i][1];
						
				}
			}
			
			if ((myPersonalizationIndexI == null) || (myPersonalizationIndexJ == null)) {
				var selectStartingPoingDiv = document.getElementById("myPersonalizationStartingPoint");
				var html = "<select name=\'" + "myPersonalizationStartingPointSelect" + "\'" + 
							"onchange=\"chosenStartingPoint(this);\">";
				html += "<option name=\'----\' value=\'----\'>----</option>";
				var optionName = null;
				for (var i=0; i<personalizePredInfo.length; i++){
					if (personalizePredInfo[i].length >= 3) {
						for (var j=0; j<personalizePredInfo[i][2].length; j++) {
							if (personalizePredInfo[i][2][j][0] == '') optionName = "default definition";
							else optionName = personalizePredInfo[i][2][j][0];
							html += "<option name=\'"+optionName+"\' value=\'"+optionName+"\'>"+optionName+"</option>";
						}
					}
				}
				selectStartingPoingDiv.innerHTML = html;
			}
			else {
				
			}
		}
		
		// "<a title='take this function as my function' href='' onclick='return copyFunction("+i+")'><img src='images/copy.png'></img></a>";
	</script>
	
</body>
</html>