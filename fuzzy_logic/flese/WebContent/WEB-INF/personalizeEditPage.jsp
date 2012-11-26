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
		<div id="fuzzificationTableDiv"></div>
		<br />
		<br />
		<form name="saveFuzzification" id="saveFuzzification" method="post" action="null" accept-charset="utf-8">
			<div class="fuzzificationBarsTitle">Your personalized fuzzification</div>
			<br />
			<div id="fuzzificationBarsDiv">
				You have not defined your personalization for this fuzzification yet. <br>
				Choose one between the existing personalizations or the default function in the selecion box below as the starting point. 
				<div id="fuzzificationBarsStartingPoint"></div>
			</div>
			<br />
			<div id="fuzzificationBarsSaveButtonDiv">
			</div>
		</form>
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

		var personalizeServletEditAction = "<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.PersonalizeServletEditAction, request, null) %>";
		
		var fileName = "<%= (String) request.getAttribute("fileName") %>";
		var fileOwner = "<%= (String) request.getAttribute("fileOwner") %>";
		var myFuzzificationDefIndexI = null;
		var myFuzzificationDefIndexJ = null;
		var localUserName = "<%=localUserName %>";
		
		function barValueChanged(barObject, kIndex, kName)
		{
			var valueOriginal=barObject.value;
			var valueFloat = parseFloat(valueOriginal);
			if ((valueFloat != null) && (valueFloat != NaN)) {
				valueToShow = valueFloat.toFixed(2);
			}
			else {
				valueFloat = 0;
				valueToShow = 0;
			}
			
			document.getElementById("fuzzificationBarValue["+kIndex+"]").innerHTML=valueToShow;
			
			// Update the values in the function.
			var i = myFuzzificationDefIndexI;
			var j = myFuzzificationDefIndexJ;			
			fuzzificationDefs[i].functionsArray[j].data[kIndex][1] = valueFloat;
			
			updateFunctionGraphic(i);
		}
		
		function updateFunctionBars() {
			
			var i = myFuzzificationDefIndexI;
			var j = myFuzzificationDefIndexJ;
			
			var divContainer = document.createElement('div');
			divContainer.className = "fuzzificationBarsTable";
			
			document.getElementById("fuzzificationBarsDiv").innerHTML = ""; // clean up !!!
			document.getElementById("fuzzificationBarsDiv").appendChild(divContainer);
			
			/* 
				var form = document.createElement("form");
				form.setAttribute('method', "post");
				form.setAttribute('action', personalizeServletEditAction);
				divContainer.appendChild(form);
			*/
			
			var html = null;
			var divRow = null;
			var divChild = null;

			divRow = document.createElement('div');
			divRow.className = "fuzzificationBarsTableRow";
			divContainer.appendChild(divRow);
			
			divChild = document.createElement('div');
			divChild.className = "fuzzificationBarsTableCell";
			divChild.innerHTML = "When " + fuzzificationDefs[i].predNecessary + " takes the value ";
			// + fuzzificationDefs[i].functionsArray[j].data[k][0];
			divRow.appendChild(divChild);
			
			divChild = document.createElement('div');
			divChild.className = "fuzzificationBarsTableCell";
			divChild.innerHTML = "I consider that " + fuzzificationDefs[i].predDefined + " is satisfied in the degree "; 
			divRow.appendChild(divChild);
			
			for (var k=0; k<fuzzificationDefs[i].functionsArray[j].data.length; k++) {
				divRow = document.createElement('div');
				divRow.className = "fuzzificationBarsTableRow";
				divContainer.appendChild(divRow);
				
				divChild = document.createElement('div');
				divChild.className = "fuzzificationBarsTableCell";
				html = ""; // Reinitialize.
				html += fuzzificationDefs[i].functionsArray[j].data[k][0];
				html += "<input type='hidden' name='fuzzificationBars["+k+"].fpx' value='"+fuzzificationDefs[i].functionsArray[j].data[k][0]+"'/>";
				divChild.innerHTML = html;
				divRow.appendChild(divChild);

				divChild = document.createElement('div');
				divChild.className = "fuzzificationBarsTableCell";
				html = ""; // Reinitialize.
				html += "<input type='range'  name='fuzzificationBars["+k+"].fpy'" + 
						" min='0' max='1' step='0.01' value='"+fuzzificationDefs[i].functionsArray[j].data[k][1]+
						"' onchange='barValueChanged(this, "+k+", "+fuzzificationDefs[i].functionsArray[j].data[k][0]+")'/>";
				html += "<span id='fuzzificationBarValue["+k+"]'>"+fuzzificationDefs[i].functionsArray[j].data[k][1]+"</span>";
				divChild.innerHTML = html; 
				divRow.appendChild(divChild);
				// alert("appended: " + html);
			}
			
			divChild = document.getElementById('fuzzificationBarsSaveButtonDiv');
			divChild.innerHTML = "<input type='submit' name='save' value='Save changes'>";
			divContainer.appendChild(divChild);
		}
		
		function updateFunctionGraphic(i) {
			var divId = "functionGraphic_" + i;
			document.getElementById(divId).innerHTML = ""; // clean up !!!.
			drawChart(divId, i);
		}
		
		function copyFunctionValues(i, j) {
			var newIndex = fuzzificationDefs[i].functionsArray.length;
			fuzzificationDefs[i].functionsArray[newIndex] = new fuzzificationFunctionDef(localUserName, new Array());
			
			for (var k=0; k<fuzzificationDefs[i].functionsArray[j].data.length; k++) {
				
				//debug.info(personalizePredInfo[i][2][j][1][k] + " " + personalizePredInfo[i][2][j][1][k].length);
				//debug.info(personalizePredInfo[i][2][j][1][k][0] + " " + personalizePredInfo[i][2][j][1][k][1]);
				
				fuzzificationDefs[i].functionsArray[newIndex].data[k] = new Array();
				fuzzificationDefs[i].functionsArray[newIndex].data[k][0] = fuzzificationDefs[i].functionsArray[j].data[k][0];
				fuzzificationDefs[i].functionsArray[newIndex].data[k][1] = fuzzificationDefs[i].functionsArray[j].data[k][1];
			}

			myFuzzificationDefIndexI = i;
			myFuzzificationDefIndexJ = newIndex;

			updateFunctionBars();
			updateFunctionGraphic(myFuzzificationDefIndexI);
		}
		
		function copyFunction(i, j) {
			var confirmationText=fuzzificationDefs[i].functionsArray[j].name;
			if (confirmationText != "default definition") {
				confirmationText = "function defined by " + confirmationText;
			}
			if (confirm("Do you want to take the " + confirmationText + " as your personalized function for the fuzzification " + 
					fuzzificationDefs[i].predDefined)) {
				copyFunctionValues(i, j);
			}
			return false;
		}
	
		function chosenStartingPoint (comboBox) {
			var comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
			// alert(comboBoxValue);
			for (var i=0; i<fuzzificationDefs.length; i++){
				if (fuzzificationDefs[i] != null) {
					for (var j=0; j<fuzzificationDefs[i].functionsArray.length; j++) {
						if (fuzzificationDefs[i].functionsArray[j].name == comboBoxValue) {
							copyFunction(i, j);
						}
					}
				}
			}
			
			// Set the form action.
			var form = document.getElementById("saveFuzzification");
			var saveAction = personalizeServletSaveAction + "&fileName=" + fileName + "&fileOwner=" + fileOwner + "&fuzzification=" + fuzzification;
			form.action = saveAction;
			form.setAttribute('action', saveAction);
		}
			
		if (fuzzificationDefs.length > 0) {
			var divContainer = document.getElementById("fuzzificationTableDiv");
			var table = document.createElement('div');
			table.id = "fuzzificationTable";
			table.className = "fuzzificationTable";
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

			for (var i=0; i<fuzzificationDefs.length; i++) {
				row = document.createElement('div');
				row.className = "fuzzificationTableRow";
				table.appendChild(row);
				
				if (fuzzificationDefs[i] != null) {
					for (var j=0; j<fuzzificationDefs[i].functionsArray.length; j++) {
						if (fuzzificationDefs[i].functionsArray[j].name == fileOwner) {
							myPersonalizationIndexI = i;
							myPersonalizationIndexJ = j;
						}
					}
					
					cell = document.createElement('div');
					cell.className = "fuzzificationTableCell3";
					cell.id = "functionGraphic_" + i;
					row.appendChild(cell);
					// cell.innerHTML=personalizePredInfo[i][2];
					drawChart(cell.id, i);
						
					document.getElementById("predDefined").innerHTML = fuzzificationDefs[i].predDefined;
					document.getElementById("predNecessary").innerHTML = fuzzificationDefs[i].predNecessary;
						
				}
			}
			
			if ((myFuzzificationDefIndexI == null) || (myFuzzificationDefIndexJ == null)) {
				var selectStartingPoingDiv = document.getElementById("fuzzificationBarsStartingPoint");
				var html = "<select name=\'fuzzificationBarsStartingPointSelect" + "\'" + 
							"onchange=\"chosenStartingPoint(this);\">";
				html += "<option name=\'----\' value=\'----\'>----</option>";
				var optionName = null;
				for (var i=0; i<fuzzificationDefs.length; i++){
					if (fuzzificationDefs[i] != null) {
						for (var j=0; j<fuzzificationDefs[i].functionsArray.length; j++) {
							html += "<option name=\'"+fuzzificationDefs[i].functionsArray[j].name+"\' value=\'"+
									fuzzificationDefs[i].functionsArray[j].name+"\'>"+
									fuzzificationDefs[i].functionsArray[j].name+"</option>";
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