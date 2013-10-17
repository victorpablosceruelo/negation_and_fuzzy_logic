/*!
 * auxiliarJS Library v1
 * auxiliar javascript code
 * Author: Victor Pablos Ceruelo
 */

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

<%@page import="constants.KUrls"%>
<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>

<% if (JspsUtils.getStringWithValueS().equals("N")) { %>
<script type="text/javascript">
<% } %>

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

var fuzzificationsFunctions = null;

function ownerPersonalization (predOwner, functionPoints) {
	this.name = predOwner;
	this.data = functionPoints;
}

function fuzzificationFunction(predDefined, predNecessary, ownersPersonalizations) {
	this.predDefined = predDefined;
	this.predNecessary = predNecessary;
	this.ownersPersonalizations = ownersPersonalizations;
}

function cleanUpFuzzificationFunctionsDefinitions () {
	fuzzificationsFunctions = null;
	fuzzificationsFunctions = new Array();
}

function addFuzzificationFunctionDefinition (predDefined, predNecessary, ownersPersonalizations) {
	fuzzificationsFunctions[fuzzificationsFunctions.length] = new fuzzificationFunction(predDefined, predNecessary, ownersPersonalizations);
}

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

function personalizeProgramFile(chooseProgramFileId, mode) {
	// alert("fileViewContentsDivId: " + fileViewContentsDivId);
	var comboBox = document.getElementById(chooseProgramFileId);
	var selectedProgramDatabase = getProgramDatabaseComboBoxValue(comboBox);
	
	if (selectedProgramDatabase == null) {
		alert("Please choose a valid database to continue.");
	}
	else {
		personalizeProgramFile(selectedProgramDatabase.fileName, selectedProgramDatabase.fileOwner, mode);
	}
	
	//prevent the browser to follow the link
	return false;
}

function personalizeProgramFile(url, params, fileName) {
	var containerId = '<%=KConstants.JspsDivsIds.auxAndInvisibleSection %>';
	
	loadAjaxInDialog(containerId, url + params, 'Personalize program file ' + fileName);
	
	//prevent the browser to follow the link
	return false;
}

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

function fuzzificationFunctionNameInColloquial(currentName, grade) {
	var result = null;
	if ((currentName != null) && (currentName != "") && (currentName != "----")) {
	
		var i = currentName.indexOf("(");
		var j = currentName.indexOf(")");
		
		if ((i != -1) && (j != -1)){
			result = "";
			if ((grade == 'all') || (grade == 'subject')) {
				result += currentName.substring(i+"(".length, j);
			}
			if (grade == 'all') {
				result += " is ";
			}
			if ((grade == 'all') || (grade == 'adjective')) {
				result += currentName.substring(0, i);
			}
		}
	}
	
	if ((result == null) && (currentName != null)) return currentName;
	else return prologNameInColloquialLanguage(result);
}

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

function showPersonalizeProgramFileDialog(fileName, fileOwner, mode) {
}

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

function personalizationFunctionChanged(comboBox, PersonalizationFunctionUnderModificationDivId) {
	
	var comboBoxValue = getComboBoxValue(comboBox);

	var index = comboBoxValue;
	var PersonalizationFunctionUnderModificationDiv = getContainer(PersonalizationFunctionUnderModificationDivId);
	PersonalizationFunctionUnderModificationDiv.innerHTML = "";
	
	var table = null;
	var row = null;
	var cell = null;
	var fuzzificationGraphicDivId = "fuzzificationGraphicDiv";
	var fuzzificationValuesAndButtonDivId = "fuzzificationValuesAndButtonDiv";
	
	// Table that contains everything about the fuzzification function.
	table = document.createElement('div');
	table.className = "personalizationDivFuzzificationFunctionTable";
	PersonalizationFunctionUnderModificationDiv.appendChild(table);		
	
	if (mode == 'advanced') {
		row = document.createElement('div');
		row.className = "personalizationDivFuzzificationFunctionTableRow";
		table.appendChild(row);
		
		// Cell that contains the graphic representation of the fuzzification function.
		cell = document.createElement('div');
		cell.id = fuzzificationGraphicDivId;
		cell.className = "personalizationDivFuzzificationFunctionTableCell1";
		cell.innerHTML = "";
		row.appendChild(cell);
		
		insertFuzzificationGraphicRepresentation(index, fuzzificationGraphicDivId);
	}
	
	row = document.createElement('div');
	row.className = "personalizationDivFuzzificationFunctionTableRow";
	table.appendChild(row);
	
	// Cell that contains the numerical representation of the function, the save
	// button and some others.
	cell = document.createElement('div');
	cell.id = fuzzificationValuesAndButtonDivId;
	cell.className = "personalizationDivFuzzificationFunctionTableCell2";
	cell.innerHTML = "";
	row.appendChild(cell);
	
	insertFuzzificationValuesAndSaveButton(index, fuzzificationValuesAndButtonDivId, fuzzificationGraphicDivId, mode, fileName, fileOwner);
}

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

function insertFuzzificationValuesAndSaveButton(index, fuzzificationValuesAndButtonDivId, fuzzificationGraphicDivId, mode, fileName, fileOwner){
	
	var container = document.getElementById(fuzzificationValuesAndButtonDivId);
	var table = null;
	var row = null;
	var cell = null;
	var saveButtonCell = null;
	var fuzzificationValuesCell = null;
	
	table = document.createElement('div');
	table.className = "personalizationDivFuzzificationFunctionWithButtonTable";
	container.appendChild(table);
	
	row = document.createElement('div');
	row.className = "personalizationDivFuzzificationFunctionWithButtonTableRow";
	table.appendChild(row);
	
	fuzzificationValuesCell = document.createElement('div');
	fuzzificationValuesCell.className = "personalizationDivFuzzificationFunctionWithButtonTableCell";
	row.appendChild(fuzzificationValuesCell);
	
	row = document.createElement('div');
	row.className = "personalizationDivFuzzificationFunctionWithButtonTableRow";
	table.appendChild(row);
	
	saveButtonCell = document.createElement('div');
	saveButtonCell.className = "personalizationDivFuzzificationFunctionWithButtonTableCell";
	row.appendChild(saveButtonCell);

	
	// Fuzzification function values view and modification.
	// We present them inside a table.
	table = document.createElement('div');
	table.className = "personalizationDivFuzzificationFunctionValuesTable";
	fuzzificationValuesCell.appendChild(table);	
	
	row = document.createElement('div');
	row.className = "personalizationDivFuzzificationFunctionValuesTableRow";
	table.appendChild(row);
	
	cell = document.createElement('div');
	cell.className = "personalizationDivFuzzificationFunctionValuesTableCell";
	row.appendChild(cell);
	cell.innerHTML = "A " + 
						fuzzificationFunctionNameInColloquial(fuzzificationsFunctions[index].predDefined, 'subject') +
						" whose value for " + 
						fuzzificationFunctionNameInColloquial(fuzzificationsFunctions[index].predNecessary, 'adjective') + 
						" is ";
	
	cell = document.createElement('div');
	cell.className = "personalizationDivFuzzificationFunctionValuesTableCell";
	row.appendChild(cell);
	cell.innerHTML = "is " + fuzzificationFunctionNameInColloquial(fuzzificationsFunctions[index].predDefined, 'adjective') +
						" with a degree of ";

	cell = document.createElement('div');
	cell.className = "personalizationDivFuzzificationFunctionValuesTableCell";
	row.appendChild(cell);
	cell.innerHTML = "Current Value";
	
	cell = document.createElement('div');
	cell.className = "personalizationDivFuzzificationFunctionValuesTableCell";
	row.appendChild(cell);
	if (mode == 'basic') cell.innerHTML = "Default Value";
	else cell.innerHTML = "Old Value";
	
	var i = null;
	var j = null;
	var indexOfDefault = null;
	var indexOfMine = null;
	
	for (i=0; i<fuzzificationsFunctions[index].ownersPersonalizations.length; i++) {
		if (fuzzificationsFunctions[index].ownersPersonalizations[i].name == localUserName) {
			indexOfMine = i;
		}
		if (fuzzificationsFunctions[index].ownersPersonalizations[i].name == 'default definition') {
			indexOfDefault = i;
		}
	}
	//debug.info("indexOfMine: " + indexOfMine);
	//debug.info("indexOfDefault: " + indexOfDefault);
	//debug.info("mode: " + mode);
	
	if (mode == 'advanced') indexOfMine = indexOfDefault;
	else {
		if (indexOfMine == null) {
			indexOfMine = fuzzificationsFunctions[index].ownersPersonalizations.length;
			fuzzificationsFunctions[index].ownersPersonalizations[indexOfMine] =
				new ownerPersonalization(localUserName, new Array());
			for (i=0; i < fuzzificationsFunctions[index].ownersPersonalizations[indexOfDefault].data.length; i++) {
				fuzzificationsFunctions[index].ownersPersonalizations[indexOfMine].data[i] = new Array();
				for (j=0; j < fuzzificationsFunctions[index].ownersPersonalizations[indexOfDefault].data[i].length; j++) {
					fuzzificationsFunctions[index].ownersPersonalizations[indexOfMine].data[i][j] = 
						fuzzificationsFunctions[index].ownersPersonalizations[indexOfDefault].data[i][j];
				}
			}
		}
	}
	//debug.info("indexOfMine (fixed): " + indexOfMine);
	
	var fpx = null;
	var fpy = null;
	var fpd = null;
	var found = false;
	
	for (i=0; i<fuzzificationsFunctions[index].ownersPersonalizations[indexOfDefault].data.length; i++) {
		fpx = fuzzificationsFunctions[index].ownersPersonalizations[indexOfDefault].data[i][0];
		fpd = fuzzificationsFunctions[index].ownersPersonalizations[indexOfDefault].data[i][1];
		fpy = null;
		
		found = false;
		if (indexOfMine != null) {
			j = 0;
			while ((j<fuzzificationsFunctions[index].ownersPersonalizations[indexOfMine].data.length) && (! found)) {
				if (fpx == fuzzificationsFunctions[index].ownersPersonalizations[indexOfMine].data[j][0]) {
					found = true;
					fpy = fuzzificationsFunctions[index].ownersPersonalizations[indexOfMine].data[j][1];
				}
				else j++;
			}
		}
		
		// Assign default value if personalized one is not found.
		if ((! found) || (fpy == null) || (fpy == undefined)) fpy = fpd;
		
		// Now it is time to build the table with the data.
		row = document.createElement('div');
		row.className = "personalizationDivFuzzificationFunctionValuesTableRow";
		table.appendChild(row);
		
		cell = document.createElement('div');
		cell.className = "personalizationDivFuzzificationFunctionValuesTableCell";
		row.appendChild(cell);
		cell.innerHTML = fpx;
		
		cell = document.createElement('div');
		cell.className = "personalizationDivFuzzificationFunctionValuesTableCell";
		row.appendChild(cell);
		cell.innerHTML = "<input type='hidden' name='fuzzificationBars["+i+"].fpx' value='"+fpx+"'/>" +
						 "<input type='range'  name='fuzzificationBars["+i+"].fpy' min='0' max='1' step='0.01' "+
						 "value='"+fpy+"' width='150px' "+
						 "onchange='barValueChanged(this, "+i+", "+indexOfMine+", "+index+", \""+fuzzificationGraphicDivId+"\")'/>";

		cell = document.createElement('div');
		cell.className = "personalizationDivFuzzificationFunctionValuesTableCell";
		row.appendChild(cell);
		cell.innerHTML = "<span id='fuzzificationBarValue["+i+"]'>"+fpy+"</span>";
		
		cell = document.createElement('div');
		cell.className = "personalizationDivFuzzificationFunctionValuesTableCell";
		row.appendChild(cell);
		cell.innerHTML = fpd;		
	}
	
	// Table for the button in charge of saving the changes.
	table = document.createElement('div');
	table.className = "personalizationDivSaveButtonAndMsgTable";
	saveButtonCell.appendChild(table);

	row = document.createElement('div');
	row.className = "personalizationDivSaveButtonAndMsgTableRow";
	table.appendChild(row);
	
	cell = document.createElement('div');
	cell.className = "personalizationDivSaveButtonAndMsgTableCell";
	row.appendChild(cell);
	cell.innerHTML = 	"<INPUT type='submit' value='Save modifications' "+
						"onclick='saveFuzzificationPersonalizations(\"saveMyFuzzificationStatus\", \""+ 
						mode + "\", \"" + fileName + "\", \"" + fileOwner + "\", " + index + ", "+ indexOfMine + ")'>";

	cell = document.createElement('div');
	cell.className = "personalizationDivSaveButtonAndMsgTableCell";
	row.appendChild(cell);
	cell.innerHTML = "&nbsp;&nbsp;&nbsp;&nbsp;";
	
	cell = document.createElement('div');
	cell.className = "personalizationDivSaveButtonAndMsgTableCell";
	cell.id = "saveMyFuzzificationStatus";
	row.appendChild(cell);
	cell.innerHTML = "";

	/*
	cell = document.createElement('div');
	cell.className = "personalizationDivSaveButtonAndMsgTableCell";
	container.appendChild(cell);
	cell.innerHTML = "";
	*/	
}

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

function insertFuzzificationGraphicRepresentation(index, fuzzificationGraphicDivId) {
	
	var div = document.getElementById(fuzzificationGraphicDivId);
	if ((div != null) && (div != undefined)) {
		div.innerHTML = ""; // Re-initialize
	
		var container = document.createElement('div');
		container.id = fuzzificationGraphicDivId + "Container";
		container.style.width= "50em"; 
		container.style.height= "15em";
		container.style.margin= "2em";
		container.style['text-align'] = "center";
		container.style['align'] = "center";
		div.appendChild(container);
	
		// alert("insertFuzzificationGraphicRepresentation not implemented yet !!!");
		drawChart(container.id, index);
	}
}

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

// var charts = new Array(); // globally available
var chart = null;

function drawChart(identifier, index) {
	
	if ((fuzzificationsFunctions[index] != null) && 
		(fuzzificationsFunctions[index].ownersPersonalizations != null)) {

		$(document).ready(function() {
			  // charts[i] = 
		      chart = new Highcharts.Chart({
		         chart: {
	    	        renderTo: identifier,
	        	    type: 'line' //,
/*					style: { margin: '0 auto' }
*/
		         },
		         title: {
	    	        text: fuzzificationFunctionNameInColloquial(fuzzificationsFunctions[index].predDefined, 'all')
	        	 },
		         xAxis: {
					title: {
						text: fuzzificationFunctionNameInColloquial(fuzzificationsFunctions[index].predNecessary, 'adjective') + 
								" of a " + fuzzificationFunctionNameInColloquial(fuzzificationsFunctions[index].predNecessary, 'subject')
					},
					min: 0

		            // categories: ['Apples', 'Bananas', 'Oranges']
		         },
		         yAxis: {
					title: {
						text: 'Truth value'
					},
					min: 0,
					max: 1
		         	// categories: [0, 0.25, 0.5, 0.75, 1]
	    	     },
/*	    	     navigator: {
	    	    	 height: 30,
	    	    	 width: 40
	    	     },
	    	     center: [60, 45],
	    	     size: 50,
*/
	        	 series: fuzzificationsFunctions[index].ownersPersonalizations
		         		/*	[{
		            name: 'Jane',
		            data: [1, 0, 4]
	    	     }, {
	        	    name: 'John',
	            	data: [5, 7, 3]
		         }] */
		      });
		   });
	}
}

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

function saveFuzzificationPersonalizations(saveMyFuzzificationStatusDivId, mode, fileName, fileOwner, index, indexOfMine) {
	document.getElementById(saveMyFuzzificationStatusDivId).innerHTML = loadingImageHtml(false);
	
	// Aqui generamos la query, la ejecutamos y mostramos el resultado.
	var query = urlMappingFor('SaveProgramFuzzificationRequest');
	query += "&fileName=" + fileName + "&fileOwner="+ fileOwner + "&predOwner=";
	
	if (mode == 'basic') query += localUserName;
	else query += "default definition";
	
	query += "&predDefined=" + fuzzificationsFunctions[index].predDefined;
	query += "&predNecessary=" + fuzzificationsFunctions[index].predNecessary;
	
	for (var i=0; i < fuzzificationsFunctions[index].ownersPersonalizations[indexOfMine].data.length; i++) {
		fpx = fuzzificationsFunctions[index].ownersPersonalizations[indexOfMine].data[i][0];
		fpy = fuzzificationsFunctions[index].ownersPersonalizations[indexOfMine].data[i][1];
		
		query += "&fpx["+i+"]=" + fpx;
		query += "&fpy["+i+"]=" + fpy;
	}

	
	
	$.get(query, 
			function(data, textStatus, jqxhr) {
				// alert(data);
				document.getElementById(saveMyFuzzificationStatusDivId).innerHTML = data;
				
	});

	
	
	
}


/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

function barValueChanged(barObject, i, indexOfMine, index, fuzzificationGraphicDivId) {
	
	if ((barObject == null) || (barObject == undefined)) {
		debug.info("barObject is null or undefined in barValueChanged.");
		return;
	}
	
	var valueOriginal=barObject.value;
	if  ((valueOriginal == null) || (valueOriginal == undefined) || (isNaN(valueOriginal))) {
		alert("Erroneous value. I'll reset to 0.");
		barObject.value = 0;
		valueOriginal = 0;
		return;
	}
	
	var valueFloat = parseFloat(valueOriginal);
	var valueToShow = 0;
	
	if ((valueFloat != null) && (valueFloat != undefined) && (! isNaN(valueFloat))) {
		valueToShow = valueFloat.toFixed(2);
	}
	else {
		valueFloat = 0;
		valueToShow = 0;
	}
	
	// Show the value in the div.
	var div = document.getElementById("fuzzificationBarValue["+i+"]");
	div.innerHTML = valueToShow;
	
	// Modify the stored value 
	fuzzificationsFunctions[index].ownersPersonalizations[indexOfMine].data[i][1] = valueFloat;

	// Display in the graphic the result.
	insertFuzzificationGraphicRepresentation(index, fuzzificationGraphicDivId);
}

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

// EOF

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

<% if (JspsUtils.getStringWithValueS().equals("N")) { %>
</script>
<% } %>
