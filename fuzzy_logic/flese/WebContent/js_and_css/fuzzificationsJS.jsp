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

var fuzzificationFunction = null;

function fuzzificationPoints (predOwner, functionPoints) {
	this.name = predOwner;
	this.data = functionPoints;
}

function fuzzificationFunction(predDefined, predNecessary, fuzzificationPoints) {
	this.predDefined = predDefined;
	this.predNecessary = predNecessary;
	this.fuzzificationPoints = fuzzificationPoints; // ownersPersonalizations
}

function setFuzzificationFunction (predDefined, predNecessary, fuzzificationPoints) {
	fuzzificationFunction = new fuzzificationFunction(predDefined, predNecessary, fuzzificationPoints);
}

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

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

function personalizationFunctionChanged(comboBox, PersonalizationFunctionUnderModificationDivId, url) {
	
	var params = getComboBoxValue(comboBox);
	loadAjaxIn(PersonalizationFunctionUnderModificationDivId, url + params);
	
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

// EOF

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

<% if (JspsUtils.getStringWithValueS().equals("N")) { %>
</script>
<% } %>
