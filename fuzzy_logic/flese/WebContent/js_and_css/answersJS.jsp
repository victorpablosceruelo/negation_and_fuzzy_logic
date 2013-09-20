/*!
 * auxiliarJS Library v1
 * auxiliar javascript code
 * Author: Victor Pablos Ceruelo
 */

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

<%@page import="constants.KConstants"%>
<%@page import="auxiliar.JspsUtils"%>

<% if (JspsUtils.getStringWithValueS().equals("N")) { %>
<script type="text/javascript">
<% } %>

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

var queryAnswers = null;

function cleanUpQueryAnswers() {
	queryAnswers = new Array();
}

function addToProgramQueryAnsers(index, realAnswer) {
	queryAnswers[index] = realAnswer;
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */


// Sort the array of answers.
function arraySortFunction(elt1, elt2) {
	// Info:
    // return -1: Sort "a" to be a lower index than "b"
	// return 0: "a" and "b" should be considered equal, and no sorting performed.
	// return +1: Sort "b" to be a lower index than "a".
	if (elt1[elt1.length -1] == "Truth Value") return -1;
	if (elt2[elt2.length -1] == "Truth Value") return +1;
	var elt1float = parseFloat(elt1[elt1.length -1]);
	var elt2float = parseFloat(elt2[elt2.length -1]);
	
	if (elt1float >  elt2float) return -1;
	if (elt1float == elt2float) return 0;
	if (elt1float <  elt2float) return +1;
}

function truncate_truth_value (truth_value) {
	if (truth_value == "Truth Value") return truth_value;
	
	var newValue = parseFloat(truth_value);
	if ((newValue != null) && (newValue != undefined) && (newValue != NaN) && (newValue != "Truth Value")) {
		return newValue.toFixed(2);
	} 
	else {
		return truth_value;
	}
}

function resultOver(value, index) {
	if ((queryAnswers[index] == null) || (queryAnswers[index] == undefined)) return false;
	var realValue = queryAnswers[index][queryAnswers[index].length -1];
	if ((realValue == null) || (realValue == undefined)) return false;
	if (realValue == "Truth Value") return true;
	return (parseFloat(realValue) > value);
}

function prologNameInColloquialLanguage(textLabelIn) {
	var textLabel = null;
	
	if ((textLabelIn == null) || (textLabelIn == undefined)) {
		alert("prologNameInColloquialLanguage: null or undefined input.");
		return null;
	}
	if (! isString(textLabelIn)) {
		alert("prologNameInColloquialLanguage: input is not a string.");
		return null;		
	}
	
	// debug.info("textLabel: " + textLabelIn);
	var i = textLabelIn.indexOf("_");
	while (i != -1) {
		textLabel = ""; //Initialize
		textLabel += textLabelIn.substring(0, i);
		textLabel += " ";
		textLabel += textLabelIn.substring(i+1, textLabelIn.length);
		// debug.info(textLabel);
		i = textLabel.indexOf("_");
		textLabelIn = textLabel;
	}
	textLabel = textLabelIn;
	return textLabel;
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

var infosForQueryAnswers = null;

function infoForQueryAnswers (tableName, queryAnswerIndex) {
	this.tableName = tableName;
	this.queryAnswersIndexes = new Array();
	if ((queryAnswerIndex != null) && (queryAnswerIndex != undefined)) {
		this.queryAnswersIndexes[0] = queryAnswerIndex;
	}
}

function insertInfoForQueryAnswers (tableName, queryAnswerIndex) {
	if (infosForQueryAnswers == null) {
		infosForQueryAnswers = new Array();
	}
	
	var i = 0;
	var found = false;
	while ((i < infosForQueryAnswers.length) && (! found)) {
		if (tableName == infosForQueryAnswers[i].tableName) {
			found = true;
		}
		else i++;
	}
	
	if (! found) {
		infosForQueryAnswers[infosForQueryAnswers.length] = new infoForQueryAnswers(tableName, queryAnswerIndex);
	}
	else {
		if ((queryAnswerIndex != null) && (queryAnswerIndex != undefined)) {
			infosForQueryAnswers[i].queryAnswersIndexes[infosForQueryAnswers[i].queryAnswersIndexes.length] = queryAnswerIndex;
		}
	}
}


var queryAnswersOver70 = null;
var queryAnswersOver50 = null;
var queryAnswersOver0 = null;

function showQueryAnswers(runQueryDivId) {
	
	if (! isString(runQueryDivId)) {
		debug.info("ERROR: runQueryDivId is not a string.");
		alert("ERROR: runQueryDivId is not a string.");
		return;
	}
	var runQueryDiv = document.getElementById(runQueryDivId);
	if ((runQueryDiv == null) || (runQueryDiv == undefined)) {
		debug.info("ERROR: runQueryDiv is null or undefined.");
		alert("ERROR: runQueryDiv is null or undefined.");
		return;
	}
	
	// Initialize.
	infosForQueryAnswers = null;
	
	if ((queryAnswers != null) && (queryAnswers.length > 1)) queryAnswers.sort(arraySortFunction);

	var best10answersName = "10 best results";
	var answersOver70Name = "Results over 70%";
	var answersOver50Name = "Results over 50%";
	var answersOver0Name  = "Results over 0%";
	var allAnswers        = "All results";
	
	// Initialize to keep the final order.
	insertInfoForQueryAnswers(best10answersName, null);
	insertInfoForQueryAnswers(answersOver70Name, null);
	insertInfoForQueryAnswers(answersOver50Name, null);
	insertInfoForQueryAnswers(answersOver0Name, null);
	insertInfoForQueryAnswers(allAnswers, null);
	
	if ((queryAnswers != null) && (queryAnswers.length > 0)) {
		for (var i=0; i<queryAnswers.length; i++) {
			if ((i <= 10) && (resultOver(0, i))) insertInfoForQueryAnswers(best10answersName, i);
			if (resultOver(0.7, i)) insertInfoForQueryAnswers(answersOver70Name, i);
			if (resultOver(0.5, i)) insertInfoForQueryAnswers(answersOver50Name, i);
			if (resultOver(0, i)) insertInfoForQueryAnswers(answersOver0Name, i);
			if (resultOver(0, i)) insertInfoForQueryAnswers(allAnswers, i);
		}
	}
	
	// Create the real tables (if necessary) and put inside the results.
	var noAnswers = true;
	for (var i=0; i<infosForQueryAnswers.length; i++) {
		// The first answer is information about the database fields.
		if (infosForQueryAnswers[i].queryAnswersIndexes.length > 1) {
			noAnswers = false;
		}
	}
	
	// if (noAnswers) {
	// 	runQueryDiv.innerHTML = "The query has no answers. Maybe the database is empty? ";
	// }
	// else {
	if (! noAnswers) {	
		runQueryDiv.innerHTML = "";

		var tabsDiv = document.createElement('div');
		tabsDiv.id = "tabs";
		runQueryDiv.appendChild(tabsDiv);
		var tabsDivList = document.createElement('ul');
		tabsDiv.appendChild(tabsDivList);
		
		var html = null;
		var i = null;
		var j = null;
		var k = null;
		var tabDiv = null;
		var tabContentDiv = null;
		var row = null;
		var cell = null;
		
		html = "";
		j = 1;
		for (i=0; i<infosForQueryAnswers.length; i++) {
			// The first answer is information about the database fields.
			if (infosForQueryAnswers[i].queryAnswersIndexes.length > 1) {
				html += "<li><a href='#tabs-"+j+"'>"+infosForQueryAnswers[i].tableName+"</a></li>";
				j++;
			}
		}
		tabsDivList.innerHTML = html;
		
		html = "";
		j = 1;
		for (i=0; i<infosForQueryAnswers.length; i++) {
			// The first answer is information about the database fields.
			if (infosForQueryAnswers[i].queryAnswersIndexes.length > 1) {
				tabDiv = document.createElement('div');
				tabDiv.id = "tabs-" + j;
				j++;
				tabsDiv.appendChild(tabDiv);
				
				tabContentDiv = document.createElement('div');
				tabContentDiv.className = "queryAnswersTable";
				tabDiv.appendChild(tabContentDiv);
				
				// Now insert each answer in a row, inside the table
				for (k=0; k<infosForQueryAnswers[i].queryAnswersIndexes.length; k++) {
					row = document.createElement('div');
					row.className = "queryAnswersTableRow";
					tabContentDiv.appendChild(row);
					
					var answer = queryAnswers[infosForQueryAnswers[i].queryAnswersIndexes[k]];
					for (var l=1; l<answer.length; l++) {
						cell = document.createElement('div');
						cell.className = "queryAnswersTableCell";
						row.appendChild(cell);
						if (l+1 < answer.length) {
//							if (k==0) {
								cell.innerHTML = prologNameInColloquialLanguage(answer[l]);
//							}
//							else {
//								cell.innerHTML = answer[l];
//							}
						}
						else {
							cell.innerHTML = truncate_truth_value(answer[l]);
						}
					}
				}
			}
		}
		
		// Enable tabs
		$( "#tabs" ).tabs();
	}
}

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

function debugQueryAnswers(parentDivId) {
	var parentDiv = document.getElementById(parentDivId);
	var debugDiv = document.createElement('div');
	debugDiv.className = "debugTable";
	parentDiv.appendChild(debugDiv);
	var rowDiv = null;
	var cellDiv = null;
	
	for (var i=0; i<queryAnswers.length; i++) {
		rowDiv = document.createElement('div');
		rowDiv.className = "debugTableRow";
		debugDiv.appendChild(rowDiv);
		
		for (var j=0; j < queryAnswers[i].length; j++) {
			cellDiv = document.createElement('div');
			cellDiv.className = "debugTableCell";
			rowDiv.appendChild(cellDiv);
			
			cellDiv.innerHTML = queryAnswers[i][j];
		}
	}
}


// EOF

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

<% if (JspsUtils.getStringWithValueS().equals("N")) { %>
</script>
<% } %>


