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

function loadingImageHtml(withNewLine) {
	var result = "<img src=\"images/loading.gif\" width=\"200\" alt=\"loading\" title=\"loading\" />";
	
	if (withNewLine) return "<br />" + result;
	else return result;
}

function getContainer(containerId) {
	var container = null;
	if (containerId === null) {
		debug.info("getContainer: containerId is null");
	}
	else {
		if (!isString(containerId)) {
			debug.info("getContainer: containerId is not a string.");
		}
		else {
			container = document.getElementById(containerId);
			if (container === null) {
				debug.info("getContainer: container is null");
				debug.info("getContainer: containerId is " + containerId);
			}
		}
	}
	return container;
}

function debugInfoIfVarIsNull(varValue, varName, preMsg) {
	if (varValue === null) {
		debug.info(preMsg + ": " + varName + " is null.");
	}
}

function executeAjaxLoadedPageJS(loadedContent) {
	// From http://stackoverflow.com/questions/10888326/executing-javascript-script-after-ajax-loaded-a-page-doesnt-work
	var content = loadedContent;
	// xmlhttp.responseText;
	
	var scriptStart = '<%= KConstants.JavaScriptScripts.jsStart %>';
	var scriptEnd = '<%= KConstants.JavaScriptScripts.jsEnd %>';

    var script = content.match("<%=KConstants.JavaScriptScripts.jsRegex %>");
    if (script != null) {
		script = script.toString().replace(scriptStart, '');
		script = script.replace(scriptEnd, '');
		eval(script);
	}
}

function loadAjaxIn(containerId, ajaxPageUrl) {
	// debug.info("loadAjaxIn("+containerId + ", " + ajaxPageUrl + ")");
	var container = getContainer(containerId);
	if (container === null) {
		debug.info("aborted loadAjaxIn("+containerId + ", " + ajaxPageUrl + ")");
		return;
	}

	container.innerHTML=loadingImageHtml(true);
    $.ajax({
        method: 'get',
        url: ajaxPageUrl,
        data: 'page=' + $(this).attr('rel'),
        beforeSend: function() {
            container.innerHTML=loadingImageHtml(true);
        },
        complete: function() {
        	debug.info("load of html is complete.");
        	// container.innerHTML=loadingImageHtml(true);
        },
        success: function(html) {
        	console.log("loading html: " + html);
        	container.innerHTML=html;
        	executeAjaxLoadedPageJS(html);
		},
		fail: function() { 
			alert("error: Impossible to load page " + ajaxPageUrl); 
		}
	});

}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function clearScreen() {
	var mainSection = getContainer('mainSecDiv');
	mainSection.innerHTML = "";
}

function showMsgs(msgs) {
	var msgsContainerId = 'msgs';
	var msgsContainer = getContainer(msgsContainerId);

	msgsContainer.innerHTML = "";
	if (Array.isArray(msgs)) {
		for (var i=0; i<msgs.length; i++) {
			var subDiv = document.createElement('div');
			// row.className = "";
			// row.id = destinyAddLine;
			// document.getElementById(msgsContainerId)
			subDiv.innerHTML = msgs[i];
			msgsContainer.appendChild(subDiv);
		}
	}
}


function isString(o) {
	var result = false;
	if ((o != null) && (o != undefined)) {
		result = (typeof o == "string") || (o instanceof String) || (typeof o == "object" && o.constructor === String);
	}
	// alert("isString returns " + result);
	return result;
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function getComboBoxValue(comboBox) {
	if (comboBox == null) return "";
	var comboBoxSelectedIndex = comboBox.selectedIndex;
	if (comboBoxSelectedIndex == null) return "";
	var comboBoxSelectedField = comboBox.options[comboBoxSelectedIndex];
	if (comboBoxSelectedField == null) return "";
	var comboBoxValue = comboBoxSelectedField.value;
	var comboBoxText = comboBoxSelectedField.text;
	var comboBoxName = comboBoxSelectedField.name;
	var comboBoxTitle = comboBoxSelectedField.title;
	debug.info("getComboBoxValue: value: " + comboBoxValue);
	debug.info("getComboBoxValue: text: " + comboBoxText);
	debug.info("getComboBoxValue: name: " + comboBoxName);
	debug.info("getComboBoxValue: title: " + comboBoxTitle);

	if (comboBoxValue === null) {
		debug.info("getComboBoxValue: comboBoxValue is null (===).");
		return "";
	}
	
	// alert("comboBoxValue: " + comboBoxValue);
	if (comboBoxValue == null) {
		debug.info("getComboBoxValue: comboBoxValue is null (==).");
		return "";
	}
	
	if (comboBoxValue == "") {
		debug.info("getComboBoxValue: comboBoxValue is empty string.");
		return "";
	} 

	if (comboBoxValue == "----") {
		debug.info("getComboBoxValue: comboBoxValue is default value (----).");
		return "";
	}
	
	return comboBoxValue;
}

function splitStringResult(head, tail) {
	this.head = head;
	this.tail = tail;
}

function splitString(inputString, separation) {
	var splitStringResult = inputString;
	var i = comboBoxValue.indexOf(separation);
	if (i != -1) {
		splitStringResult = new splitStringResult('', '');
		splitStringResult.head = comboBoxValue.substring(0, i);
		splitStringResult.tail = comboBoxValue.substring(i+separation.length);
	}
	return splitStringResult;
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function fileInfo(fileName, fileOwner) {
	this.fileName = fileName;
	this.fileOwner = fileOwner;
}

// Declare as global the variable containing the files list.
var filesList = null;

function cleanUpFilesList() {
	filesList = new Array();
}

function addToFilesList(index, fileName, fileOwner) {
	filesList[index] = new fileInfo(fileName, fileOwner);
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

var programIntrospection = new Array();

function cleanUpProgramIntrospection() {
	programIntrospection = null;
	programIntrospection = new Array();
}

function addToProgramIntrospection(index, predInfo) {
	programIntrospection[index] = predInfo;
}

function predMoreInfoClass(key, value) {
	this.key = key;
	this.value = value;
}

function predInfo(predName, predArity, predType, predMoreInfo) {
	this.predName = predName;
	this.predArity = predArity;
	this.predType = predType;
	this.predMoreInfo = new Array();
	for (var i=0; i<predMoreInfo.length; i++) {
		if (predMoreInfo[i].length >= 2) {
			this.predMoreInfo[i] = new predMoreInfoClass(predMoreInfo[i][0], predMoreInfo[i][1]);
			for (var j=2; j<predMoreInfo[i].length; j++) {
				alert("predMoreInfo["+i+"][" +j +"]: " + predMoreInfo[i][j]);
			}
		}
	}
	
}

function selectedProgramDatabaseInfo(fileName, fileOwner) {
	this.fileName = fileName;
	this.fileOwner = fileOwner;
}

function getProgramDatabaseComboBoxValue(comboBox) {
	var comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
	var selectedProgramDatabase = null;
	
	// alert("comboBoxValue: " + comboBoxValue);
	if ((comboBoxValue != null) && (comboBoxValue != "") && (comboBoxValue != "----")) {
		var separation = "-owned-by-";
	
		i = comboBoxValue.indexOf(separation);
		if (i != -1) {
			selectedProgramDatabase = new selectedProgramDatabaseInfo('', '');
			selectedProgramDatabase.fileName = comboBoxValue.substring(0, i);
			selectedProgramDatabase.fileOwner = comboBoxValue.substring(i+separation.length);
		}
	}
	return selectedProgramDatabase;
}

function selectedProgramDatabaseChanged(comboBox) {
	var selectQueryDivId = '<%= KConstants.JspsDivsAndFields.selectQueryDivId %>'; 
	var runQueryDivId = '<%= KConstants.JspsDivsAndFields.runQueryDivId %>';
	
	var selectQueryDiv = document.getElementById(selectQueryDivId);
	debugInfoIfVarIsNull(selectQueryDiv, "selectQueryDiv", "selectedProgramDatabaseChanged");
	selectQueryDiv.innerHTML = loadingImageHtml(true);
	
	var runQueryDiv = document.getElementById(runQueryDivId);
	debugInfoIfVarIsNull(runQueryDiv, "runQueryDiv", "selectedProgramDatabaseChanged");
	runQueryDiv.innerHTML = "";
	
	var selectedProgramDatabaseUrl = getComboBoxValue(comboBox);
	
	if (selectedProgramDatabaseUrl == "") {
		selectQueryDiv.innerHTML="Please choose a valid database to continue.";
	}
	else {
		loadAjaxIn(selectQueryDivId, selectedProgramDatabaseUrl);
	}
	
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

var databaseName = "";

function setDatabaseParam(databaseNameIn) {
	databaseName = databaseNameIn;
}

function getDatabaseParam() {
	return databaseName;
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function selectedQueryStartTypeChanged(comboBox) {
	var queryLinesContainerId = "<%=KConstants.JspsDivsAndFields.queryLinesContainerId %>";
	var searchOrPersonalizeTableId = "<%=KConstants.JspsDivsAndFields.searchOrPersonalizeTableId %>";
	
	var startupType = getComboBoxValue(comboBox);
	debug.info("startupType changed to " + startupType);
	
	resetQueryLinesCounterFieldValue();
	var queryLinesContainerDiv = getContainer(queryLinesContainerId); 
	queryLinesContainerDiv.innerHTML="";
	
	var searchOrPersonalizeTable = document.getElementById(searchOrPersonalizeTableId);
	
	if ((startupType == "") || (startupType == '')) {
		queryLinesContainerDiv.innerHTML="Please select what are you looking for.";
		queryLinesContainerDiv.style.display='none';
		searchOrPersonalizeTable.style.visibility = 'hidden'; 
	}
	else {
		searchOrPersonalizeTable.style.visibility = 'visible'; 
		queryLinesContainerDiv.style.display='block';
		loadAjaxIn(queryLinesContainerId, startupType);
	}
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function resetQueryLinesCounterFieldValue() {
	var queryLinesCounterFieldId = "<%=KConstants.JspsDivsAndFields.counterFieldId %>";
	document.getElementById(queryLinesCounterFieldId).value = 0;
}

function incrementQueryLinesCounterFieldValue() {
	var queryLinesCounterFieldId = "<%=KConstants.JspsDivsAndFields.counterFieldId %>";
	document.getElementById(queryLinesCounterFieldId).value ++;
	return getQueryLinesCounterFieldValue();
}

function getQueryLinesCounterFieldValue() {
	var queryLinesCounterFieldId = "<%=KConstants.JspsDivsAndFields.counterFieldId %>";
	if ((document.getElementById(queryLinesCounterFieldId) != null) &&
		(document.getElementById(queryLinesCounterFieldId) != undefined) &&
		(document.getElementById(queryLinesCounterFieldId).value != null) &&
		(document.getElementById(queryLinesCounterFieldId).value != undefined)) {
		return document.getElementById(queryLinesCounterFieldId).value;
	}
	else return 0;
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function selectQueryAddLine(urlQueryAddLine, urlQueryAddAggregator) {
	debug.info("selectQueryAddLine");
	debug.info(urlQueryAddLine);
	debug.info(urlQueryAddAggregator);
	var queryLinesTableId = "<%=KConstants.JspsDivsAndFields.queryLinesTableId %>";
	var queryLinesAggregatorTableId = "<%=KConstants.JspsDivsAndFields.queryLinesAggregatorTableId %>";
	
	var queryLinesCounter = getQueryLinesCounterFieldValue();
	var queryLineId = "queryLine[" + queryLinesCounter + "]";
	var destinyAddLine = queryLineId + ".row";

	var row = document.createElement('div');
	row.className = queryLinesTableId + "Row";
	row.id = destinyAddLine;
	document.getElementById(queryLinesTableId).appendChild(row);
	
	var lineInfo = "&" + "<%= KConstants.Request.lineNumberParam %>" + "=" + queryLinesCounter;
	var lineId = "&" + "<%= KConstants.Request.lineIdParam %>" + "=" + queryLineId;
	var linesCounter = "&" + "<%= KConstants.QueryParams.queryLinesCounter %>" + "=" + queryLinesCounter;
	
	loadAjaxIn(destinyAddLine, urlQueryAddLine + lineInfo + lineId);
	loadAjaxIn(queryLinesAggregatorTableId, urlQueryAddAggregator + lineInfo + lineId + linesCounter);
	
	queryLinesCounter = incrementQueryLinesCounterFieldValue();
	// Do not allow navigator to call url.
	return false;
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function selectPredicateChanged(comboBox, lineId, negUrl, quantUrl, opUrl, valueUrl) {
	// lineId + ".predicate"
	var startupType = getComboBoxValue(comboBox);
	
	loadAjaxIn(lineId + ".negationDiv", negUrl + startupType);
	loadAjaxIn(lineId + ".quantifierDiv", quantUrl + startupType);
	loadAjaxIn(lineId + ".operatorDiv", opUrl + startupType);
	loadAjaxIn(lineId + ".valueDiv", valueUrl + startupType);
	
	/*
	if (foundPredInfo.predType[typeIndex][foundPredInfo.predType[typeIndex].length -1] == 'rfuzzy_truth_value_type') {
		insertChooseQuantifier(queryLineId, rowId, 1, queryLinesTableId);
		insertChooseQuantifier(queryLineId, rowId, 0, queryLinesTableId);
	}
	else {
		insertRfuzzyComputeOperator(queryLineId, rowId, index, typeIndex, queryLinesTableId);
		insertRfuzzyComputeArgument(queryLineId, rowId, index, typeIndex, queryLinesTableId);
	}
	*/
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function aggregatorDetailsShow(toShowMsgId, toHideMsgId, chooseAgregatorInfoCellId, chooseAgregatorCellId) {
	document.getElementById(toShowMsgId).style.display='none';
	
	document.getElementById(toHideMsgId).style.display='inline';
	document.getElementById(toHideMsgId).className = "queryLineAggregatorTable";
	
	document.getElementById(chooseAgregatorInfoCellId).style.display='table-cell';
	document.getElementById(chooseAgregatorInfoCellId).className = "queryLineAggregatorTable";
	
	document.getElementById(chooseAgregatorCellId).style.display='table-cell';
	document.getElementById(chooseAgregatorCellId).className = "queryLineAggregatorTable";
	return false;
}

function aggregatorDetailsHide(toShowMsgId, toHideMsgId, chooseAgregatorInfoCellId, chooseAgregatorCellId) {
	document.getElementById(toHideMsgId).style.display='none';
	document.getElementById(chooseAgregatorInfoCellId).style.display='none';
	document.getElementById(chooseAgregatorCellId).style.display='none';
	
	document.getElementById(toShowMsgId).style.display='inline';
	document.getElementById(toShowMsgId).className = "queryLineAggregatorTable";
	return false;		
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function comboBoxCheckValue(fieldName, errorText) {
	// alert("comboBoxCheckValue(" + fieldName + ", " + errorText + ")");
	if (! isString(fieldName)) return null;
	
	var comboBox = document.getElementById(fieldName);
	var comboBoxValue = null;
	
	// alert("step 1 comboBoxCheckValue " + fieldName);
	if (comboBox == null) {
		debug.info("null comboBox "+fieldName);
		return null;
	}
	else {
		if (comboBox.selectedIndex != null) {
			comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
			if (comboBoxValue != null) {
				if (comboBoxValue != '----') {
					debug.info("comboBoxValue ("+fieldName+"): " + comboBoxValue);
					return comboBoxValue;
				}
				else {
					if (errorText != null) {
						debug.info("choosingIsMandatory in comboBox "+fieldName);
						alert(errorText);
					}
					return null;
				}
			}
			else {
				debug.info("null comboBoxValue in comboBox "+fieldName);
				return null; 				
			}
		}
		else {
			/// Siempre se mete por aqui !!!!!
			debug.info("null selectedIndex in comboBox "+fieldName);
			return null;
		}
	}
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function comboBoxOrTextBoxCheckValue(fieldName, errorText) {
	// document.getElementsByName(fieldName);
	var textField = document.getElementById(fieldName);
	if (textField == null) {
		debug.info("null textField "+fieldName);
		return null;
	}
	else {
		if (textField.value == null) {
			debug.info("null value in textField "+fieldName);
			return comboBoxCheckValue(fieldName, errorText);
		}
		if (textField.value == undefined) {
			debug.info("undefined value in textField "+fieldName);
			if (errorText != null) alert(errorText);
			return null;
		}
		
		if ((textField.value != null) && (textField.value != undefined)) {
			if (textField.value == '') {
				debug.info("textField.value ("+fieldName+"): empty string");
				alert(errorText);
				return null;
			}
			else {
				if ((errorText != null) && (textField.value == '----')) {
					alert(errorText);
					return null;
				}
				else {
					debug.info("textField.value ("+fieldName+"): " + textField.value);
					return textField.value;
				}
			}
		}
	}	
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

/* This function makes a soft test of the query. The one in charge of running the query is below. */
function runQueryAfterSoftTests(url) {

	var runQueryDivId = "<%= KConstants.JspsDivsAndFields.runQueryDivId %>";
	debug.info("runQueryAfterSoftTests");
	
	var error = false;
	var action = url;
	
	var chooseQueryStartType = getDatabaseParam();
	if (chooseQueryStartType == null) {
		error = true;
	}
	else {
		action += "&selectQueryStartupType=" + chooseQueryStartType; 
	
		actionTmp = comboBoxOrTextBoxCheckValue("queryLinesCounter", null);
		if (actionTmp != null) action += "&queryLinesCounter=" +actionTmp;
		else error = true;		
		
		var actionTmp = null;
		var queryLinesCounter = getQueryLinesCounterFieldValue();
		for (var i=0; i < queryLinesCounter; i++) {
			// predicate
			actionTmp = comboBoxCheckValue("queryLine["+i+"].selectPredicate", null);
			if (actionTmp != null) action += "&queryLine["+i+"].selectPredicate=" + actionTmp;
			
			if (needsComputeFields(actionTmp, chooseQueryStartType)) {
				// operator 
				actionTmp = comboBoxCheckValue("queryLine["+i+"].selectRfuzzyComputeOperator", "Please fill the operator in subquery number " + (i+1));
				if (actionTmp != null) action += "&queryLine["+i+"].selectRfuzzyComputeOperator=" + actionTmp;
				else error = true;
		
				// value 
				actionTmp = comboBoxOrTextBoxCheckValue("queryLine["+i+"].selectRfuzzyComputeValue", "Please fill the value in subquery number " + (i+1));
				if (actionTmp != null) action += "&queryLine["+i+"].selectRfuzzyComputeValue=" +actionTmp;
				else error = true;
			}
			// quantifier 0
			actionTmp = comboBoxCheckValue("queryLine["+i+"].selectQuantifier_0", null);
			if (actionTmp != null) action += "&queryLine["+i+"].selectQuantifier_0=" + actionTmp;

			// quantifier 1
			actionTmp = comboBoxCheckValue("queryLine["+i+"].selectQuantifier_1", null);
			if (actionTmp != null) action += "&queryLine["+i+"].selectQuantifier_1=" + actionTmp;
		}
	}
	
	if (error) {
		var runQueryDiv = getContainer(runQueryDivId);
		runQueryDiv.innerHTML = "Your query contains errors. Please, fix them and press the search button again.";
	}
	else {
		loadAjaxIn(runQueryDivId, action);
	}

	// Tell the navigator not to follow the link !!!
	return false;
}

function needsComputeFields(actionTmp, chooseQueryStartType) {
	var i = 0;
	var found = false;
	while (i<programIntrospection.length && ! found){
		if (programIntrospection[i].predName == actionTmp) found = true;
		else i++;
	}
	if (! found) {
		debug.info("Predicate " + actionTmp + "could not be found.");
		return false;
	}
	else {
		var j = 0;
		found = false;
		while (j<programIntrospection[i].predType.length && ! found) {
			if (programIntrospection[i].predType[j][0] == chooseQueryStartType) found = true;
			else j++;
		}
		if (! found) {
			alert("Predicate " + actionTmp + "does not work for type " + chooseQueryStartType);
			return false;
		}
		else {
			if (programIntrospection[i].predType[j][programIntrospection[i].predType[j].length -1] != 'rfuzzy_truth_value_type') {
				return true;
			}
		}
	}
	return false;
}
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

userInformation = null;

function userInformationClass(fieldName, fieldValue) {
	this.fieldName = fieldName;
	this.fieldValue = fieldValue;
}

function cleanUpUserInformation () {
	userInformation = null;
	userInformation = new Array ();
}

function addToUserInformation(index, fieldName, fieldValue) {
	userInformation[index] = new userInformationClass(fieldName, fieldValue);
}

function insertUserOptions(parentDivId) {
	var parentDiv = document.getElementById(parentDivId);
	
	parentDiv.innerHTML = loadingImageHtml(true);
	
	$.getScript(urlMappingFor('UserOptionsRequest'), 
			function(data, textStatus, jqxhr) {
	   			parentDiv.innerHTML = "";
	   			
	   			var userInformationDiv = document.getElementById("userInformationDiv");
	   			if (userInformationDiv == null) {
	   				userInformationDiv = document.createElement('div');
	   				userInformationDiv.id = "userInformationDiv";
	   				userInformationDiv.className = "userInformationTable";
	   				parentDiv.appendChild(userInformationDiv);
	   			}
	   			userInformationDiv.innerHTML = "";
	   			
	   			var row = null;
	   			var cell = null;
	   			
	   			row = document.createElement('div');
	   			row.className = "userInformationTableRow";
	   			userInformationDiv.appendChild(row);
	   			
	   			cell = document.createElement('div');
	   			cell.className = "userInformationTableCell";
	   			cell.innerHTML = "Field Name";
	   			row.appendChild(cell);

	   			cell = document.createElement('div');
	   			cell.className = "userInformationTableCell";
	   			cell.innerHTML = "Value";
	   			row.appendChild(cell);

	   			for (var i=0; i<userInformation.length; i++) {
		   			row = document.createElement('div');
		   			row.className = "userInformationTableRow";
		   			userInformationDiv.appendChild(row);
		   			
		   			cell = document.createElement('div');
		   			cell.className = "userInformationTableCell";
		   			cell.innerHTML = userInformation[i].fieldName;
		   			row.appendChild(cell);

		   			cell = document.createElement('div');
		   			cell.className = "userInformationTableCell";
		   			cell.innerHTML = userInformation[i].fieldValue;;
		   			row.appendChild(cell);
	   			}
	   			
	   			insertFilesList(parentDivId);
	   			insertFileUploadFacility(parentDivId);
			});
	
	return false;
}

function insertFilesList (parentDivId) {
	
	var parentDiv = document.getElementById(parentDivId);
	var filesListDiv = document.getElementById('filesListDiv'); 
	if (filesListDiv == null) {
		filesListDiv = document.createElement('div');
		filesListDiv.id = "filesListDiv";
		parentDiv.appendChild(filesListDiv);
	}
	filesListDiv.innerHTML = loadingImageHtml(true);
	
	var fileViewContentsDiv = document.getElementById("fileViewContentsDiv");
	if (fileViewContentsDiv == null) {
		fileViewContentsDiv = document.createElement('div');
		fileViewContentsDiv.id = "fileViewContentsDiv";	 
		parentDiv.appendChild(fileViewContentsDiv);
	}
	fileViewContentsDiv.innerHTML = "";
		
	$.getScript(urlMappingFor('FilesListRequest'), 
			function(data, textStatus, jqxhr) {
				filesListDiv.innerHTML = "";
	   			filesListDiv.className = "filesListTable"; 			
	   			var row = null;
	   			var cell = null;
	   			
	   			var showHead = true;
	   			if ((filesList != null) && (filesList.length > 0)) {
	   				for (var i=0; i<filesList.length; i++) {
	   					if (filesList[i].fileOwner == localUserName) {
	   						if (showHead) {
	   							insertFilesListHead(filesListDiv.id);
	   							showHead = false;
	   						}
	   						
	   						row = document.createElement('div');
	   						row.className = "filesListTableRow";
	   						filesListDiv.appendChild(row);
		   			
	   						cell = document.createElement('div');
	   						cell.className = "filesListTableCell";
	   						cell.innerHTML = "<a href='#' title='view program file " + filesList[i].fileName + "' "+
		   								 	 "onclick='fileViewAction(" + i + ", \"" + fileViewContentsDiv.id + "\");' >" + 
		   								 	 filesList[i].fileName + "</a>";
	   						row.appendChild(cell);

	   						cell = document.createElement('div');
	   						cell.className = "filesListTableCell";
	   						cell.innerHTML = "<a href='#' title='remove program file " + filesList[i].fileName + "' "+
	   										 "onclick='removeFileAction(" + i + ", \"" + parentDivId + "\");' >" + 
	   										 "<img src='images/remove-file.gif' width='20em'>" + "</a>";
	   						row.appendChild(cell);

	   						cell = document.createElement('div');
	   						cell.className = "filesListTableCell";
		   					cell.innerHTML = "<a href='#' title='personalize program file " + filesList[i].fileName + "' "+
								 			 "onclick='return personalizeProgramFile(\"" + filesList[i].fileName + "\", \"" + filesList[i].fileOwner + "\", \"advanced\");'>" + 
								 			 "<img src='images/edit.png' width='20em'>" + "</a>";
		   					row.appendChild(cell);
	   					}
	   				}
	   			}
	   			
	   			if (showHead) {
	   				filesListDiv.innerHTML = "You do not owe any program file. Upload one by using the facility below.";
	   			}
			});
}

function insertFilesListHead(filesListDivId) {
	var filesListDiv = document.getElementById(filesListDivId);
	var row = null;
	var cell = null;
	
	row = document.createElement('div');
	row.className = "filesListTableRow";
	filesListDiv.appendChild(row);
		
	cell = document.createElement('div');
	cell.className = "filesListTableCell";
	cell.innerHTML = "Program File Name";
	row.appendChild(cell);

	cell = document.createElement('div');
	cell.className = "filesListTableCell";
	cell.innerHTML = "";
	row.appendChild(cell);

	cell = document.createElement('div');
	cell.className = "filesListTableCell";
	cell.innerHTML = "";
	row.appendChild(cell);
}

function fileViewAction(index, fileViewContentsDivId) {
	// alert("fileViewContentsDivId: " + fileViewContentsDivId);
	var fileViewContentsDiv = document.getElementById(fileViewContentsDivId);
	
	$.get(urlMappingFor('FileViewRequest') + "&fileName="+filesList[index].fileName+"&fileOwner="+filesList[index].fileOwner, 
			function(data, textStatus, jqxhr) {
				fileViewContentsDiv.innerHTML = data;
				
			    $(function() {
			    	$(fileViewContentsDiv).dialog({
		                // add a close listener to prevent adding multiple divs to the document
		                close: function(event, ui) {
		                    // remove div with all data and events
		                    // dialog.remove();
		                    fileViewContentsDiv.innerHTML = "";
		                },
		                modal: true,
		                resizable: true, 
		                height: "auto", // 800,
		                width: "auto", // 800,
		                title: 'Contents of program file ' + filesList[index].fileName
		            });
			        // $( "#" + fileViewContentsDivId ).dialog();
			    });
			});
	
	//prevent the browser to follow the link
	return false;
}

function removeFileAction (index, parentDivId) {
	$.get(urlMappingFor('FileRemoveRequest') + "&fileName="+filesList[index].fileName+"&fileOwner="+filesList[index].fileOwner, 
			function(data, textStatus, jqxhr) {
				// Reload the screen.
				// alert("parentDivId: " + parentDivId);
				insertFilesList(parentDivId);
			});
}

function getIframeWindow(iframe_object) {
	  var doc = null;

	  if (iframe_object.contentWindow) return iframe_object.contentWindow;
	  if (iframe_object.window) return iframe_object.window;

	  if ((doc == null) && iframe_object.contentDocument) doc = iframe_object.contentDocument;
	  if ((doc == null) && iframe_object.document) doc = iframe_object.document;

	  if ((doc != null) && doc.defaultView) return doc.defaultView;
	  if ((doc != null) && doc.parentWindow) return doc.parentWindow;

	  return null;
	}

function notNullNorundefined(value) {
	return ((value != null) && (value != undefined));
}

function insertFileUploadFacility(parentDivId) {
	var parentDiv = document.getElementById(parentDivId);
	var fileUploadDiv = document.getElementById("fileUploadDiv");
	if (fileUploadDiv == null) {
		fileUploadDiv = document.createElement('div');
		fileUploadDiv.id = "fileUploadDiv";
		parentDiv.appendChild(fileUploadDiv);
	}
	
	var uploadFormId = "uploadForm";
	var uploadStatusDivId = "uploadStatus";
	var uploadFormTargetiFrameId = "uploadFormTargetiFrame";
	
	fileUploadDiv.innerHTML = "Upload Program files <br />" + 
							  "<FORM ID='"+uploadFormId+"' ENCTYPE='multipart/form-data' method='POST' accept-charset='UTF-8' "+
							  "target='" + uploadFormTargetiFrameId+ "' action='" + urlMappingFor('FileUploadRequest') + "' >" +
							  		"<INPUT TYPE='file' NAME='programFileToUpload' size='50' "+
							  		"onchange='uploadActionOnChange(\""+uploadFormId+"\", \""+uploadStatusDivId+"\");'>" +
							  "</FORM>" +
							  "<div id='"+uploadStatusDivId+"'></div>" +
							  "<iframe id='"+uploadFormTargetiFrameId+"' name='"+uploadFormTargetiFrameId+"' "+
							  "style='display:none;'></iframe>";
	// This does not work on google chrome: "src='#' " 
    	
	$('#' + uploadFormTargetiFrameId).load(function() {
		// document.getElementById('#' + submitiFrameId);
		var responseHtmlText = null;
		var iFrameWindow = getIframeWindow(this);
		if ((notNullNorundefined(iFrameWindow)) && (notNullNorundefined(iFrameWindow.document)) && (notNullNorundefined(iFrameWindow.document.body))) {
			responseHtmlText = iFrameWindow.document.body.innerHTML;
			// Do something with response text.
			if (notNullNorundefined(responseHtmlText)) {
				iFrameWindow.document.body.innerHTML="";
				debug.info(responseHtmlText);
			}
			// Clear the content of the iframe.
			// this.contentDocument.location.href = '/images/loading.gif';
			// alert("responseText: " + responseHtmlText);
			document.getElementById(uploadStatusDivId).style.visibility = 'visible';
			document.getElementById(uploadStatusDivId).innerHTML = responseHtmlText;

			// Update the files list.
			insertFilesList (parentDivId);
		}
		  
	});	
}

function uploadActionOnChange(formId, uploadStatusDivId) {
	// alert("Upload Submit Action started ...");
	document.getElementById(uploadStatusDivId).style.visibility = 'visible';
	document.getElementById(uploadStatusDivId).innerHTML = loadingImageHtml(true);

	var form = document.getElementById(formId);
	form.submit();
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

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
	runQueryDiv.innerHTML = "";
	
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
	
	if (noAnswers) {
		runQueryDiv.innerHTML = "The query has no answers. Maybe the database is empty? ";
	}
	else {
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

function personalizeProgramFile(fileName, fileOwner, mode) {
	$.get(urlMappingFor('ListProgramFuzzificationsRequest') + "&fileName="+fileName+"&fileOwner="+fileOwner, 
		function(data, textStatus, jqxhr) {
			// Evaluate the JS code returned by the server.
			eval(data);

			if ((mode == 'basic') || (mode == 'advanced')) {
				// Show the personalization dialog to the user.
				showPersonalizeProgramFileDialog(fileName, fileOwner, mode);
			}
			else {
				alert("mode selected is not basic nor advanced. Internal error.");
			}
			
		});
	
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
	var personalizationDiv = document.createElement('div');
	
	var personalizationDivMainTable = document.createElement('div');
	personalizationDivMainTable.className = "personalizationDivMainTable";
	personalizationDiv.appendChild(personalizationDivMainTable);
	
	var row = null;
	var cell = null;
	var subTable = null;
	var subRow = null;
	var subCell = null;
	
	row = document.createElement('div');
	row.className = "personalizationDivMainTableRow";
	personalizationDivMainTable.appendChild(row);
	
	cell = document.createElement('div');
	cell.className = "personalizationDivMainTableCell";
	row.appendChild(cell);
	
	subTable = document.createElement('div');
	subTable.className = "personalizationDivSelectFuzzificationTable";
	cell.appendChild(subTable);
	
	subRow = document.createElement('div');
	subRow.className = "personalizationDivSelectFuzzificationTableRow";
	subTable.appendChild(subRow);
	
	subCell = document.createElement('div');
	subCell.className = "personalizationDivSelectFuzzificationTableCell";	
	subCell.innerHTML = "I want to personalize how it is determined that a &nbsp;";
	subRow.appendChild(subCell);
	
	subCell = document.createElement('div');
	subCell.className = "personalizationDivSelectFuzzificationTableCell";
	subRow.appendChild(subCell);
	
	var PersonalizationFunctionUnderModificationDivId = "PersonalizationDivCell";
	
	// Fill in the div.
	var personalizationSelectComboBoxId = "personalizationSelectComboBox";
	var html = "";
	html += "<select name='" + personalizationSelectComboBoxId + "' id='"+personalizationSelectComboBoxId+"' " +
			"onchange='personalizationFunctionChanged(this, \""+PersonalizationFunctionUnderModificationDivId + "\", \"" + 
														mode + "\", \"" + fileName + "\", \"" + fileOwner + "\");'>";
	html += "<option name=\'----\' value=\'----\'>----</option>";
	
	if (fuzzificationsFunctions != null) {
		for (var i=0; i < fuzzificationsFunctions.length; i++) {
			html+= "<option name=\'" + i + "\' value=\'" + i + "\'>" +
					fuzzificationFunctionNameInColloquial(fuzzificationsFunctions[i].predDefined, 'all') +
					" from the value it has for " + 
					fuzzificationFunctionNameInColloquial(fuzzificationsFunctions[i].predNecessary, 'adjective') + 
					"</option>";
		}
	}
	html += "</select>";
	subCell.innerHTML = html;
	
	row = document.createElement('div');
	row.className = "personalizationDivMainTableRow";
	personalizationDivMainTable.appendChild(row);
	
	cell = document.createElement('div');
	cell.className = "personalizationDivMainTableCell";
	cell.id = PersonalizationFunctionUnderModificationDivId;
	row.appendChild(cell);
	cell.innerHTML = "Select the fuzzification you want to personalize.";
	
	if (fuzzificationsFunctions == null) {
		alert("The program has nothing to personalize.");
	}
	else {
		// Show the div.
    	$(function() {
    		$(personalizationDiv).dialog({
    			// add a close listener to prevent adding multiple divs to the document
    			close: function(event, ui) {
    				// remove div with all data and events
    				// dialog.remove();
    				personalizationDiv.innerHTML = "";
    			},
    			modal: true,
    			resizable: true, 
    			height: "auto", // 800,
    			width: "auto", // 800,
    			title: 'Personalizing program file ' + fileName + " owned by " + fileOwner
    		});
    		// $( "#" + fileViewContentsDivId ).dialog();
    	});
	}
}

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

function personalizationFunctionChanged(comboBox, PersonalizationFunctionUnderModificationDivId, mode, fileName, fileOwner) {
	
	if ((comboBox == null) || (comboBox == undefined)) {
		alert("comboBox is invalid in personalizationFunctionChanged.");
		return;
	}
	
	var comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
	if (	(comboBoxValue == null) || (comboBoxValue == undefined) || (isNaN(comboBoxValue)) || 
			(comboBoxValue < 0) || (comboBoxValue >= fuzzificationsFunctions.length)) {
		alert("comboBoxValue is invalid in personalizationFunctionChanged.");
		return;
	}
	
	var index = comboBoxValue;
	var PersonalizationFunctionUnderModificationDiv = document.getElementById(PersonalizationFunctionUnderModificationDivId);
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


