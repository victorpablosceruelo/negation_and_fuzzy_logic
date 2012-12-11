/*!
 * auxiliarJS Library v1
 * Author: Victor Pablos Ceruelo
 */

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function loadingImageHtml() {
	return "<br /><img src=\"images/loading.gif\" width=\"200\" alt=\"loading\" title=\"loading\" />";
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function fileInfo(fileName, fileOwner) {
	this.fileName = fileName;
	this.fileOwner = fileOwner;
}

function insertProgramFileSelection(parentDivId) {
	var parentDiv = document.getElementById(parentDivId);
	parentDiv.innerHTML = loadingImageHtml();
	
	$.getScript(urlMappingFor('FilesListRequest'), 
			function(data, textStatus, jqxhr) {
		parentDiv.innerHTML = "";
		

		var selectDatabaseContainer = document.createElement('div');
		selectDatabaseContainer.id = "selectDatabaseContainerDiv";
		selectDatabaseContainer.className = "selectDatabaseTable";
		parentDiv.appendChild(selectDatabaseContainer);
		
		var row = document.createElement('div');
		row.className = "selectDatabaseTableRow";
		selectDatabaseContainer.appendChild(row);
		
		var cell = null;
		
		if ((filesList == null) || (filesList.length == 0)) {
			cell = document.createElement('div');
			cell.className = "selectDatabaseTableCell";
			cell.innerHTML = "No databases. Please upload one via your user options.";
			row.appendChild(cell);
		}
		else {
			cell = document.createElement('div');
			cell.className = "selectDatabaseTableCell";
			cell.id = "selectDatabaseDiv";
			row.appendChild(cell);

			var chooseProgramFileId = "selectProgramFile";
			var html = "";
			html += "<select name='"+chooseProgramFileId+"' id='"+chooseProgramFileId+"' ";
			html += "onchange='selectedProgramDatabaseChanged(this, \""+parentDivId+"\")' >";
			html += "<option id='----' name='----' title='----' value='----'>----</option>";
			for (var i=0; i<filesList.length; i++) {
				html += "<option id='" + filesList[i].fileName + "-owned-by-" + filesList[i].fileOwner + "' " +
						"name='" + filesList[i].fileName + "-owned-by-" + filesList[i].fileOwner + "' " +
						"title='" + filesList[i].fileName + "-owned-by-" + filesList[i].fileOwner + "' " +
						"value='" + filesList[i].fileName + "-owned-by-" + filesList[i].fileOwner + "'>" + 
						filesList[i].fileName + " ( owned by " + filesList[i].fileOwner + " ) " +
						"</option>";
			}
			html += "</select>";
			cell.innerHTML = html;
			
			//cell = document.createElement('div');
			//cell.className = "selectDatabaseTableCell";
			//cell.id = "personalizationButtonContainer";
			//row.appendChild(cell);
			
			//cell.innerHTML = "<INPUT type='submit' value='Personalize Program File' onclick='return personalizeProgramFile(\"" +
			//				 chooseProgramFileId + "\", \"basic\");'>";
		}
	});
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

function predInfo(predName, predArity, predType, predOtherInfo) {
	this.predName = predName;
	this.predArity = predArity;
	this.predType = predType;
	this.predOtherInfo = predOtherInfo;
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

function selectedProgramDatabaseChanged(comboBox, parentDivId) {
	// debug.info("parentDivId: " + parentDivId);
	var parentDiv = document.getElementById(parentDivId);
	
	var selectQueryDiv = document.getElementById('selectQueryDiv');
	if (selectQueryDiv == null) {
		selectQueryDiv = document.createElement('div');
		selectQueryDiv.id = 'selectQueryDiv';
		parentDiv.appendChild(selectQueryDiv);
	}

	selectQueryDiv.innerHTML = loadingImageHtml();
	
	runQueryDivId = 'runQueryDiv';
	var runQueryDiv = document.getElementById(runQueryDivId);
	if (runQueryDiv == null) {
		runQueryDiv = document.createElement('div');
		runQueryDiv.id = runQueryDivId;
		parentDiv.appendChild(runQueryDiv);
	}
	runQueryDiv.innerHTML = "";
	
	var selectedProgramDatabase = getProgramDatabaseComboBoxValue(comboBox);
	
	if (selectedProgramDatabase == null) {
		selectQueryDiv.innerHTML="Please choose a valid database to continue.";
	}
	else {
		$.getScript(urlMappingFor('ProgramFileIntrospectionRequest') + 
				"&fileName="+selectedProgramDatabase.fileName+"&fileOwner="+selectedProgramDatabase.fileOwner, 
				function(data, textStatus, jqxhr) {
					// debug.info("ProgramFileIntrospectionRequest done ... ");
		   			// alert("ProgramFileIntrospectionRequest done ... ");
					alert("data: " + data);
		   			insertQuerySelection(parentDivId, runQueryDivId, selectQueryDiv.id, selectedProgramDatabase.fileName, selectedProgramDatabase.fileOwner);
				});
	}
	
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function insertQuerySelection(parentDivId, runQueryDivId, selectQueryDivId, fileName, fileOwner) {
	// alert("Running insertQuerySelection ... ");
	var selectQueryDiv = document.getElementById(selectQueryDivId);
	selectQueryDiv.innerHTML = "";
	
	var chooseQueryStartTypeId = "selectQueryStartupType";
	var chooseQueryStartTypeContainerId = "chooseQueryStartTypeDiv";
	var queryLinesContainerId = "queryLinesContainer";
	var queryLinesCounterFieldId = "queryLinesCounter";
	var runQueryTargetiFrameId = "runQueryTargetiFrame";
	
	var html = "";
	html += "<form id='queryForm' method='POST' accept-charset='utf-8' ";
	html += "      action='"+ urlMappingFor('RunQueryRequest') + "&fileName="+fileName+"&fileOwner="+fileOwner + "' ";
	html += "      target='" + runQueryTargetiFrameId+ "'>";
	html += "     <div id='queryStartContainer' class='queryStartContainerTable'>";
	html += "          <div class='queryStartContainerTableRow'>";
	html += "               <div class='queryStartContainerTableCell'> ";
	html += "                    <h3>Your query: I'm looking for a </h3> ";
	html += "               </div>";
	html += "               <div class='queryStartContainerTableCell' id='"+ chooseQueryStartTypeContainerId +"'></div>";
	html += "          </div>";
	html += "     </div>";
    html += "     <input type='hidden' name='"+ queryLinesCounterFieldId +"' value='0' id='"+ queryLinesCounterFieldId +"'>";
    html += "     <div id='"+ queryLinesContainerId +"' class='"+queryLinesContainerId+"Table'></div>";
    html += "     <div class='searchOrPersonalizeTable'>";
    html += "          <div class='searchOrPersonalizeTableRow'>";
    html += "               <div class='searchOrPersonalizeTableCell'>";
	html += "                    <INPUT type='submit' value='Search' "+
									"onclick='return runQueryAfterSoftTests(\"" + parentDivId + "\", \"" + runQueryDivId +  
									"\", \"" + runQueryTargetiFrameId + "\", \"" + chooseQueryStartTypeId + 
									"\", \"" + queryLinesCounterFieldId+"\");'>";
	html += "               </div>";
	html += "               <div class='searchOrPersonalizeTableCell'>";
	html += "                    <INPUT type='submit' value='Personalize Program File' " +
									"onclick='return personalizeProgramFile(\"" + fileName + "\", \"" + fileOwner + "\", \"basic\");'>";
	html += "               </div>";
	html += "          </div>";
	html += "     </div>";
	html += "</form><br />";
	html += "<iframe id='"+runQueryTargetiFrameId+"' name='"+runQueryTargetiFrameId+"' style='display:none;'> </iframe>";
	// This does not work on google chrome: src='#' 
    	
	selectQueryDiv.innerHTML = html;
	
	// Callback run after runQueryTargetiFrame is loaded.
	$('#' + runQueryTargetiFrameId).load(function() {
		// document.getElementById('#' + submitiFrameId);
		var responseHtmlText = null;
		var iFrameWindow = getIframeWindow(this);
		if ((notNullNorundefined(iFrameWindow)) && (notNullNorundefined(iFrameWindow.document)) && (notNullNorundefined(iFrameWindow.document.body))) {
			responseHtmlText = iFrameWindow.document.body.innerHTML;
			// Do something with response text.
			if (notNullNorundefined(responseHtmlText)) {
				iFrameWindow.document.body.innerHTML="";
			}
			// Clear the content of the iframe.
			// this.contentDocument.location.href = '/images/loading.gif';
			// alert("responseText: " + responseHtmlText);
			// document.getElementById(uploadStatusDivId).style.visibility = 'visible';
			document.getElementById(runQueryDivId).innerHTML = responseHtmlText;

			// Update the files list.
			// insertFilesList (parentDivId);
			
			// Evaluate the JS code returned by the server.
			eval(responseHtmlText);
			
			// Show the answers retrieved to the user.
			showQueryAnswers(runQueryDivId);
			
			// Debug information
			// debugQueryAnswers(parentDivId);
		}
		  
	});
	
	insertChooseQueryStartupType(chooseQueryStartTypeId, chooseQueryStartTypeContainerId, queryLinesContainerId, queryLinesCounterFieldId);
	
	/*
	"Upload Program files <br />" + 
	  "<FORM ID='"+uploadFormId+"' ENCTYPE='multipart/form-data' method='POST' accept-charset='UTF-8' "+
	  "target='" + uploadFormTargetiFrameId+ "' action='" + urlMappingFor('FileUploadRequest') + "' >" +
	  		"<INPUT TYPE='file' NAME='programFileToUpload' size='50' "+
	  		"onchange='uploadActionOnChange(\""+uploadFormId+"\", \""+uploadStatusDivId+"\");'>" +
	  "</FORM>" +
	*/
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function insertChooseQueryStartupType(chooseQueryStartTypeId, chooseQueryStartTypeContainerId, queryLinesContainerId, queryLinesCounterFieldId) {
	var validTypesArray = new Array();
	var valid = false;
	for (var i=0; i<programIntrospection.length; i++) {
		valid = false;
		if (programIntrospection[i].predOtherInfo != '[]') {
			for (var j=0; j<programIntrospection[i].predOtherInfo.length; j++) {
				if (programIntrospection[i].predOtherInfo[j][0] == 'database') {
					valid = true;
				}
			}
		}
		
		if (valid) {
			var k=0;
			var found=false;
			while ((k < validTypesArray.length) && (! found)) {
				if (validTypesArray[k] == programIntrospection[i].predName)	found = true;
				else k++;
			}
			if (! found) {
				validTypesArray[validTypesArray.length] = programIntrospection[i].predName;
			}
		}
	}
	
	var html = "<select id='"+chooseQueryStartTypeId+"' name='"+chooseQueryStartTypeId+"' "+
				"onchange=\"selectQueryStartupTypeChanged(this, \'"+queryLinesContainerId+
				"\', \'"+queryLinesCounterFieldId+"\');\">";
	html += "<option name=\'----\' value=\'----\''>----</option>";
	for (var i=0; i<validTypesArray.length; i++) {
		html += "<option name=\'"+validTypesArray[i]+"\' value=\'"+validTypesArray[i]+"\''>"+validTypesArray[i]+"</option>";
	}
	html += "</select>";
	document.getElementById(chooseQueryStartTypeContainerId).innerHTML = html;
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function selectQueryStartupTypeChanged(comboBox, queryLinesContainerId, queryLinesCounterFieldId) {
	var startupType = comboBox.options[comboBox.selectedIndex].value;
	debug.info("startupType changed to " + startupType);
	
	resetQueryLinesCounterField(queryLinesCounterFieldId);
	var queryLinesContainerDiv = document.getElementById(queryLinesContainerId); 
	queryLinesContainerDiv.innerHTML="";
	
	if (startupType == "----") {
		queryLinesContainerDiv.innerHTML="You cannot be looking for that.";
	}
	else {
		var row = null;
		var cell = null;
		var cellContents = null;
		var queryLinesTableId = "queryLinesTable";
		var queryLinesAggregatorTableId = "queryLinesAggregatorTable";
	
		row = document.createElement('div');
		row.className = queryLinesContainerId + "TableRow";
		queryLinesContainerDiv.appendChild(row);
	
		cell = document.createElement('div');
		cell.className = queryLinesContainerId + "TableCell";
		row.appendChild(cell);
	
		cellContents = document.createElement('div');
		cellContents.className = queryLinesTableId;
		cellContents.id = queryLinesTableId;
		cell.appendChild(cellContents);
	
		cell = document.createElement('div');
		cell.className = queryLinesContainerId + "TableCell";
		row.appendChild(cell);
	
		cellContents = document.createElement('div');
		cellContents.className = queryLinesAggregatorTableId;
		cellContents.id = queryLinesAggregatorTableId;
		cell.appendChild(cellContents);
	
		insertQueryLine(queryLinesCounterFieldId, queryLinesTableId, queryLinesAggregatorTableId, startupType);
		// insertAggregatorTable(queryLinesTableId, queryLinesAggregatorTableId, comboBoxValue);
	}
}

function resetQueryLinesCounterField(queryLinesCounterFieldId) {
	document.getElementById(queryLinesCounterFieldId).value = 0;
}

function incrementQueryLinesCounterField(queryLinesCounterFieldId) {
	document.getElementById(queryLinesCounterFieldId).value ++;
	return getQueryLinesCounterField(queryLinesCounterFieldId);
}

function getQueryLinesCounterField(queryLinesCounterFieldId) {
	return document.getElementById(queryLinesCounterFieldId).value;
}


/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

// Constant 
var queryLinesCounterLimit = 100;

function insertQueryLine(queryLinesCounterFieldId, queryLinesTableId, queryLinesAggregatorTableId, startupType) {
	var queryLinesCounter = getQueryLinesCounterField(queryLinesCounterFieldId);
	if (queryLinesCounter == queryLinesCounterLimit) {
		alert("You cannot add more subqueries. Maximum number of allowed subqueries is: " + queryLinesCounterLimit);
	} else {
		var queryLineId = "queryLine[" + queryLinesCounter + "]";			

		var row = document.createElement('div');
		row.className = queryLinesTableId + "Row";
		row.id = queryLineId + ".row";
		document.getElementById(queryLinesTableId).appendChild(row);
		
		// Playing with styles ... best to use CSS.
		// document.getElementById(queryLineTableId).style.border = "none";
		// document.getElementById(queryLineTableId).style.border = "hidden";
		// document.getElementById(queryLineTableId).style.borderColor = "white";
		// document.getElementById(queryLineTableId).style.borderCollapse="collapse";
							
		insertChooseRule(row.id, queryLineId, queryLinesTableId, startupType);
		
		queryLinesCounter = incrementQueryLinesCounterField(queryLinesCounterFieldId);
		insertAggregatorTable(queryLinesCounterFieldId, queryLinesTableId, queryLinesAggregatorTableId, startupType);
	}
	
	// Do not allow navigator to call url.
	return false;
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function insertChooseRule(rowId, queryLineId, queryLinesTableId, startupType) {
	
	var row = document.getElementById(rowId);
	var cell = document.createElement('div');
	cell.id = queryLineId + ".predicate";
	cell.className = queryLinesTableId + "Cell";
	row.appendChild(cell);
	
	var queryLineSelectPredicateId = queryLineId + ".selectPredicate";
	var html = "<select name=\'"+queryLineSelectPredicateId+"\'"+
				"onchange=\"selectPredicateChanged(this, \'" + queryLineId + "\', \'"+rowId+ "\', \'"+startupType+"\', \'" + queryLinesTableId + "');\">";
	html += "<option name=\'----\' value=\'----\''>----</option>";
	var addOption=false;
	for (var i=0; i<programIntrospection.length; i++){
		addOption=false;
		for (var j=0; j<programIntrospection[i].predType.length; j++){
			addOption = addOption || 
				((startupType == programIntrospection[i].predType[j][0]) && (programIntrospection[i].predArity == "2"));
		}
		if (addOption) {
			html += "<option title=\'" + i + "\' value=\'" + programIntrospection[i].predName + "\'>"+
					programIntrospection[i].predName + "</option>";
		}
	}
	html += "</select>";
	
	cell.innerHTML = html;
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function selectPredicateChanged(comboBox, queryLineId, rowId, startupType, queryLinesTableId) {
	// var comboBox = document.getElementById('fuzzyRule[' + fuzzyRuleIndex + ']');
	var comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
	var comboBoxText = comboBox.options[comboBox.selectedIndex].text;
	var comboBoxName = comboBox.options[comboBox.selectedIndex].name;
	var comboBoxTitle = comboBox.options[comboBox.selectedIndex].title;
	debug.info("changeInChooseRule: comboBoxValue: " + comboBoxValue);
	debug.info("changeInChooseRule: comboBoxText: " + comboBoxText);
	debug.info("changeInChooseRule: comboBoxName: " + comboBoxName);
	debug.info("changeInChooseRule: comboBoxTitle: " + comboBoxTitle);
	
	var index = comboBoxTitle;
	
	var row = document.getElementById(rowId);
	var rowCells = row.childNodes;
	
	if ((rowCells != null) && (rowCells.length != 'undefined')) {	
		for (var i=(rowCells.length -1); i >= 0 ; i--) {
			if ((rowCells[i] != null) && (rowCells[i] != undefined) && (rowCells[i].id != (queryLineId + ".predicate"))) {
				row.removeChild(rowCells[i]);
			}
		}
	}
	
	var foundPredInfo = programIntrospection[index];
	debug.info("programIntrospection["+index+"]: " + programIntrospection[index].predName);

	if (foundPredInfo != null) {
		var typeIndex = 0;
		while ((typeIndex < foundPredInfo.predType.length) && 
				(foundPredInfo.predType[typeIndex][0] != startupType)) {
			typeIndex++;
		}
		if ((typeIndex < foundPredInfo.predType.length) && 
				(foundPredInfo.predType[typeIndex][0] == startupType)) {
			if (foundPredInfo.predType[typeIndex][foundPredInfo.predType[typeIndex].length -1] == 'rfuzzy_truth_value_type') {
				insertChooseQuantifier(queryLineId, rowId, 1, queryLinesTableId);
				insertChooseQuantifier(queryLineId, rowId, 0, queryLinesTableId);
			}
			else {
				insertRfuzzyComputeOperator(queryLineId, rowId, index, typeIndex, queryLinesTableId);
				insertRfuzzyComputeArgument(queryLineId, rowId, index, typeIndex, queryLinesTableId);
			}
		}
	}
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function insertChooseQuantifier(queryLineId, rowId, quantifierIndex, queryLinesTableId) {
	var row = document.getElementById(rowId);
	var firstCell = row.firstChild;
	
	var cell = document.createElement('div');
	cell.id = queryLineId + ".quantifier_" + quantifierIndex;
	cell.className = queryLinesTableId + "Cell";
	row.insertBefore(cell, firstCell);
	
	var quantifierId = queryLineId + ".selectQuantifier_" + quantifierIndex;

	var html = "<select name=\'" + quantifierId + "\'>";
	html += "<option name=\'----\' value=\'----\'>----</option>";
	for (var i=0; i<programIntrospection.length; i++){
		if (isQuantifierPredicate(i)) {
			if (((quantifierIndex == 0) && (programIntrospection[i].predName == "fnot")) ||
					((quantifierIndex == 1) && (programIntrospection[i].predName != "fnot"))) {
				html += "<option name=\'" + programIntrospection[i].predName + 
							"\' value=\'" + programIntrospection[i].predName + "\'>";
				if (programIntrospection[i].predName == "fnot") html += "not";
				else html += programIntrospection[i].predName;
				html += "</option>";
			}
		}
	}
	html += "</select>";
	cell.innerHTML = html;	
}

function isQuantifierPredicate(index) {
	var result = false;
	var i = 0;
	var predInfo = programIntrospection[index];
	
	// debug.info("quantifier ?? predName: " + predInfo.predName);
	if (predInfo.predArity == "2") {
		while (i<predInfo.predType.length && ! result) {
			// debug.info("quantifier ?? predType["+i+"]: " + predInfo.predType[i]);
			result = result || ((predInfo.predType[i].length == 2) &&
								(predInfo.predType[i][0] == "rfuzzy_predicate_type") &&
					  			(predInfo.predType[i][1] == "rfuzzy_truth_value_type"));
			i++;
		}
	}
	
	// if (! result) debug.info("not a quantifier:" + predInfo.predName);
	return result;
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

// insertQueryLine(queryLinesCounterFieldId, queryLinesTableId, queryLinesAggregatorTableId, startupType);
function insertAggregatorTable(queryLinesCounterFieldId, queryLinesTableId, queryLinesAggregatorTableId, startupType) {

	var row = null;
	var cell = null;
	var queryLinesAggregatorTable = document.getElementById(queryLinesAggregatorTableId);
	
	var queryLinesCounter = getQueryLinesCounterField(queryLinesCounterFieldId);
	// debug.info("queryLinesCounter: " + queryLinesCounter);
	
	if (queryLinesCounter == 1) {
		// debug.info("queryLinesCounter: 1 == " + queryLinesCounter);
		row = document.createElement('div');
		row.className = queryLinesAggregatorTableId + "Row";
		queryLinesAggregatorTable.appendChild(row);
	
		cell = document.createElement('div');
		cell.className = queryLinesAggregatorTableId + "Cell";
		row.appendChild(cell);
	
		cell.innerHTML= "<a href=\"#\" onClick='return insertQueryLine"+
						"(\""+queryLinesCounterFieldId+"\", \""+queryLinesTableId+
						"\", \""+queryLinesAggregatorTableId+"\", \""+startupType+"\");' >" +
						"<img src=\"images/add.png\" width=\"20\" alt=\"Add more conditions to the query\" "+
						"title=\"Add more conditions to the query\" /></a>";
	}
	
	if (queryLinesCounter == 2) {
		// debug.info("queryLinesCounter: 2 == " + queryLinesCounter);
		row = document.createElement('div');
		row.className = queryLinesAggregatorTableId + "Row";
		queryLinesAggregatorTable.appendChild(row);

		var queryLinesSelectAggregatorShowOptionsId='queryLinesSelectAggregatorShowOptions';
		var queryLinesSelectAggregatorHideOptionsId='queryLinesSelectAggregatorHideOptions';
		var chooseAgregatorInfoCellId='queryLines.chooseAgregatorInfoCell';
		var chooseAgregatorCellId = 'queryLines.chooseAgregatorCell';

		cell = document.createElement('div');
		cell.className = queryLinesAggregatorTableId + "Cell";
		cell.innerHTML = ""+
		"<a id='"+queryLinesSelectAggregatorShowOptionsId+"' href='' onclick='return aggregatorDetailsShow(\""+
				queryLinesSelectAggregatorShowOptionsId+"\", \""+queryLinesSelectAggregatorHideOptionsId+"\", \""+
				chooseAgregatorInfoCellId+"\", \""+chooseAgregatorCellId+"\");'>"+
				"show options</a>"+
		"<a id='"+queryLinesSelectAggregatorHideOptionsId+"' href='' onclick='return aggregatorDetailsHide(\""+
				queryLinesSelectAggregatorShowOptionsId+"\", \""+queryLinesSelectAggregatorHideOptionsId+"\", \""+
				chooseAgregatorInfoCellId+"\", \""+chooseAgregatorCellId+"\");'>"+
				"hide options</a>";
		row.appendChild(cell);
		document.getElementById(queryLinesSelectAggregatorHideOptionsId).style.display='none';
			
		row = document.createElement('div');
		row.className = queryLinesAggregatorTableId + "Row";
		queryLinesAggregatorTable.appendChild(row);
			
		cell = document.createElement('div');
		cell.className = queryLinesAggregatorTableId + "Cell";
		cell.id = chooseAgregatorInfoCellId;			
		cell.innerHTML = "The aggregator used to combine <br />the subqueries' truth values is:";
		cell.style.display='none';
		row.appendChild(cell);
			
		row = document.createElement('div');
		row.className = queryLinesAggregatorTableId + "Row";
		queryLinesAggregatorTable.appendChild(row);

		cell = document.createElement('div');
		cell.className = queryLinesAggregatorTableId + "Cell";
		cell.id = chooseAgregatorCellId;
		cell.style.display='none';
				
		var queryLinesSelectAggregatorId = "queryLines.selectAggregator"; // used below.
		var predInfo = null;
		var isAggregator = false;
		var html = "";
		html += "<select name=\'"+queryLinesSelectAggregatorId+"\'>";
		for (var i=0; i<programIntrospection.length; i++){
			predInfo = programIntrospection[i];
			isAggregator = false;
			for (var j=0; j<predInfo.predType.length; j++) {
				isAggregator = isAggregator || ((predInfo.predType[j].length == 3) &&
						(predInfo.predType[j][0] == 'rfuzzy_truth_value_type') &&
						(predInfo.predType[j][1] == 'rfuzzy_truth_value_type') &&
						(predInfo.predType[j][2] == 'rfuzzy_truth_value_type'));
			}
			if  (isAggregator) {
				html += "<option ";
				if (predInfo.predName == "min") html += "selected ";
				html += "name=\'" + predInfo.predName + 
						"\' value=\'" + predInfo.predName + "\'>"+predInfo.predName + "</option>";
			}
		}
		html += "</select>";
		cell.innerHTML = html;
		row.appendChild(cell);
	}
	
	// alert("stop");
	return false;
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

function insertRfuzzyComputeArgument(queryLineGeneralId, rowId, foundPredInfoIndex, typeIndex, queryLinesTableId) {
	foundPredInfo = programIntrospection[foundPredInfoIndex];
	
	var row = document.getElementById(rowId);
	var cell = document.createElement('div');
	cell.id = queryLineGeneralId + ".rfuzzyComputeArgument";
	cell.className = queryLinesTableId + "Cell";
	row.appendChild(cell);
	
	var rfuzzyComputeArgumentId = queryLineGeneralId + ".selectRfuzzyComputeValue";
	debug.info("foundPredType: "+foundPredInfo.predType[foundPredInfo.predType.length-1]);
	var html = "";
	var type = foundPredInfo.predType[typeIndex];
	
	if (type[type.length-1] == 'rfuzzy_enum_type') {
		html += "<select name=\'" + rfuzzyComputeArgumentId + "\'>";;
		html += "<option name=\'----\' value=\'----\'>----</option>";
		var values = foundPredInfo.predOtherInfo;
		var valuesLength = values.length;
		i = 0;
		while (i<valuesLength) {
			html+= "<option name=\'" + values[i] + "\' value=\'" + values[i] + "\'>" +
			values[i] + "</option>";
			i++;
		}
		html += "</select>";
	}
	else {
		html += "<input type='text' value='' name='"+rfuzzyComputeArgumentId+"'/>";
	}
	cell.innerHTML = html;
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function insertRfuzzyComputeOperator(queryLineGeneralId, rowId, foundPredInfoIndex, typeIndex, queryLinesTableId) {
	
	foundPredInfo = programIntrospection[foundPredInfoIndex];
	debug.info("foundPredInfo: "+foundPredInfo);
	debug.info("foundPredInfo.predName: "+foundPredInfo.predName);
	debug.info("foundPredInfo.predArity: "+foundPredInfo.predArity);
	debug.info("foundPredInfo.predType: "+foundPredInfo.predType);
	debug.info("foundPredInfo.predOtherInfo: "+foundPredInfo.predOtherInfo);
	var foundPredInfoLastType = foundPredInfo.predType[typeIndex][foundPredInfo.predType[typeIndex].length -1];
	debug.info("foundPredInfoLastType: "+foundPredInfoLastType);
	
	var index = 0;
	var operatorsPredInfo = null;
	while (index<programIntrospection.length && operatorsPredInfo == null){
		if (programIntrospection[index].predName == 'rfuzzy_compute_defined_operators') {
			operatorsPredInfo = programIntrospection[index];
		}
		else index++;
	}
	
	var row = document.getElementById(rowId);
	var cell = document.createElement('div');
	cell.id = queryLineGeneralId + ".rfuzzyComputeOperator";
	cell.className = queryLinesTableId + "Cell";
	row.appendChild(cell);
	
	var rfuzzyComputeOperatorId = queryLineGeneralId + ".selectRfuzzyComputeOperator";
	var html="<select name=\'";
	html += rfuzzyComputeOperatorId;
	html += "\'>";
	html += "<option name=\'----\' value=\'----\'>----</option>";		
	

	var operators = operatorsPredInfo.predOtherInfo;
	for (var i=0; i<operators.length; i++) {
		
		debug.info("operatorType: "+operators[i][1]);
		var case1 = ((foundPredInfoLastType == 'rfuzzy_enum_type') && 
				((operators[i][1] == 'rfuzzy_enum_type') || (operators[i][1] == 'rfuzzy_any_type')));
		var case2 = ((foundPredInfoLastType != 'rfuzzy_enum_type') &&
				(operators[i][1] != 'rfuzzy_enum_type'));
		if (case1 || case2) {
			html+= "<option name=\'" + operators[i][0] + "\' value=\'" + operators[i][0] + "\'>" +
					operators[i][0] + "</option>";
		}
	}
	html += "</select>";
	cell.innerHTML = html;	
	
}

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

/* This function makes a soft test of the query. The one in charge of running the query is below. */
function runQueryAfterSoftTests(parentDivId, runQueryDivId, runQueryTargetiFrameId, chooseQueryStartTypeId, queryLinesCounterFieldId) {
	var comboBox = document.getElementById(chooseQueryStartTypeId);
	var comboBoxValue = null;
	
	if ((comboBox != null) && (comboBox.length == 1)) comboBox = comboBox[0];
	if (comboBox != null) {
		if (comboBox.selectedIndex != null) {
			comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
			if ((comboBoxValue != null) && (comboBoxValue != '----')) {
				debug.info("comboBoxValue: " + comboBoxValue);
			}
			else {
				alert("Please, say what you are looking for.");
				return false; 				
			}
		}
		else {
			debug.info("comboBox.selectedIndex is null ");
			return false;
		}
	}
	else {
		debug.info("comboBox is null.");
		return false;
	}
	
	// alert("Stop 2");
	var queryLinesCounter = getQueryLinesCounterField(queryLinesCounterFieldId);
	for (var i=0; i < queryLinesCounter; i++) {
		// operator 
		comboBox = document.getElementsByName("queryLine["+i+"].selectRfuzzyComputeOperator");
		if ((comboBox != null) && (comboBox.length == 1)) comboBox = comboBox[0];
		if (comboBox != null) {
			if (comboBox.selectedIndex != null) {
				comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
				if (comboBoxValue == '----') {
					alert("Please fill the operator in subquery number " + (i+1));
					return false; 
				}
				else debug.info("comboBoxValue: " + comboBoxValue);
			}
			else debug.info("comboBox.selectedIndex is null ");
		}
		else debug.info("comboBox is null.");
		
		// value 
		var value = document.getElementsByName("queryLine["+i+"].selectRfuzzyComputeValue");
		if ((value != null) && (value.length == 1)) value = value[0];
		if (value != null) {
			if (value.value == '') {
				alert("Please fill the value in subquery number " + (i+1));
				return false; 
			}
			else debug.info("value: " + value);
		}
		else debug.info("value is null.");
	}
	
	var runQueryDiv = document.getElementById(runQueryDivId);
	runQueryDiv.innerHTML = loadingImageHtml();
	runQueryDiv.style.display='block'; 
	// runQueryDiv.style.display='inline';
	
	// Used to debug
	//alert("Stop");
	
	// Tell the navigator to follow the link !!!
	return true;
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
	
	parentDiv.innerHTML = loadingImageHtml();
	
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
	filesListDiv.innerHTML = loadingImageHtml();
	
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
		   					cell.innerHTML = "Personalizations";
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
	cell.innerHTML = "Personalizations";
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
	document.getElementById(uploadStatusDivId).innerHTML = loadingImageHtml();

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
	var realValue = queryAnswers[index][queryAnswers[index].length -1];
	if ((realValue == null) || (realValue == undefined)) return false;
	if (realValue == "Truth Value") return true;
	return (parseFloat(realValue) > value);
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
		found = (tableName == infosForQueryAnswers[i].tableName);
		if (!found) i++;
	}
	
	if (! found) {
		infosForQueryAnswers[infosForQueryAnswers.length] = new infoForQueryAnswers(tableName, queryAnswerIndex);
	}
	else {
		infosForQueryAnswers[i].queryAnswersIndexes[infosForQueryAnswers[i].queryAnswersIndexes.length] = queryAnswerIndex;
	}
}


var queryAnswersOver70 = null;
var queryAnswersOver50 = null;
var queryAnswersOver0 = null;

function showQueryAnswers(runQueryDivId) {
	
	// Initialize.
	infosForQueryAnswers = null;
	
	if (queryAnswers.length > 1) queryAnswers.sort(arraySortFunction);

	var best10answersName = "The 10 answers that best satisfy the query";
	var answersOver70Name = "The query is very satisfied";
	var answersOver50Name = "The query is fairly satisfied";
	var answersOver0Name  = "The query is satisfied";
	var allAnswers        = "All answers";
	
	// Initialize to keep the final order.
	insertInfoForQueryAnswers(best10answersName, null);
	insertInfoForQueryAnswers(answersOver70Name, null);
	insertInfoForQueryAnswers(answersOver50Name, null);
	insertInfoForQueryAnswers(answersOver0Name, null);
	insertInfoForQueryAnswers(allAnswers, null);
	
	if (queryAnswers.length > 0) {
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
		if (infosForQueryAnswers[i].queryAnswersIndexes.length > 0) {
			noAnswers = false;
		}
	}
	
	var runQueryDiv = document.getElementById(runQueryDivId);
	runQueryDiv.innerHTML = "";
	
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
		var tabDiv = null;
		var tabContentDiv = null;
		var row = null;
		var cell = null;
		
		html = "";
		j = 1;
		for (i=0; i<infosForQueryAnswers.length; i++) {
			if (infosForQueryAnswers[i].queryAnswersIndexes.length > 0) {
				html += "<li><a href='#tabs-"+j+"'>"+infosForQueryAnswers[i].tableName+"</a></li>";
				j++;
			}
		}
		tabsDivList.innerHTML = html;
		
		html = "";
		j = 1;
		for (i=0; i<infosForQueryAnswers.length; i++) {
			if (infosForQueryAnswers[i].queryAnswersIndexes.length > 0) {
				tabDiv = document.createElement('div');
				tabDiv.id = "tabs-" + j;
				j++;
				tabsDiv.appendChild(tabDiv);
				
				tabContentDiv = document.createElement('div');
				tabContentDiv.className = "queryAnswersTable";
				tabDiv.appendChild(tabContentDiv);
				
				// Now insert each answer in a row, inside the table
				for (var k=0; k<infosForQueryAnswers[i].queryAnswersIndexes.length; k++) {
					row = document.createElement('div');
					row.className = "queryAnswersTableRow";
					tabContentDiv.appendChild(row);
					
					var answer = queryAnswers[infosForQueryAnswers[i].queryAnswersIndexes[k]];
					for (var l=1; l<answer.length; l++) {
						cell = document.createElement('div');
						cell.className = "queryAnswersTableCell";
						row.appendChild(cell);
						if (l+1 < answer.length) {
							cell.innerHTML = answer[l];
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
	this.predOwner = predOwner;
	this.functionPoints = functionPoints;
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

			// Show the personalization dialog to the user.
			showBasicPersonalizeProgramFileDialog(fileName, fileOwner, mode);
			
		});
	
	//prevent the browser to follow the link
	return false;
}

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

function fuzzificationFunctionNameInColloquial(currentName) {
	var result = null;
	if ((currentName != null) && (currentName != "") && (currentName != "----")) {
	
		var i = currentName.indexOf("(");
		var j = currentName.indexOf(")");
		
		if ((i != -1) && (j != -1)){
			result = currentName.substring(i+"(".length, j) + " is " + currentName.substring(0, i);
		}
	}
	return result;
}

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

function showBasicPersonalizeProgramFileDialog(fileName, fileOwner, mode) {
	var personalizationDiv = document.createElement('div');
	
	var personalizationDivSubTable = document.createElement('div');
	personalizationDivSubTable.className = "personalizationDivSubTable";
	personalizationDiv.appendChild(personalizationDivSubTable);
	
	var row = null;
	var cell = null;
	var subTable = null;
	var subRow = null;
	var subCell = null;
	
	row = document.createElement('div');
	row.className = "personalizationDivSubTableRow";
	personalizationDivSubTable.appendChild(row);
	
	cell = document.createElement('div');
	cell.className = "personalizationDivSubTableCellType1";
	row.appendChild(cell);
	
	subTable = document.createElement('div');
	subTable.className = "personalizationDivSubTable";
	cell.appendChild(subTable);
	
	subRow = document.createElement('div');
	subRow.className = "personalizationDivSubTableRow";
	subTable.appendChild(subRow);
	
	subCell = document.createElement('div');
	subCell.className = "personalizationDivSubTableCellType2";	
	subCell.innerHTML = "I want to personalize how it is determined that a ";
	subRow.appendChild(subCell);
	
	subCell = document.createElement('div');
	subCell.className = "personalizationDivSubTableCellType2";
	subCell.innerHTML = "Test";
	subRow.appendChild(subCell);
	
	var PersonalizationFunctionUnderModificationDivId = "PersonalizationDivCell";
	
	// Fill in the div.
	var personalizationSelectComboBoxId = "personalizationSelectComboBox";
	var html = "";
	html += "<select name='" + personalizationSelectComboBoxId + "' id='"+personalizationSelectComboBoxId+"' " +
			"onchange='personalizationFunctionChanged(this, \""+PersonalizationFunctionUnderModificationDivId+"\");'>";
	html += "<option name=\'----\' value=\'----\'>----</option>";
	
	for (var i=0; i < fuzzificationsFunctions.length; i++) {
		html+= "<option name=\'" + fuzzificationsFunctions[i].predDefined + "\' "+
				"value=\'" + fuzzificationsFunctions[i].predDefined + "\'>" +
				fuzzificationFunctionNameInColloquial(fuzzificationsFunctions[i].predDefined) + "</option>";
	}
	html += "</select>";
	subCell.innerHTML = html;
	
	row = document.createElement('div');
	row.className = "personalizationDivSubTableRow";
	personalizationDivSubTable.appendChild(row);
	
	cell = document.createElement('div');
	cell.className = "personalizationDivSubTableCell";
	cell.id = PersonalizationFunctionUnderModificationDivId;
	personalizationDivSubTable.appendChild(cell);
	cell.innerHTML = "Select the fuzzification you want to personalize.";
	
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

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

function personalizationFunctionChanged(comboBox, PersonalizationFunctionUnderModificationDivId) {
	var comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
/*	var comboBoxText = comboBox.options[comboBox.selectedIndex].text;
	var comboBoxName = comboBox.options[comboBox.selectedIndex].name;
	var comboBoxTitle = comboBox.options[comboBox.selectedIndex].title;
	debug.info("changeInChooseRule: comboBoxValue: " + comboBoxValue);
	debug.info("changeInChooseRule: comboBoxText: " + comboBoxText);
	debug.info("changeInChooseRule: comboBoxName: " + comboBoxName);
	debug.info("changeInChooseRule: comboBoxTitle: " + comboBoxTitle);
*/
	var found = false;
	var i = 0;
	while ((i<fuzzificationsFunctions.length) && (! found)) {
		if (comboBoxValue == fuzzificationsFunctions[i].predDefined) {
			found = true;
		}
		else i++;
	}
	
	showPersonalizationForm(i, PersonalizationFunctionUnderModificationDivId);
}

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

function showPersonalizationForm(i, PersonalizationFunctionUnderModificationDivId) {
	var PersonalizationFunctionUnderModificationDiv = document.getElementById(PersonalizationFunctionUnderModificationDivId);
	PersonalizationFunctionUnderModificationDiv.innerHTML = "";
	
	var table = document.createElement('div');
	table.className = "personalizationDivSubTableRow";
	PersonalizationFunctionUnderModificationDiv.appendChild(table);
	
}

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

// EOF

/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/


