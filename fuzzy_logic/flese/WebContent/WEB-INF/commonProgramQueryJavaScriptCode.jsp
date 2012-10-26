<script type="text/javascript">

	var queryLinesCounter = 0;
	var queryLinesCounterLimit = 100;
	var variablesCounter = 0;

	function convertTypeNameToVarName(inputString) {
		var firstCharacter = inputString[0];
		firstCharacter = firstCharacter.toUpperCase(); 
		var lastCharacters = inputString.substring(0, (inputString.length -1));
		return firstCharacter + lastCharacters; 
	}
	
	function addRfuzzyComputeOperator(queryLineGeneralId, rowId, foundPredInfoIndex) {
		
		foundPredInfo = programIntrospectionArray[foundPredInfoIndex];
		debug.info("foundPredInfo: "+foundPredInfo);
		debug.info("foundPredInfo.predName: "+foundPredInfo.predName);
		debug.info("foundPredInfo.predArity: "+foundPredInfo.predArity);
		debug.info("foundPredInfo.predType: "+foundPredInfo.predType);
		debug.info("foundPredInfo.predOtherInfo: "+foundPredInfo.predOtherInfo);
		var foundPredInfoLastType = foundPredInfo.predType[foundPredInfo.predType.length -1];
		debug.info("foundPredInfoLastType: "+foundPredInfoLastType);
		
		var index = 0;
		var operatorsPredInfo = null;
		while (index<programIntrospectionArray.length && operatorsPredInfo == null){
			if (programIntrospectionArray[index].predName == 'rfuzzy_compute_defined_operators') {
				operatorsPredInfo = programIntrospectionArray[index];
			}
			else index++;
		}
		
		var row = document.getElementById(rowId);
		var cell = row.insertCell(-1);
		cell.id = queryLineGeneralId + ".rfuzzyComputeOperator";
		
		var rfuzzyComputeOperatorId = queryLineGeneralId + ".selectRfuzzyComputeOperator";
		html="<select name=\'" + rfuzzyComputeOperatorId + "\'>";
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
	function addRfuzzyComputeArgument(queryLineGeneralId, rowId, foundPredInfoIndex) {
		foundPredInfo = programIntrospectionArray[foundPredInfoIndex];
		
		var row = document.getElementById(rowId);
		var cell = row.insertCell(-1);
		cell.id = queryLineGeneralId + ".rfuzzyComputeArgument";
		
		var rfuzzyComputeArgumentId = queryLineGeneralId + ".selectRfuzzyComputeOperator";
		debug.info("foundPredType: "+foundPredInfo.predType[foundPredInfo.predType.length-1]);
		var html = "";
		if (foundPredInfo.predType[foundPredInfo.predType.length-1] == 'rfuzzy_enum_type') {
			html += "<select name=\'" + rfuzzyComputeArgumentId + "\'>";;
			html += "<option name=\'----\' value=\'----\'>----</option>";
			var values = foundPredInfo.predOtherInfo;
			var valuesLength = values.length;
			for (var i=0; i<valuesLength; i++) {
				html+= "<option name=\'" + values[i] + "\' value=\'" + values[i] + "\'>" +
				values[i] + "</option>";
			}
			html += "</select>";
		}
		else {
			html += "<input type='text' value='' name='"+rfuzzyComputeArgumentId+"'/>";
		}
		cell.innerHTML = html;
	}

	
	function isQuantifierPredicate(programIntrospectionElement) {
		
		if ((programIntrospectionElement.predArity == 2) &&
			(programIntrospectionElement.predType != "-variable-")){
			var types = programIntrospectionElement.predType;
				return ((types[0] == "rfuzzy_predicate_type") &&
						(types[1] == "rfuzzy_truth_value_type"));
		}
		else return false;
	}
	
	function addQuantifier(queryLineGeneralId, rowId, quantifierIndex) {
		var row = document.getElementById(rowId);
		var cell = row.insertCell(0);
		cell.id = queryLineGeneralId + ".quantifier_" + quantifierIndex;
		var quantifierId = queryLineGeneralId + ".selectQuantifier_" + quantifierIndex;

		var html = "<select name=\'" + quantifierId + "\'>";
		html += "<option name=\'----\' value=\'----\'>----</option>";
		for (var i=0; i<programIntrospectionArray.length; i++){
			if (isQuantifierPredicate(programIntrospectionArray[i])) {
				if (((quantifierIndex == 0) && (programIntrospectionArray[i].predName == "fnot")) ||
						((quantifierIndex == 1) && (programIntrospectionArray[i].predName != "fnot"))) {
					html += "<option name=\'" + programIntrospectionArray[i].predName + 
								"\' value=\'" + programIntrospectionArray[i].predName + "\'>";
					if (programIntrospectionArray[i].predName == "fnot") html += "not";
					else html += programIntrospectionArray[i].predName;
					html += "</option>";
				}
			}
		}
		html += "</select>";
		cell.innerHTML = html;	
	}
	
	function changeInChooseRule(comboBox, queryLineGeneralId, rowId, startupType) {
		// var comboBox = document.getElementById('fuzzyRule[' + fuzzyRuleIndex + ']');
		var comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
		debug.info("changeInChooseRule: comboBoxValue: " + comboBoxValue);
		
		var foundPredInfo = null;
		var index = 0;
		debug.info("changeInChooseRule: searching ... ");
		while ((foundPredInfo == null) && (index < programIntrospectionArray.length)) {
			debug.info("programIntrospectionArray["+index+"]: " + programIntrospectionArray[index].predName);
			if (comboBoxValue == programIntrospectionArray[index].predName) {
				foundPredInfo = programIntrospectionArray[index];
				debug.info("changeInChooseRule: found");
			}
			else index++;
		}
		
		var row = document.getElementById(rowId);
		var cell = null;
		debug.info("row.cells.lenght = " + row.cells.length);
		if ((row.cells.length != 1) && (row.cells.length != 'undefined')) {	
			for (var i=(row.cells.length -1); i>=0; i--) {
				cell = row.cells[i];
				debug.info("row.cells["+i+"].id = " + row.cells[i].id);
				if (cell.id != (queryLineGeneralId + ".predicate")) {
					row.deleteCell(i);
					debug.info("row.cell removed");
				}
			}
		}
		
		if (foundPredInfo != null) {
			var i = 0;
			var predType = null;
			while ((i < foundPredInfo.predType.length) && (predType == null)) {
				if (foundPredInfo.predType[i][0] == startupType) predType = foundPredInfo.predType[i];
				else i++;
			}
			if (predType[predType.length -1] == 'rfuzzy_truth_value_type') {
				addQuantifier(queryLineGeneralId, rowId, 1);
				addQuantifier(queryLineGeneralId, rowId, 0);
			}
			else {
				addRfuzzyComputeOperator(queryLineGeneralId, rowId, index);
				addRfuzzyComputeArgument(queryLineGeneralId, rowId, index);
			}
		}
	}
	
	function startupTypeIsValid(startupType, predType, predArity) {
		return ((predType[0] == startupType) && (predArity == "2"));
	}
	
	function addChooseRule(rowId, queryLineGeneralId, startupType) {
		
		var row = document.getElementById(rowId);
		var cell = row.insertCell(0);
		cell.id = queryLineGeneralId + ".predicate";
		
		var queryLineSelectPredicateId = queryLineGeneralId + ".selectPredicate";
		var html = "<select name=\'"+queryLineSelectPredicateId+"\'"+
					"onchange=\"changeInChooseRule(this, \'" + queryLineGeneralId + "\', \'"+rowId+ "\', \'"+startupType+"\');\">";
		html += "<option name=\'----\' value=\'----\''>----</option>";
		var addOption=false;
		for (var i=0; i<programIntrospectionArray.length; i++){
			addOption=false;
			for (var j=0; j<programIntrospectionArray[i].predType.length; j++){
				addOption = addOption || startupTypeIsValid(startupType, programIntrospectionArray[i].predType[j], programIntrospectionArray[i].predArity);
			}
			if (addOption) {
				html += "<option name=\'" + programIntrospectionArray[i].predName + 
						"\' value=\'" + programIntrospectionArray[i].predName + "\'>"+
						programIntrospectionArray[i].predName + "</option>";
			}
		}
		html += "</select>";
		
		cell.innerHTML = html;
	}
	
	function addQueryLine(queryLinesCellId, startupType) {
		if (queryLinesCounter == queryLinesCounterLimit) {
			alert("You have reached the limit of adding " + queryLinesCounter + " subqueries.");
		} else {
			var queryLineGeneralId = "queryLine[" + queryLinesCounter + "]";			
			var queryLineTableId = queryLineGeneralId + ".table";
			var queryLineTable = document.createElement('table');
			queryLineTable.id = queryLineTableId;
			queryLineTable.className = "queryLineTable";
			document.getElementById(queryLinesCellId).appendChild(queryLineTable);
			
			// Playing with styles ... best to use CSS.
			// document.getElementById(queryLineTableId).style.border = "none";
			// document.getElementById(queryLineTableId).style.border = "hidden";
			// document.getElementById(queryLineTableId).style.borderColor = "white";
			// document.getElementById(queryLineTableId).style.borderCollapse="collapse";
			
			rowId = queryLineGeneralId + ".row";
			var row = queryLineTable.insertRow(0);
			row.id = rowId;
						
			addChooseRule(rowId, queryLineGeneralId, startupType);
			
			queryLinesCounter++;
			changeInFormTheQueryLinesCounter(queryLinesCounter);
		}
	}
	
	function addQueryLineAndAggregatorChoose(queryLinesCellId, chooseAgregatorCellId, comboBoxValue) {
		addQueryLine(queryLinesCellId, comboBoxValue);
		queryLinesSelectAggregatorId = "queryLines.selectAggregator";
		var chooseAgregatorCell = document.getElementById(chooseAgregatorCellId);
		if ((chooseAgregatorCell.innerHTML == null) || (chooseAgregatorCell.innerHTML == "")) {
			debug.info("null nor empty chooseAgregatorCell.innerHTML");
			var predInfo = null;
			var html = "";
			html += "Truth values combined by: <br />";
			html += "<select name=\'"+queryLinesSelectAggregatorId+"\'>";
			for (var i=0; i<programIntrospectionArray.length; i++){
				predInfo = programIntrospectionArray[i];
				if ((predInfo.predType.length == 3) &&
					(predInfo.predType[0] == 'rfuzzy_truth_value_type') &&
					(predInfo.predType[1] == 'rfuzzy_truth_value_type') &&
					(predInfo.predType[2] == 'rfuzzy_truth_value_type')) {
					html += "<option name=\'" + predInfo.predName + 
							"\' value=\'" + predInfo.predName + "\'>"+predInfo.predName + "</option>";
				}
			}
			html += "</select>";
			chooseAgregatorCell.innerHTML = html;
		}
		else debug.info("not null nor empty chooseAgregatorCell.innerHTML");
		return false;
	}
	
	function addMoreQueryLines(queryLinesCellId, aggregatorCellId, comboBoxValue) {

		var aggregatorCell = document.getElementById(aggregatorCellId);
		var aggregatorTable = document.createElement('table');
		aggregatorTable.id = "queryLines.aggregatorTable";
		aggregatorTable.className = "queryLineAggregatorTable";
		aggregatorCell.appendChild(aggregatorTable);
		
		var row1 = aggregatorTable.insertRow(-1);
		var row2 = aggregatorTable.insertRow(-1);
		var addMoreQueryLinesButtonCell = row1.insertCell(-1);
		var chooseAgregatorCell = row2.insertCell(-1);
		chooseAgregatorCell.id = "queryLines.chooseAgregatorCell";
		
		var html = "<a href=\"\" onClick='return addQueryLineAndAggregatorChoose"+
					"(\""+queryLinesCellId+"\", \""+chooseAgregatorCell.id+"\", \""+comboBoxValue+"\");' >" +
					"<img src=\"add.png\" width=\"20\" alt=\"Add more conditions to the query\" /></a>";
		addMoreQueryLinesButtonCell.innerHTML = html;
		return false;
	}
	
	function changeInFormTheQueryLinesCounter(newValue) {
		var formCounter = document.getElementById('queryLinesCounter');
		formCounter.value = newValue;
	}
	
	
	function startupChange(comboBox, queryLinesDivId, queryTypeDivId) {
		var comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
		debug.info("comboBoxValue: " + comboBoxValue);
		queryLinesCounter=0;
		changeInFormTheQueryLinesCounter(queryLinesCounter);
		
		document.getElementById(queryLinesDivId).innerHTML="";
		
		var queryLinesTableId = "queryLines.table";
		var queryLinesTable = document.createElement('table');
		queryLinesTable.id = queryLinesTableId;
		document.getElementById(queryLinesDivId).appendChild(queryLinesTable);
		
		row = queryLinesTable.insertRow(0);
		row.id = "queryLines.row";
		var cell1 = row.insertCell(-1);
		cell1.id = "queryLines.cell1";
		var cell2 = row.insertCell(-1);
		cell2.id = "queryLines.cell2";
		
		addQueryLine(cell1.id, comboBoxValue);
		addMoreQueryLines(cell1.id, cell2.id, comboBoxValue);
	}
	
	function fillQueryStartupValues(queryStartId) {
		var validTypesArray = new Array();
		var validTypesArrayCounter = 0;
		var valid = false;
		for (var i=0; i<programIntrospectionArray.length; i++) {
			valid = false;
			if (programIntrospectionArray[i].predOtherInfo != '[]') {
				for (var j=0; j<programIntrospectionArray[i].predOtherInfo.length; j++) {
					if (programIntrospectionArray[i].predOtherInfo[j] == 'database') {
						valid = true;
					}
				}
			}
			
			if (valid) {
				var k=0;
				var found=false;
				while ((k < validTypesArrayCounter) && (! found)) {
					if (validTypesArray[k] == programIntrospectionArray[i].predName)	found = true;
					else k++;
				}
				if (! found) {
					validTypesArray[validTypesArrayCounter] = programIntrospectionArray[i].predName;
					validTypesArrayCounter++;
				}
			}
		}
		
		var html = "<select name=\'startupType' onchange=\"startupChange(this, \'queryLinesDiv\', \'simpleOrAdvancedQuery\');\">";
		html += "<option name=\'----\' value=\'----\''>----</option>";
		for (var i=0; i<validTypesArrayCounter; i++) {
			html += "<option name=\'"+validTypesArray[i]+"\' value=\'"+validTypesArray[i]+"\''>"+validTypesArray[i]+"</option>";
		}
		html += "</select>";
		document.getElementById(queryStartId).innerHTML = html;
	}
	
</script>
