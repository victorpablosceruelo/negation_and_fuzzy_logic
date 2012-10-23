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
	
	function isQuantifierPredicate(programIntrospectionElement) {
		
		if ((programIntrospectionElement.predArity == 2) &&
			(programIntrospectionElement.predType != "-variable-")){
			var types = programIntrospectionElement.predType;
				return ((types[0] == "rfuzzy_predicate_type") &&
						(types[1] == "rfuzzy_truth_value_type"));
		}
		else return false;
	}
	
	function addQuantifier(queryLineGeneralId, chooseRuleDivId, quantifierIndex) {
		var queryLineQuantifierGeneralId = queryLineGeneralId + ".quantifier_" + quantifierIndex;
		var quantifierDiv = document.createElement('div');
		quantifierDiv.id= queryLineQuantifierGeneralId + ".divContainer";

		var html = "<select name=\'" + queryLineQuantifierGeneralId + "\'>";
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
		quantifierDiv.innerHTML = html;
		chooseRuleDiv = document.getElementById(chooseRuleDivId);
		document.getElementById(queryLineGeneralId + ".divContainer").insertBefore(quantifierDiv, chooseRuleDiv);
		
	}
	
	function changeInChooseRule(comboBox, queryLineGeneralId, chooseRuleDivId) {
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
		
		if (foundPredInfo != null) {
			if (foundPredInfo.predType[foundPredInfo.predType.length -1] == 'rfuzzy_truth_value_type') {
				addQuantifier(queryLineGeneralId, chooseRuleDivId, 0)
				addQuantifier(queryLineGeneralId, chooseRuleDivId, 1)
			}
			else {
				addRfuzzyComputeArguments()
			}
		}
	}
	
	function startupTypeIsValid(startupType, predType) {
		return (predType[0] == startupType);
	}
	
	function addChooseRule(queryLineDivContainerId, queryLineGeneralId, startupType) {
		var queryLinePredicateGeneralId = queryLineGeneralId + ".rule";
		var chooseRuleDiv = document.createElement('div');
		chooseRuleDiv.id= queryLinePredicateGeneralId + ".divContainer";
		
		var html = "<select name=\'"+queryLinePredicateGeneralId + "\'"+
					"onchange=\"changeInChooseRule(this, \'" + queryLineGeneralId + "\', \'"+chooseRuleDiv.id+"\');\">";
		html += "<option name=\'----\' value=\'----\''>----</option>";
		for (var i=0; i<programIntrospectionArray.length; i++){
			if (startupTypeIsValid(startupType, programIntrospectionArray[i].predType)) {
				html += "<option name=\'" + programIntrospectionArray[i].predName + 
							"\' value=\'" + programIntrospectionArray[i].predName + "\'>"+
							programIntrospectionArray[i].predName + "</option>";
			}
		}
		html += "</select>";
		
		chooseRuleDiv.innerHTML = html;
		document.getElementById(queryLineDivContainerId).appendChild(chooseRuleDiv);
	}
	
	function addQueryLine(queryLinesDivId, startupType) {
		if (queryLinesCounter == queryLinesCounterLimit) {
			alert("You have reached the limit of adding " + queryLinesCounter + " subqueries.");
		} else {
			var queryLineGeneralId = "queryLine[" + queryLinesCounter + "]";
			var queryLineDivContainerId = queryLineGeneralId+".divContainer"; 
			var queryLineDivContainer = document.createElement('div');
			queryLineDivContainer.id = queryLineDivContainerId;
			document.getElementById(queryLinesDivId).appendChild(queryLineDivContainer);
			
			var queryLineTableId = queryLineGeneralId + ".table";
			var queryLineTable = document.createElement('table');
			queryLineTable.id = queryLineTableId;
			document.getElementById(queryLineDivContainerId).appendChild(queryLineTable);
			
			rowId = queryLineGeneralId + ".row";
			var row = queryLineTable.insertRow(0);
			row.id = rowId;
						
			addChooseRule(rowId, queryLineGeneralId, startupType);
			
			queryLinesCounter++;
		}
	}
	
	function startupChange(comboBox, queryLinesDivId, queryTypeDivId) {
		var comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
		debug.info("comboBoxValue: " + comboBoxValue);
		queryLinesCounter=0;
		document.getElementById(queryLinesDivId).innerHTML="";
		
		addQueryLine(queryLinesDivId, comboBoxValue);
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
		
		var html = "<select name=\'startupType' onchange=\"startupChange(this, \'queryLines\', \'simpleOrAdvancedQuery\');\">";
		html += "<option name=\'----\' value=\'----\''>----</option>";
		for (var i=0; i<validTypesArrayCounter; i++) {
			html += "<option name=\'"+validTypesArray[i]+"\' value=\'"+validTypesArray[i]+"\''>"+validTypesArray[i]+"</option>";
		}
		html += "</select>";
		document.getElementById(queryStartId).innerHTML = html;
	}
	
</script>
