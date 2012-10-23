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
	
	function chooseQuantifierCode(fuzzyRuleIndex, fuzzyRuleQuantifierIndex) {
		var html = "<select name=\'fuzzyRuleQuantifier[" + fuzzyRuleIndex + "][" + fuzzyRuleQuantifierIndex + "]\'>";
		html += "<option name=\'----\' value=\'----\'>----</option>";
		for (var i=0; i<programIntrospectionArray.length; i++){
			if (isQuantifierPredicate(programIntrospectionArray[i])) {
				if (((fuzzyRuleQuantifierIndex == 0) && (programIntrospectionArray[i].predName == "fnot")) ||
						((fuzzyRuleQuantifierIndex == 1) && (programIntrospectionArray[i].predName != "fnot"))) {
					html += "<option name=\'" + programIntrospectionArray[i].predName + 
								"\' value=\'" + programIntrospectionArray[i].predName + "\'>";
					if (programIntrospectionArray[i].predName == "fnot") html += "not";
					else html += programIntrospectionArray[i].predName;
					html += "</option>";
				}
			}
			
		}
		html += "</select>";
		return html;
	}
	
	function fuzzyRuleArgsCode(fuzzyRuleIndex) {
		return "<td id=\'fuzzyRuleArgs[" + fuzzyRuleIndex + "]\' class='hidden'></td>";
	}
	
	function addVariablesArguments(predInfo, queryLineGeneralId, chooseRuleDivId) {
		var variablesIndex = variablesCounter;
		variablesCounter += foundPredInfo.predArity;
		
		convertTypeNameToVarName
		var variableDiv;
		var variableName;
		var variableGeneralId;
		var index = 0;
		var chooseRuleDiv=getElementById(chooseRuleDivId);

		while ((variablesIndex + index) < variablesCounter) {
			variableDiv = document.createElement('div');
			variableGeneralId = queryLineGeneralId + ".variable["+variablesIndex+"]";
			variableDiv.id= variableGeneralId + ".div";
			
			if (predInfo.predType == '-variable-') {
				variableName = '-variable-';
			}
			else {
				variableName = predInfo.predType[index];
			}

			variableDiv.innerHTML = addVariableArgument(variableGeneralId, variablesIndex, index, variableName);
			
			chooseRuleDiv.appendChild(variableDiv);
			variablesIndex += 1;
		}
	}
	
	function addVariableArgument(variableGeneralId, variablesIndex, index, variableName) {
		debug.info("addVariableArgument: ");
		var constantDivId = variableGeneralId + ".constant";
		var html = "";
		html += "<select name=\'"+variableGeneralId+".variable\'" + 
				"onchange=\"variableChange(this, " + constantDivId +");\">";
		if (variableName != "-variable-") {
			html += "<option name=\'"+variableName+ "\' value=\'"+variableName+"\'>"+ variableName + "</option>";
		}
		for (var k=variablesIndex; k<variablesCounter; k++){
			html += "<option name=\'var_" + k + "\' value=\'var_" + k + "\'>variable_"+ k + "</option>";
		}
		for (var k=0; k<variablesIndex; k++){
			html += "<option name=\'var_" + k + "\' value=\'var_" + k + "\'>variable_"+ k + "</option>";
		}
		html += "<option name=\'constant' value=\'constant\'>constant</option>";
		html += "</select>";
		html += "<div id=\'" + constantDivId + "\'> </div>";
		return html;
	}
	
	function variableChange(comboBox, fuzzyRuleIndex, variablesIndex) {
		var comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
		var divName = "fuzzyRuleArgumentConstant[" + fuzzyRuleIndex + "]["+ variablesIndex+"]";
		if (comboBoxValue == "constant") {
			var html = "<input type=\'text\' name=\'fuzzyRuleArgumentConstant["+ fuzzyRuleIndex + "][" + variablesIndex + "]\'>";
			document.getElementById(divName).innerHTML = html;
		}
		else {
			document.getElementById(divName).innerHTML = "";
		}
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
				addQuantifiers()
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
		
		var html = "<select name=\'"+queryLinePredicateGeneralId"\'"+
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
			var queryLineDivContainer = document.createElement('div');
			queryLineDivContainer.id=queryLineGeneralId+".divContainer";
			//newDiv.innerHTML = "<table id=\'queryLineTable_"+queryLinesCounter+"\'><tr><td>" +  
			//		chooseQuantifierCode(queryLinesCounter, 0) + "</td><td>" +
			//		chooseQuantifierCode(queryLinesCounter, 1) + "</td><td>" +
			//		chooseRuleCode(queryLinesCounter, startupType) + "</td>" +
			//		fuzzyRuleArgsCode(queryLinesCounter) + "</tr></table>";
			
			document.getElementById(queryLinesDivId).appendChild(queryLineDivContainer);			
			addChooseRule(queryLineDivContainer.id, queryLineGeneralId, startupType);
			
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
