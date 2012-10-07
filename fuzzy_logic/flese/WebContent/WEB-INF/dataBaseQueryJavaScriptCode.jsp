<script type="text/javascript">

	var fuzzyRulesCounter = 0;
	var limit = 50;
	var fuzzyVarsCounter = 0;

	function chooseQuantifierCode(fuzzyRuleIndex, fuzzyRuleQuantifierIndex) {
		var html = "<select name=\'fuzzyRuleQuantifier[" + fuzzyRuleIndex + "][" + fuzzyRuleQuantifierIndex + "]\'>";
		html += "<option name=\'none\' value=\'none\'>none</option>";
		for (var i=0; i<quantifiersArray.length; i++){
			html += "<option name=\'" + quantifiersArray[i].predName + 
						"\' value=\'" + quantifiersArray[i].predName + "\'>"+
						quantifiersArray[i].predName + "</option>";
		}
		html += "</select>";
		return html;
	}
	function chooseFuzzyRuleCode(fuzzyRuleIndex) {
		var html = "<select name=\'fuzzyRule[" + fuzzyRuleIndex + 
		           "]\' onchange=\"fuzzyRuleChange(this, " + fuzzyRuleIndex + ");\">";
		html += "<option name=\'none\' value=\'none\''>none</option>";
		for (var i=0; i<fuzzyRulesArray.length; i++){
			html += "<option name=\'" + fuzzyRulesArray[i].predName + 
						"\' value=\'" + fuzzyRulesArray[i].predName + "\'>"+
						fuzzyRulesArray[i].predName + "</option>";
		}
		html += "</select>";
		return html;
	}
	function fuzzyRuleArgsCode(fuzzyRuleIndex) {
		return "<div id=\'fuzzyRuleArgs[" + fuzzyRuleIndex + "]\'> </div>";
	}
	
	function addFuzzyRuleArgumentFields(fuzzyRuleIndex, fuzzyVarIndex) {
		debug.info("addFuzzyRuleArgumentFields: fuzzyRuleIndex:" + fuzzyRuleIndex + " fuzzyVarIndex:" + fuzzyVarIndex);
		var html = "";
		html += "<select name=\'fuzzyRuleArgument[" + fuzzyRuleIndex + "]["+ fuzzyVarIndex+"]\'" + 
				"onchange=\"fuzzyRuleArgumentChange(this, " + fuzzyRuleIndex + ", " + fuzzyVarIndex +");\">";
		for (var k=fuzzyVarIndex; k<fuzzyVarsCounter; k++){
			html += "<option name=\'var_" + k + "\' value=\'var_" + k + "\'>variable_"+ k + "</option>";
		}
		for (var k=0; k<fuzzyVarIndex; k++){
			html += "<option name=\'var_" + k + "\' value=\'var_" + k + "\'>variable_"+ k + "</option>";
		}
		html += "<option name=\'constant' value=\'constant\'>constant</option>";
		html += "</select>";
		html += "<div id=\'fuzzyRuleArgumentConstant[" + fuzzyRuleIndex + "]["+ fuzzyVarIndex+"]\'> </div>";
		return html;
	}
	function fuzzyRuleArgumentChange(comboBox, fuzzyRuleIndex, fuzzyVarIndex) {
		var comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
		var divName = "fuzzyRuleArgumentConstant[" + fuzzyRuleIndex + "]["+ fuzzyVarIndex+"]";
		if (comboBoxValue == "constant") {
			var html = "<input type=\'text\' name=\'fuzzyRuleArgumentConstant["+ fuzzyRuleIndex + "][" + fuzzyVarIndex + "]\'>";
			document.getElementById(divName).innerHTML = html;
		}
		else {
			document.getElementById(divName).innerHTML = "";
		}
	}
	
	function fuzzyRuleChange(comboBox, fuzzyRuleIndex) {
		// var elementId = "";
		var divName = "fuzzyRuleArgs[" + fuzzyRuleIndex + "]";
		debug.info("fuzzyRuleChange(comboBox, " + fuzzyRuleIndex + ") -> divName: " + divName);
		// var comboBox = document.getElementById('fuzzyRule[' + fuzzyRuleIndex + ']');
		var comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
		debug.info("comboBoxValue: " + comboBoxValue);
		
		var found = false;
		var i = 0;
		var predArity = 0;
		while ((! found) && (i < fuzzyRulesArray.length)) {
			debug.info("fuzzyRulesArray["+i+"]: " + fuzzyRulesArray[i].predName);
			if (comboBoxValue == fuzzyRulesArray[i].predName) {
				found = true;
				predArity = fuzzyRulesArray[i].predArity;
			}
			i++;
		}
		debug.info("Predicate Arity: " + predArity);
		
		var fuzzyVarIndex = fuzzyVarsCounter;
		fuzzyVarsCounter += predArity;
		var html = "";
		while (fuzzyVarIndex < fuzzyVarsCounter) {
			if (fuzzyVarIndex +1 == fuzzyVarsCounter) html+= "result: ";
			html += addFuzzyRuleArgumentFields(fuzzyRuleIndex, fuzzyVarIndex);
			fuzzyVarIndex += 1;
		}
		document.getElementById(divName).innerHTML = html;
		document.getElementById('hiddenNumOfVariables').innerHTML = " " + 
					"<input type=\"hidden\" name=\"fuzzyVarsCounter\" id=\"fuzzyVarsCounter\" value=\""+ fuzzyVarsCounter + "\" />";
	}
	
	function addQueryLine(divName) {
		if (fuzzyRulesCounter == limit) {
			alert("You have reached the limit of adding " + fuzzyRulesCounter + " subqueries.");
		} else {
			var newdiv = document.createElement('div');
			newdiv.innerHTML = " " +  
					chooseQuantifierCode(fuzzyRulesCounter, 0) + " &nbsp; " +
					chooseQuantifierCode(fuzzyRulesCounter, 1) + " &nbsp; " +
					chooseFuzzyRuleCode(fuzzyRulesCounter) + " &nbsp; " +
					fuzzyRuleArgsCode(fuzzyRulesCounter) + " &nbsp; ";
			
			document.getElementById(divName).appendChild(newdiv);
			fuzzyRulesCounter++;
		}
	}
</script>
