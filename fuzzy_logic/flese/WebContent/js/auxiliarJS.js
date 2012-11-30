/*!
 * programQuery Library v1
 * Author: Victor Pablos Ceruelo
 */

function loadingImageHtml() {
	return "<img src=\"images/loading.gif\" width=\"200\" alt=\"loading\" title=\"loading\" />";
}

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
		
		var selectDatabaseDiv = document.createElement('div');
		selectDatabaseDiv.id = "selectDatabaseDiv";
		parentDiv.appendChild(selectDatabaseDiv);
		
		if ((filesList == null) || (filesList.length == 0)) {
			selectDatabaseDiv.innerHTML = "No databases. Please upload one via your user options.";
		}
		else {
			var html = "";
			html += "<select name='selectedDatabase' onchange='selectedProgramDatabaseChanged(this, \""+parentDivId+"\")' >";
			html += "<option id='----' value='----'>----</option>";
			for (var i=0; i<filesList.length; i++) {
				html += "<option id=" + filesList[i].fileName + "-owned-by-" + filesList[i].fileOwner +
						"value=" + filesList[i].fileName + "-owned-by-" + filesList[i].fileOwner + ">" + 
						filesList[i].fileName + " ( owned by " + filesList[i].fileOwner + " ) " +
						"</option>";
			}
			html += "</select>";
			selectDatabaseDiv.innerHTML = html;
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

function insertProgramFileSelectionAux(parentDivId) {	
}

function selectedProgramDatabaseChanged(comboBox, parentDivId) {
	// debug.info("parentDivId: " + parentDivId);
	var parentDiv = document.getElementById(parentDivId);
	
	var selectQueryDiv = document.getElementById('selectQueryDiv');
	if (selectQueryDiv == null) {
		selectQueryDiv = document.createElement('div');
		selectQueryDiv.id = 'selectQueryDiv';
		parentDiv.appendChild(selectDatabaseDiv);
	}

	selectQueryDiv.innerHTML = loadingImageHtml();
	
	var comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
	if (comboBoxValue == "----") {
		selectQueryDiv.innerHTML="Please choose a valid database to continue.";
	}
	else {
		var fileName = null;
		var fileOwner = null;
		var separation = "-owned-by-";
	
		i = comboBoxValue.indexOf(separation);
		if (i != -1) {
			fileName = comboBoxValue.substring(0, i);
			fileOwner = comboBoxValue.substring(i+separation.length);
		}
		
		$.getScript(urlMappingFor('ProgramFileIntrospectionRequest') + "&fileName="+fileName+"&fileOwner="+fileOwner, 
				function(data, textStatus, jqxhr) {
		   			console.log(data); //data returned
		   			console.log(textStatus); //success
		   			console.log(jqxhr.status); //200
		   			console.log('Load was performed.');
				});
	}
	
}