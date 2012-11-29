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
	
	$.getScript(urlMappingFor('FilesListRequest'), insertProgramFileSelectionAux(parentDivId));
}

function insertProgramFileSelectionAux(parentDivId) {
	var parentDiv = document.getElementById(parentDivId);
	parentDiv.innerHTML = "";
	
	var selectDatabaseDiv = document.createElement('div');
	parentDiv.appendChild(selectDatabaseDiv);
	
	if ((filesList == null) || (filesList.length == 0)) {
		selectDatabaseDiv.innerHTML = "No databases. Please upload one via your user options.";
	}
	else {
		var html = "";
		html += "<select name='selectedDatabase' onchange='selectedProgramDatabaseChanged(this, "+parentDivId+")' >";
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
		
		
		
	
	
}

function selectedProgramDatabaseChanged(comboBox, selectQueryDivId) {
	
	var selectQueryDiv = document.getElementById(selectQueryDivId);
	selectQueryDiv.innerHTML = loadingImageHtml();
	
	var comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
	var fileName = null;
	var fileOwner = null;
	var separation = "-owned-by-";
	
	i = comboBoxValue.indexOf(separation);
	if (i != -1) {
		fileName = comboBoxValue.substring(0, i);
		fileOwner = comboBoxValue.substring(i+separation.length);
	}
	
	$.getScript("DispatcherServlet?op=databaseIntrospection&fileName="+fileName+"&fileOwner="+fileOwner, 
			function(data, textStatus, jqxhr) {
		   		console.log(data); //data returned
		   		console.log(textStatus); //success
		   		console.log(jqxhr.status); //200
		   		console.log('Load was performed.');
			});
	
	alert("sent a request to the dispatcher");
	
}