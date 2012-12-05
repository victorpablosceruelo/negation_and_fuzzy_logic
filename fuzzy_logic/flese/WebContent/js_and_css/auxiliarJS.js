/*!
 * programQuery Library v1
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
		
		var selectDatabaseDiv = document.createElement('div');
		selectDatabaseDiv.id = "selectDatabaseDiv";
		parentDiv.appendChild(selectDatabaseDiv);
		
		if ((filesList == null) || (filesList.length == 0)) {
			selectDatabaseDiv.innerHTML = "No databases. Please upload one via your user options.";
		}
		else {
			var html = "";
			html += "<select name='selectedDatabase' onchange='selectedProgramDatabaseChanged(this, \""+parentDivId+"\")' >";
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
	
	var comboBoxValue = comboBox.options[comboBox.selectedIndex].value;
	// alert("comboBoxValue: " + comboBoxValue);
	if ((comboBoxValue == null) || (comboBoxValue == "") || (comboBoxValue == "----")) {
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
		else {
			fileName = '';
			fileOwner = '';
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
	   			
	   			var userInformationDiv = document.createElement('div');
	   			userInformationDiv.id = "userInformationDiv";
	   			userInformationDiv.className = "userInformationTable";
	   			parentDiv.appendChild(userInformationDiv);
	   			
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
	var filesListDiv = document.createElement('div');
	filesListDiv.id = "filesListDiv";
	parentDiv.appendChild(filesListDiv);
	filesListDiv.innerHTML = loadingImageHtml();
	
	var fileViewContents = document.createElement('div');
	fileViewContents.id = "fileViewContents";
	parentDiv.appendChild(fileViewContents);
	fileViewContents.innerHTML = "";
		
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
		   								 	 "onclick='fileViewAction(" + i + ", " + fileViewContents.id + ");' >" + 
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
	var fileViewContents = document.getElementById(fileViewContentsDivId);
	
	$.get(urlMappingFor('FileViewRequest') + "&fileName="+filesList[index].fileName+"&fileOwner="+filesList[index].fileOwner, 
			function(data, textStatus, jqxhr) {
				fileViewContents.innerHTML = data;
				
			    $(function() {
			        $( "#" + fileViewContentsDivId ).dialog();
			    });
			});
	
	
	return false;
}

function removeFileAction (index, parentDivId) {
	$.get(urlMappingFor('FileRemoveRequest') + "&fileName="+filesList[index].fileName+"&fileOwner="+filesList[index].fileOwner, 
			function(data, textStatus, jqxhr) {
				fileViewContents.innerHTML = data;
				
				// Reload the screen.
				// alert("parentDivId: " + parentDivId);
				insertUserOptions(parentDivId);
			});
}

function insertFileUploadFacility(parentDivId) {
	var parentDiv = document.getElementById(parentDivId);
	
	parentDiv.innerHTML = "<FORM ID='uploadForm' ENCTYPE='multipart/form-data' method='POST' action='" + urlMappingFor('FileUploadRequest') + "'>" +
						  "<INPUT TYPE='file' NAME='programFileToUpload' size='50' onchange='autoSendForm(\"uploadForm\");'>" +
						  "<DIV id='uploadButton'> </DIV> " +
						  "</FORM>";
	
    <div class="progress">
    <div class="bar"></div >
    <div class="percent">0%</div >
</div>

<div id="status"></div>
	
	var bar = $('.bar');
	var percent = $('.percent');
	var status = $('#status');
	   
	$('form').ajaxForm({
	    beforeSend: function() {
	        status.empty();
	        var percentVal = '0%';
	        bar.width(percentVal);
	        percent.html(percentVal);
	    },
	    uploadProgress: function(event, position, total, percentComplete) {
	        var percentVal = percentComplete + '%';
	        bar.width(percentVal);
	        percent.html(percentVal);
	    },
		complete: function(xhr) {
			status.html(xhr.responseText);
		}
	}); 
	
}

function autoSendForm(formId) {
	var form = document.getElementById(formId);
	form.submit();
}



/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/





