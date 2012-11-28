/*!
 * programQuery Library v1
 * Author: Victor Pablos Ceruelo
 */

function selectedProgramDatabaseChanged(comboBox) {
	
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