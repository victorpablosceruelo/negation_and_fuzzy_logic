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

/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------------------------------------------- */

function fileViewAction(fileViewContentsDivId, urlFileView, fileOwner, fileName) {
	var fileViewContentsDiv = getContainer(fileViewContentsDivId);
	
	$.get(urlView + "&<%=KConstants.Request.fileOwnerParam%>=" + fileOwner + "&<%=KConstants.Request.fileNameParam%>=" + fileName, 
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
		                title: 'Contents of program file ' + fileName
		            });
			        // $( "#" + fileViewContentsDivId ).dialog();
			    });
			});
	
	//prevent the browser to follow the link
	return false;
}


/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/
/* ----------------------------------------------------------------------------------------------------------------------------*/

<% if (JspsUtils.getStringWithValueS().equals("N")) { %>
</script>
<% } %>