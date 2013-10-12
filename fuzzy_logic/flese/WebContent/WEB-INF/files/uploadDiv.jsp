<%@page import="constants.KConstants"%>
<%@page import="constants.KUrls"%>
<%
	String urlUpload = KUrls.Files.Upload.getUrl(true);
%>


	Upload Program files <br /> 
	<FORM ID='<%=KConstants.JspsDivsIds.uploadFormId %>' ENCTYPE='multipart/form-data' method='POST' accept-charset='UTF-8' 
			target='<%=KConstants.JspsDivsIds.uploadFormTargetiFrameId %>' action='<%=urlUpload %>' >
			<INPUT TYPE='file' NAME='programFileToUpload' size='50' 
					onchange='uploadActionOnChange("<%=KConstants.JspsDivsIds.uploadFormId %>", "<%=KConstants.JspsDivsIds.uploadStatusDivId%>");'>
	</FORM>
	
	<div id='<%=KConstants.JspsDivsIds.uploadStatusDivId%>'></div>
	<iframe id='<%=KConstants.JspsDivsIds.uploadFormTargetiFrameId %>' name='<%=KConstants.JspsDivsIds.uploadFormTargetiFrameId%>' style='display:none;' ></iframe>
	<!-- style='display:none;' -->

<script type="text/javascript">
function insertFileUploadFacility(mainSecDivId, urlUpload, uploadFormId, uploadStatusDivId, uploadFormTargetiFrameId) {
	
	// This does not work on google chrome: "src='#' " 
    	
	$('#' + uploadFormTargetiFrameId).load(function() {
		// document.getElementById('#' + submitiFrameId);
		var responseHtmlText = null;
		var iFrameWindow = getIframeWindow(this);
		if ((notNullNorUndefined(iFrameWindow)) && (notNullNorUndefined(iFrameWindow.document)) && (notNullNorUndefined(iFrameWindow.document.body))) {
			responseHtmlText = iFrameWindow.document.body.innerHTML;
			// Do something with response text.
			if (notNullNorUndefined(responseHtmlText)) {
				iFrameWindow.document.body.innerHTML="";
				debug.info(responseHtmlText);
			}
			// Clear the content of the iframe.
			// this.contentDocument.location.href = '/images/loading.gif';
			// alert("responseText: " + responseHtmlText);
			var container = getContainer(uploadStatusDivId);
			container.style.visibility = 'visible';
			container.innerHTML = responseHtmlText;
		}
		  
	});	
}

function uploadActionOnChange(formId, uploadStatusDivId) {
	// alert("Upload Submit Action started ...");
	// document.getElementById(uploadStatusDivId).style.visibility = 'visible';
	// document.getElementById(uploadStatusDivId).innerHTML = loadingImageHtml(true);

	var form = document.getElementById(formId);
	form.submit();
}

function uploadFileResults(uploadStatusDivId, results) {
	var uploadStatusDiv = getContainer(uploadStatusDivId);
	for (var i=0; i<results.length; i++) {
		document.getElementById(uploadStatusDiv).innerHTML += "<br>" + results[i];
	}
}
</script>




<!-- END -->