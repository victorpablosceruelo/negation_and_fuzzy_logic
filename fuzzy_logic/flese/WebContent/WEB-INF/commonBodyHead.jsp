
<!-- beginning of commonBodyHead -->

<%@page import="constants.KConstants"%>
<%@page import="auxiliar.ServletsAuxMethodsClass"%>
<script type="text/javascript">

	function isString(o) {
		var result = false;
		if ((o != null) && (o != undefined)) {
			result = (typeof o == "string") || (o instanceof String) || (typeof o == "object" && o.constructor === String);
		}
		// alert("isString returns " + result);
		return result;
	}
	
	function setupHref (aId, href) {
		var aLink = document.getElementById(aId);
		if (aLink != null) aLink.href = href;
	}

<%

%>
	function setupBodyHeadLoggedDiv(mainSectionDivId) {
		var bodyHeadLoggedDiv = document.getElementById("bodyHeadLogged");
		if (localUserName == null) {
			bodyHeadLoggedDiv.innerHTML = "Not logged in";
		}
		else {
			bodyHeadLoggedDiv.innerHTML = "logged as <br /> " + localUserName + " <br> " + 
			"<a id='userOptions' title='user options' href='' onclick='return insertUserOptions(\""+mainSectionDivId+"\");'>user options</a> | " +
			"<a id='newQuery' title='new query' href='' onclick='return insertProgramFileSelection(\""+mainSectionDivId+"\");'>new query</a>";
		}
	}
	

	

	
</script>



<header>
	<div id="bodyHeadTable" class="bodyHeadTable">
		<div id="bodyHeadTitle" class="bodyHeadTable">
			FleSe : <span class="underline">Fle</span>xible 
			<span class="underline">Se</span>arches in Databases
		</div>
		<div id="bodyHeadLogged" class="bodyHeadTable">
		<%
			if (request != null) {
				String localUserName = (String) request.getAttribute("localUserName");
				if (localUserName != null) {
					out.write("    var localUserName = '" + localUserName + "';\n");
	} 
	else {
		out.write("    var localUserName = null;\n");
	}	
		</div>
		<div id="bodyHeadLogout" class="bodyHeadTable"><a id="signOut" title="Sign out" 
				href="<%= KConstants.Pages.SignOutRequest.getUrl() %>">Sign out</a>
		</div>
	</div>
</header>
<br />
<section id="bodyToUserMsgs" class="bodyToUserMsgs">
<%
	if (request != null) {
		if (request.getAttribute("msgs") != null) {
			String [] msgs = (String []) request.getAttribute("msgs");
			if (msgs.length > 0) {
				out.write("<ul>");
				for (int i=0; i<msgs.length; i++) {
					out.write("<li>" + msgs[i] + "</li>");
				}
				out.write("</ul>");
			}
			request.removeAttribute("msgs");
		}			
	}
	else {
		out.write("ERROR: request is null.");
	}
%>
</section>
<br />

<script type="text/javascript">
	setupBodyHeadLoggedDiv('mainSection');
	showMsgsToTheUser();
</script>

<!-- end of commonBodyHead -->


