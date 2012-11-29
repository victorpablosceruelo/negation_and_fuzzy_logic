
<!-- beginning of commonBodyHead -->

<%@page import="auxiliar.ServletsAuxMethodsClass"%>

<%
	String localUserName = null;
	if (request != null) {
		localUserName = (String) request.getAttribute("localUserName");
	}
	if (localUserName != null) {
%>
	<script type="text/javascript">
		var localUserName = "<%=localUserName %>";
	</script>
<%
	} else {
%>
	<script type="text/javascript">
		var localUserName = null;
	</script>
<%		
	}
%>

<script type="text/javascript">
	function urlMapping(nickName, url) {
		this.nickName = nickName;
		this.url = url;
	}
	var urlsMappings = new Array();
<%
	String [][] urlsMappings = ServletsAuxMethodsClass.urlsMappings();
	for (int i=0; i<urlsMappings.length; i++) {
%>
	urlsMappings[<%=i%>] = new urlMapping("<%=urlsMappings[i][0]%>", "<%=urlsMappings[i][1]%>");
<%
	}
%>
	function urlMappingFor (nickName) {
		var found = false;
		var i = 0;
		while ((! found) && (i < urlsMappings.length)) {
			if (urlsMappings[i].nickName == nickName) {
				found = true;
				return urlsMappings[i].url;
			}
			else i++
		}
		return null;
	}
	function setupHref (aId, href) {
		var aLink = document.getElementById(aId);
		if (aLink != null) aLink.href = href;
	}
</script>


<header>
	<div id="bodyHeadTable" class="bodyHeadTable">
		<div id="bodyHeadTitle" class="bodyHeadTable">
			FleSe : <span class="underline">Fle</span>xible 
			<span class="underline">Se</span>arches in Databases
		</div>
		<div id="bodyHeadLogged" class="bodyHeadTable"></div>
		<div id="bodyHeadLogout" class="bodyHeadTable"><a id="signOut" title="Sign out" href="">Signout</a></div>
	</div>
</header>

<script type="text/javascript">
	var bodyHeadLoggedDiv = document.getElementById("bodyHeadLogged");
	if (localUserName == null) {
		bodyHeadLoggedDiv.innerHTML = "Not logged in";
	}
	else {
		bodyHeadLoggedDiv.innerHTML = "logged as <br /> " + localUserName + " <br> " + "<a id='userOptions' title='user options' href=''>user options</a>";
	}
	
	setupHref ('signOut', urlMappingFor('SocialAuthServletSignOut'));
	setupHref ('userOptions', urlMappingFor('SocialAuthServletUserInfo'));
</script>

<section class="bodyToUserMsgs">
	<%
		if (request != null) {
			if (request.getAttribute("msgs") != null) {
				%><h3 class="bodyToUserMsgs">Messages to the user:</h3>
					<ul class="bodyToUserMsgs">
				<%
				String [] msgs = (String []) request.getAttribute("msgs");
				for (int i=0; i<msgs.length; i++) {
					%><li class="bodyToUserMsgs"><%=msgs[i]%></li><%
				}
				%></ul><%
				request.removeAttribute("msgs");
			}			
		}
		else {
			%>
				<ul class="bodyToUserMsgs">
					<li class="bodyToUserMsgs"> ERROR: request is null.</li>
				</ul>
			<%
		}
	%>
</section>

<!-- end of commonBodyHead -->


