<jsp:include page="commonHtmlHead.jsp" />
<%@page import="auxiliar.JspsUtils"%>
<%@page import="constants.KConstants"%>
<%@page import="constants.KUrls"%>


<body>
   	<header>
		<div id="bodyHeadTable" class="bodyHeadTable">
			<div id="bodyHeadTitle" class="bodyHeadTable">
				FleSe : <span class="underline">Fle</span>xible 
				<span class="underline">Se</span>arches in Databases
			</div>
			<div id="bodyHeadLogged" class="bodyHeadTable">
			<%  
				String localUserInfoName = JspsUtils.getLocalUserInfoName(request);
				if ("".equals(localUserInfoName)) {
					%>Not logged in<%
				}
				else {
					%>
					logged as <br /> 
					<%= localUserInfoName %>
					<br />
					<a id='userOptions' title='user options' href='#' onclick="return loadAjaxIn('mainSecDiv', '<%=KUrls.User.Options.getUrl(true) %>');">user options</a> | 
					<a id='newQuery' title='new query' href='#' onclick="return loadAjaxIn('mainSecDiv', '<%=KUrls.Queries.SelectProgramFile.getUrl(true) %>');">new query</a>
			<%  } %>
			</div>
			<div id="bodyHeadLogout" class="bodyHeadTable">
				<a id="signOut" title="Sign out" href="<%=KUrls.Auth.SignOut.getUrl(false)%>">Sign out</a>
			</div>
		</div>
	</header>
	<br />
	<section id="msgs" class="bodyToUserMsgs">

	</section>
	<br />

	<section id="mainSecDiv" class="">
	<% if ("".equals(localUserInfoName)) { %>
		<jsp:include page="providers.jsp" />
	<% } %>
	</section>
	<br />
	
	<div id="footer">
	
	</div>
	<br /><br /><br /><br /><br />

	<% if (! "".equals(localUserInfoName)) { %>
	<script type="text/javascript">
		loadAjaxIn('mainSecDiv', "<%=KUrls.Queries.SelectProgramFile.getUrl(true)%>");		
	</script>
	<% } %>
</body>
</html>
