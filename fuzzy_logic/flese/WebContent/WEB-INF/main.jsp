<jsp:include page="commonHtmlHead.jsp" />
<%@page import="auxiliar.JspsUtils"%>
<%@page import="constants.KConstants"%>
<%@page import="constants.KUrls"%>
<%@page import="java.util.ArrayList"%>

<%
	String msgsArray = JspsUtils.getResultMessageInJS(request);
%>



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
					<a id='userOptions' title='user options' href='#' 
						onclick="return loadAjaxIn('<%= KConstants.JspsDivsIds.mainSecDivId %>', 
								'<%=KUrls.User.Options.getUrl(true) %>');">user options</a> | 
					<a id='newQuery' title='new query' href='#' 
						onclick="return loadAjaxIn('<%= KConstants.JspsDivsIds.mainSecDivId %>', 
								'<%=KUrls.Queries.SelectProgramFile.getUrl(true) %>');">new query</a>
			<%  } %>
			</div>
			<div id="bodyHeadLogout" class="bodyHeadTable">
				<a id="signOut" title="Sign out" href="<%=KUrls.Auth.SignOut.getUrl(false)%>">Sign out</a>
			</div>
		</div>
	</header>
	<br />
	<section id="<%=KConstants.JspsDivsIds.msgsSecDivId %>" class="bodyToUserMsgs">

	</section>
	<section id="<%=KConstants.JspsDivsIds.auxAndInvisibleSection %>" style='display:none;' >
	</section>
	
	<br />

	<section id="<%= KConstants.JspsDivsIds.mainSecDivId %>" class="">
	<% if ("".equals(localUserInfoName)) { %>
		<jsp:include page="providers.jsp" />
	<% } %>
	</section>
	<br />
	
	<div id="footer">
	
	</div>
	<br /><br /><br /><br /><br />

	<script type="text/javascript">
		<% if (! "".equals(localUserInfoName)) { %>
			loadAjaxIn('mainSecDiv', "<%=KUrls.Queries.SelectProgramFile.getUrl(true)%>");
		<% } %>		
		<% if ((msgsArray != null) && (msgsArray.length() > 0)) { %>
			showMsgs(<%= msgsArray %>);
		<% } %>
	</script>

</body>
</html>
