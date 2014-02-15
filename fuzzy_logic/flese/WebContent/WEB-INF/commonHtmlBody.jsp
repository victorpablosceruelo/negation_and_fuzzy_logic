<jsp:include page="commonHtmlHead.jsp" />
<%@page import="auxiliar.JspsUtils"%>
<%@page import="constants.KConstants"%>
<%@page import="constants.KUrls"%>
<%@page import="java.util.ArrayList"%>

<body>
	<header>
		<div id="bodyHeadTable" class="bodyHeadTable">
			<div id="bodyHeadTitle" class="bodyHeadTable">
				FleSe : <span class="underline">Fle</span>xible <span
					class="underline">Se</span>arches in Databases
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
				<br /> <a id='userOptions' title='user options' href='#'
					onclick="return loadUserOptions();">user options</a> | <a
					id='newQuery' title='new query' href='#'
					onclick="return loadNewQuery();">new query</a>
				<%  } %>
			</div>
			<div id="bodyHeadLogout" class="bodyHeadTable">
				<a id="signOut" title="Sign out"
					href="<%=KUrls.Auth.SignOut.getUrl(false)%>">Sign out</a>
			</div>
		</div>
	</header>
	<br />
	<section id="<%=KConstants.JspsDivsIds.msgsSecDivId %>"
		class="bodyToUserMsgs"></section>
	<section id="<%=KConstants.JspsDivsIds.auxAndInvisibleSection %>"
		style='display: none;'></section>

	<br />

	<section id="<%= KConstants.JspsDivsIds.mainSecDivId %>" class="">
	</section>
	<br />

	<div id="footer"></div>

	<br />
	<br />
	<br />
	<br />
	<br />

	<!--  <-/-body>  -->
	<!--  <-/-html>  -->