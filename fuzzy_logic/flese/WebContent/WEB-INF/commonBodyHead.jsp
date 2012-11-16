
<!-- beginning of commonBodyHead -->

<%@page import="auxiliar.ServletsAuxMethodsClass"%>

<%
	String localUserName = null;
	if (session != null) {
		localUserName = (String) session.getAttribute("localUserName");
	}
%>

	<div id="bodyHeadContainer" class="bodyHead">
		<div id="bodyHeadTitle" class="bodyHead">
				FleSe : <span class="underline">Fle</span>xible 
				<span class="underline">Se</span>arches in Databases
		</div>
		<div id="bodyHeadLogged" class="bodyHead">
			<% if ((localUserName != null) && (! "".equals(localUserName))) { %>
			logged as <%=localUserName %>
			<br>
			<a title="view user profile" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.SocialAuthServletUserInfo, request, null) %>">user profile</a>
			<% } else { %>
			Not logged in
			<% } %>
		</div>
		<div id="bodyHeadLogout" class="bodyHead">
			<a title="Sign out" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.SocialAuthServletSignOut, request, null) %>">Signout</a>
		</div>
	</div>



<div class="bodyToUserMsgs">
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
</div>

<!-- end of commonBodyHead -->


