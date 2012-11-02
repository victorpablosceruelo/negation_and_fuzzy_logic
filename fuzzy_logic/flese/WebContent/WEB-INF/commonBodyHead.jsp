
<!-- beginning of commonBodyHead -->

<%@page import="auxiliar.ServletsAuxMethodsClass"%>

<%
	String localUserName = null;
	if (session != null) {
		localUserName = (String) session.getAttribute("localUserName");
	}
%>

	<table id="bodyHead" class="bodyHead">
		<tr class="bodyHead">
			<td id="bodyHeadTitle" class="bodyHead">
				FleSe : <span class="underline">Fle</span>xible 
				<span class="underline">Se</span>arches in Databases
			</td>
			<td id="bodyHeadLogged" class="bodyHead">
				<% if ((localUserName != null) && (! "".equals(localUserName))) { %>
				logged as <%=localUserName %>
				<br>
				<a title="view user profile" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.SocialAuthServletUserInfo, request, null) %>">user profile</a>
				<% } else { %>
				Not logged in
				<% } %>
			</td>
			<td id="bodyHeadLogout" class="bodyHead">
				<a title="Sign out" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.SocialAuthServletSignOut, request, null) %>">Signout</a>
			</td>
		</tr>
	</table>



<div class="bodyToUserMsgs">
	<%
		if (session != null) {
			if (session.getAttribute("msgs") != null) {
				%><h3 class="bodyToUserMsgs">Messages to the user:</h3>
					<ul class="bodyToUserMsgs">
				<%
				String [] msgs = (String []) session.getAttribute("msgs");
				for (int i=0; i<msgs.length; i++) {
					%><li class="bodyToUserMsgs"><%=msgs[i]%></li><%
				}
				%></ul><%
				session.removeAttribute("msgs");
			}			
		}
		else {
			%>
				<ul class="bodyToUserMsgs">
					<li class="bodyToUserMsgs"> ERROR: Session is null.</li>
				</ul>
			<%
		}
	%>
</div>

<!-- end of commonBodyHead -->


