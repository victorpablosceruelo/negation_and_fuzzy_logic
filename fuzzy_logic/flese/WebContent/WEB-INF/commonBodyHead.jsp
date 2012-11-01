
<%@page import="auxiliar.ServletsAuxMethodsClass"%>

<%
	String localUserName = (String) session.getAttribute("localUserName");
%>

	<table id="bodyHead" class="bodyHead">
		<tr class="bodyHead">
			<td id="bodyHeadTitle" class="bodyHead">
				<H1>FleSe : <u>Fle</u>xible <u>Se</u>arches in Databases</H1>
			</td>
			<td id="bodyHeadLogged" class="bodyHead">
				<% if ((localUserName != null) && (! "".equals(localUserName))) { %>
				logged as <%=localUserName %>
				<br>
				<a title="view user profile" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.AuthenticationServletUserInfo, request, null) %>">user profile</a>
				<% } else { %>
				Not logged in
				<% } %>
			</td>
			<td id="bodyHeadLogout" class="bodyHead">
				<a title="Sign out" href="<%=ServletsAuxMethodsClass.getFullPathForUriNickName(ServletsAuxMethodsClass.AuthenticationServletSignout, request, null) %>">Signout</a>
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
			%><p class=class="bodyToUserMsgs"> ERROR: Session is null.</h3><%
		}
	%>
</div>

