

<%
	String localUserName = (String) session.getAttribute("localUserName");
%>

	<table id="bodyHead">
		<tr>
			<td id="bodyHeadTitle">
				<H1>FleSe : <u>Fle</u>xible <u>Se</u>arches in Databases</H1>
			</td>
			<td id="bodyHeadLogged">
				<% if ((localUserName != null) && (! "".equals(localUserName))) { %>
				logged as <%=localUserName %>
				<br>
				<a href="UserInfoServlet">user profile</a>
				<% } else { %>
				Not logged in
				<% } %>
			</td>
			<td id="bodyHeadLogout">
				<a href="SocialAuthServlet?mode=signout">Signout</a>
			</td>
		</tr>
	</table>


<div class="bodyToUserMsgs">
	<%
		if (session != null) {
			if (session.getAttribute("msgs") != null) {
				%><h3>Messages to the user:</h3>
					<ul>
				<%
				String [] msgs = (String []) session.getAttribute("msgs");
				for (int i=0; i<msgs.length; i++) {
					%><li><%=msgs[i]%></li><%
				}
				%></ul><%
				session.removeAttribute("msgs");
			}			
		}
		else {
			%><h3> ERROR: Session is null.</h3><%
		}
	%>
</div>

