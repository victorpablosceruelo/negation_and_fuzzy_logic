

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
			if ((session.getAttribute("msg1") != null) || (session.getAttribute("msg2") != null)) {
				%><h2>Messages to the user</h2><%
			}
			if (session.getAttribute("msg1") != null) {
				%><h3><%=session.getAttribute("msg1")%></h3><%
				session.removeAttribute("msg1");
			}
			if (session.getAttribute("msg2") != null) {
				%><h3><%=session.getAttribute("msg2")%></h3><%
				session.removeAttribute("msg2");
			}			
		}
		else {
			%><h3> ERROR: Session is null.</h3><%
		}
	%>
</div>

