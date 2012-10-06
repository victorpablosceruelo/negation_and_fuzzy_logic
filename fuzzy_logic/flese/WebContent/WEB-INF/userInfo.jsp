<jsp:include page="commonHead.jsp" />
<!-- JavaScript Debugging Code and more -->
<jsp:include page="commonJavaScriptCode.jsp" />

<%@page import="java.util.List"%>
<%@page import="java.util.Iterator"%>
<%@page import="org.brickred.socialauth.Profile"%>
<%@page import="org.brickred.socialauth.Contact"%>
<%@page import="org.brickred.socialauth.AuthProvider"%>

<%
	Profile profile = (Profile) session.getAttribute("profile");
	AuthProvider provider = (AuthProvider) session.getAttribute("provider");
%>

<body>
    <div id="body">
    	<jsp:include page="commonBody.jsp" />
    	<h3><a href="DataBasesMenuServlet">Program Files Menu</a> &gt; User Info </h3>

		<h3>User information</h3>
		<table>
			<thead>
				<tr>
					<th>Field</th>
					<th>Value</th>
				</tr>
			</thead>
			<%
				if (profile == null) {
					out.print("<tr><td>"+"DisplayName"+"</td><td>"+session.getAttribute("localUserName")+"</td></tr>");
				}
				else {
					out.print("<tr><td>"+"Country"+"</td><td>"+profile.getCountry()+"</td></tr>");
					out.print("<tr><td>"+"DisplayName"+"</td><td>"+profile.getDisplayName()+"</td></tr>");
					out.print("<tr><td>"+"Email"+"</td><td>"+profile.getEmail()+"</td></tr>");
					out.print("<tr><td>"+"FirstName"+"</td><td>"+profile.getFirstName()+"</td></tr>");
					out.print("<tr><td>"+"FullName"+"</td><td>"+profile.getFullName()+"</td></tr>");
					out.print("<tr><td>"+"Gender"+"</td><td>"+profile.getGender()+"</td></tr>");
					out.print("<tr><td>"+"Language"+"</td><td>"+profile.getLanguage()+"</td></tr>");
					out.print("<tr><td>"+"LastName"+"</td><td>"+profile.getLastName()+"</td></tr>");
					out.print("<tr><td>"+"Location"+"</td><td>"+profile.getLocation()+"</td></tr>");
					out.print("<tr><td>"+"ProfileImageUrl"+"</td><td>"+profile.getProfileImageURL()+"</td></tr>");
					out.print("<tr><td>"+"ProviderId"+"</td><td>"+profile.getProviderId()+"</td></tr>");
					out.print("<tr><td>"+"ValidatedId"+"</td><td>"+profile.getValidatedId()+"</td></tr>");
					out.print("<tr><td>"+"Date Of BirthDay"+"</td><td>"+profile.getDob()+"</td></tr>");
				}
			%>
		</table>
		
		<%
		
		if (provider != null) {
			List<Contact> contactsList = provider.getContactList();
			if (contactsList != null) {
				Iterator<Contact> contactsIterator = contactsList.iterator();
				if (contactsIterator != null) {
		%>
					<h3>User contacts information</h3>
							<table>
							<thead>
								<tr>
									<th>Display Name</th>
									<th>Field</th>
									<th>Value</th>
								</tr>
							</thead>
		<%			
					while (contactsIterator.hasNext()) {
						Contact contact = contactsIterator.next();
						out.print("<tr><td rowspan=7>"+contact.getDisplayName()+"</td>");
						out.print("<td>"+"Email"+"</td><td>"+contact.getEmail()+"</td></tr>");
						out.print("<tr><td>"+"EmailHash"+"</td><td>"+contact.getEmailHash()+"</td></tr>");
						out.print("<tr><td>"+"FirstName"+"</td><td>"+contact.getFirstName()+"</td></tr>");
						out.print("<tr><td>"+"Id"+"</td><td>"+contact.getId()+"</td></tr>");
						out.print("<tr><td>"+"LastName"+"</td><td>"+contact.getLastName()+"</td></tr>");
						out.print("<tr><td>"+"ProfileUrl"+"</td><td>"+contact.getProfileUrl()+"</td></tr>");
						out.print("<tr><td>"+"OtherEmails"+"</td><td>"+contact.getOtherEmails()+"</td></tr>");
					}
		%>
							</table>
		<%
				}
			}
		}
		
		%>
	</div>
</body>
</html>