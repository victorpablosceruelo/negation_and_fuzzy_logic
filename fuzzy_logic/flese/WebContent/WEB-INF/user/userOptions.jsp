<%@page import="constants.KConstants"%>
<%@page import="storeHouse.SessionStoreHouse"%>
<%@page import="auxiliar.JspsUtils"%>
<%@page import="java.util.List"%>
<%@page import="java.util.Iterator"%>
<%@page import="org.brickred.socialauth.Profile"%>
<%@page import="org.brickred.socialauth.Contact"%>
<%@page import="org.brickred.socialauth.AuthProvider"%>

<script type="text/javascript">
<%
	SessionStoreHouse sessionStoreHouse = JspsUtils.getSessionStoreHouse(request);
	AuthProvider provider = sessionStoreHouse.getAuthProvider();
	Profile profile = null;
	if (provider != null) profile = provider.getUserProfile();

	out.println("cleanUpUserInformation();");
	if (profile == null) {
		out.println("addToUserInformation(0, 'DisplayName', '"+ request.getAttribute("localUserName") + "');");
	} else {
		out.println("addToUserInformation(0, 'DisplayName', '" + profile.getDisplayName() + "');");
		out.println("addToUserInformation(1, 'Email', '" + profile.getEmail() + "');");
		out.println("addToUserInformation(2, 'FirstName', '" + profile.getFirstName() + "');");
		out.println("addToUserInformation(3, 'LastName', '" + profile.getLastName() + "');");
		out.println("addToUserInformation(4, 'FullName', '" + profile.getFullName() + "');");
		out.println("addToUserInformation(5, 'Language', '" + profile.getLanguage() + "');");
		out.println("addToUserInformation(6, 'Country', '" + profile.getCountry() + "');");
		out.println("addToUserInformation(7, 'Location', '" + profile.getLocation() + "');");
		out.println("addToUserInformation(8, 'Gender', '" + profile.getGender() + "');");
		out.println("addToUserInformation(9, 'ProfileImageUrl', '" + profile.getProfileImageURL() + "');");
		out.println("addToUserInformation(10, 'ProviderId', '" + profile.getProviderId() + "');");
		out.println("addToUserInformation(11, 'ValidatedId', '" + profile.getValidatedId() + "');");
		out.println("addToUserInformation(12, 'Date Of BirthDay', '" + profile.getDob() + "');");
	}

	/*
	if (provider != null) {
		List<Contact> contactsList = provider.getContactList();
		if (contactsList != null) {
			Iterator<Contact> contactsIterator = contactsList
					.iterator();
			if (contactsIterator != null) {
				while (contactsIterator.hasNext()) {
					Contact contact = contactsIterator.next();
					out.println("<tr><td rowspan=7>" + contact.getDisplayName() + "</td>");
					out.println("<td>" + "Email" + "</td><td>" + contact.getEmail() + "</td></tr>");
					out.println("<tr><td>" + "EmailHash" + "</td><td>"
							+ contact.getEmailHash() + "</td></tr>");
					out.println("<tr><td>" + "FirstName" + "</td><td>"
							+ contact.getFirstName() + "</td></tr>");
					out.println("<tr><td>" + "Id" + "</td><td>"
							+ contact.getId() + "</td></tr>");
					out.println("<tr><td>" + "LastName" + "</td><td>"
							+ contact.getLastName() + "</td></tr>");
					out.println("<tr><td>" + "ProfileUrl" + "</td><td>"
							+ contact.getProfileUrl() + "</td></tr>");
					out.println("<tr><td>" + "OtherEmails" + "</td><td>"
							+ contact.getOtherEmails() + "</td></tr>");
				}
			}
		}
	}
	*/
%>
	insertUserOptions('<%= KConstants.JspsDivsIds.mainSecDivId %>');
</script>