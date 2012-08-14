<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<%@page import="java.util.List"%>
<%@page import="java.util.Iterator"%>
<%@page import="org.brickred.socialauth.Profile"%>
<%@page import="org.brickred.socialauth.Contact"%>
<%@page import="org.brickred.socialauth.AuthProvider"%>

<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Insert title here</title>
</head>
<body>

	<h2>User information</h2>
	<%
		Profile profile = (Profile) session.getAttribute("profile");
		if (profile == null) {
			out.print("<h3>"+"Sorry, user profile is null"+"</h3>");
			out.print("<h3>"+"DisplayName: "+session.getAttribute("userDisplayName")+"</h3>");
		}
		else {
			out.print("<h3>"+"Country: "+profile.getCountry()+"</h3>");
			out.print("<h3>"+"DisplayName: "+profile.getDisplayName()+"</h3>");
			out.print("<h3>"+"Email: "+profile.getEmail()+"</h3>");
			out.print("<h3>"+"FirstName: "+profile.getFirstName()+"</h3>");
			out.print("<h3>"+"FullName: "+profile.getFullName()+"</h3>");
			out.print("<h3>"+"Gender: "+profile.getGender()+"</h3>");
			out.print("<h3>"+"Language: "+profile.getLanguage()+"</h3>");
			out.print("<h3>"+"LastName: "+profile.getLastName()+"</h3>");
			out.print("<h3>"+"Location: "+profile.getLocation()+"</h3>");
			out.print("<h3>"+"ProfileImageUrl: "+profile.getProfileImageURL()+"</h3>");
			out.print("<h3>"+"ProviderId: "+profile.getProviderId()+"</h3>");
			out.print("<h3>"+"ValidatedId: "+profile.getValidatedId()+"</h3>");
			out.print("<h3>"+"Date Of BirthDay: "+profile.getDob()+"</h3>");
		}
	%>
	<h2>User contacts information</h2>
	<%
		AuthProvider provider = (AuthProvider) session.getAttribute("provider");
		if (provider == null) {
			out.print("<h3>"+"Sorry, provider is null"+"</h3>");
		}
		else {
			List<Contact> contactsList = provider.getContactList();
			if (contactsList == null) {
				out.print("<h3>"+"Sorry, contactsList is null"+"</h3>");
			}
			else {
				Iterator<Contact> contactsIterator = contactsList.iterator();
				if (contactsIterator == null) {
					out.print("<h3>"+"Sorry, contactsIterator is null"+"</h3>");
				}
				else {
					while (contactsIterator.hasNext()) {
						Contact contact = contactsIterator.next();
						out.print("<h3>"+"DisplayName: "+contact.getDisplayName()+"</h3>");
						out.print("<h4>"+"Email: "+contact.getEmail()+"</h4>");
						out.print("<h4>"+"EmailHash: "+contact.getEmailHash()+"</h4>");
						out.print("<h4>"+"FirstName: "+contact.getFirstName()+"</h4>");
						out.print("<h4>"+"Id: "+contact.getId()+"</h4>");
						out.print("<h4>"+"LastName: "+contact.getLastName()+"</h4>");
						out.print("<h4>"+"ProfileUrl: "+contact.getProfileUrl()+"</h4>");
						out.print("<h4>"+"OtherEmails: "+contact.getOtherEmails()+"</h4>");
					}
				}
			}
		}
	%>
	<h2><a href="DataBasesMenuServlet">Back to DataBases Menu</a></h2>

</body>
</html>