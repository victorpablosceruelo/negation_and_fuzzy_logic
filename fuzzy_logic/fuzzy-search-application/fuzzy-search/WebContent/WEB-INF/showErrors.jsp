
	<%
		if (session != null) {
		if ((session.getAttribute("msg1") != null) || (session.getAttribute("msg2") != null)) {
			out.print("<h2>Messages to the user</h2>");
		}
		if (session.getAttribute("msg1") != null) {
			out.print("<h3>"+session.getAttribute("msg1")+"</h3>");
			session.removeAttribute("msg1");
		}
		if (session.getAttribute("msg2") != null) {
			out.print("<h3>"+session.getAttribute("msg2")+"</h3>");
			session.removeAttribute("msg2");
		}			
		}
		else {
			out.print("<h3> ERROR: Session is null.</h3>");
		}
	%>