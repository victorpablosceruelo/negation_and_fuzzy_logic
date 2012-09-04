package servlets;

import java.io.IOException;
import java.util.Date;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import auxiliar.ServletsAuxMethodsClass;

/**
 * Servlet implementation class UserInfoServlet
 */
@WebServlet("/UserInfoServlet")
public class UserInfoServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(UserInfoServlet.class);


	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGetOrDoPost(request, response);
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGetOrDoPost(request, response);
	}

	protected void doGetOrDoPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		try {
			HttpSession session = request.getSession(false);
			
			if (ServletsAuxMethodsClass.client_session_is_not_authenticated(session)) {
				LOG.info("no session. logout.");
				ServletsAuxMethodsClass.redirect_to(ServletsAuxMethodsClass.AuthenticationSignout_Page, request, response, LOG);
			}
			else {
				LOG.info("valid session. Session id: " + session.getId() + " Creation Time" + new Date(session.getCreationTime()) + " Time of Last Access" + new Date(session.getLastAccessedTime()));
				ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.UserInfo_Page, request, response, LOG);
				
			}
		} catch (Exception e) {
			LOG.error("Exception thrown: ");
			LOG.error(e);
			e.printStackTrace();
			ServletsAuxMethodsClass.redirect_to(ServletsAuxMethodsClass.AuthenticationSignout_Page, request, response, LOG);
		}
	}
}
