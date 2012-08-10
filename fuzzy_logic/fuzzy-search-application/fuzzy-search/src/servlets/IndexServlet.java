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
 * Servlet implementation class IndexServlet
 */
@WebServlet("/")
public class IndexServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(IndexServlet.class);
	private Integer counter = new Integer(0);

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		LOG.info("--- doGet invocation ---");
		doGetOrDoPost(request, response);
		LOG.info("--- doGet end ---");
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		LOG.info("--- doPost invocation ---");
		doGetOrDoPost(request, response);
		LOG.info("--- doPost end ---");
	}
	
	protected void doGetOrDoPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		counter++;
		if (counter == Integer.MAX_VALUE) {
			counter = Integer.valueOf(0);
		}
		
		// Create a new session if the client has no session.
		HttpSession session = request.getSession(true);
		LOG.info("--- doGetOrDoPost invocation. Counter: "+ counter + " <--> Session ID: "+ session.getId() +" ---");
		
		if (ServletsAuxMethodsClass.client_session_is_not_authenticated(session)) {
			LOG.info("session is new. showing index page.");
			ServletsAuxMethodsClass.goToAppIndexPage(request, response, LOG);
		}
		else {
			LOG.info("session is not new. Session id: " + session.getId() + " Creation Time" + new Date(session.getCreationTime()) + " Time of Last Access" + new Date(session.getLastAccessedTime()));
			ServletsAuxMethodsClass.goToAuthenticationSignin(request, response, LOG);
		}
	}
}
