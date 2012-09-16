package servlets;

import java.io.IOException;
import java.util.Date;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import auxiliar.LogServerInfoClass;
import auxiliar.ServletsAuxMethodsClass;

/**
 * Servlet implementation class IndexServlet
 */

@WebServlet(urlPatterns={"/IndexServlet"})
public class IndexServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(IndexServlet.class);
	private Integer counter = new Integer(0);
	LogServerInfoClass logServerInfo = null;

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
	
	private void doGetOrDoPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		counter++;
		if (counter == Integer.MAX_VALUE) {
			counter = Integer.valueOf(0);
		}
		LOG.info("Query Info: \n" + 
				 "Request_URL:" + request.getRequestURL().toString() + "\n" +
		         "Request_URI:" + request.getRequestURI().toString() + " ");
		
		if (! serveStaticContent(request, response)) {
			// Log info about the server.
			logServerInfo = new LogServerInfoClass(request);

			// Create a new session if the client has no session.
			HttpSession session = request.getSession(true);
			LOG.info("--- doGetOrDoPost invocation. Counter: "+ counter + " <--> Session ID: "+ session.getId() +" ---");

			if (ServletsAuxMethodsClass.client_session_is_not_authenticated(session)) {
				LOG.info("session is new. showing index page.");
				ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.Index_Page, request, response, LOG);
			}
			else {
				LOG.info("session is not new. Session id: " + session.getId() + " Creation Time" + new Date(session.getCreationTime()) + " Time of Last Access" + new Date(session.getLastAccessedTime()));
				ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.AuthenticationSignin_Page,request, response, LOG);
			}
		}
	}
	
	private boolean serveStaticContent(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		if ((request != null) && (request.getRequestURI() != null)) {
			String requestURI = request.getRequestURI().toString();
			if ((requestURI != null) && (requestURI.startsWith(ServletsAuxMethodsClass.appPath))) {
				requestURI = requestURI.substring(ServletsAuxMethodsClass.appPath.length());
				if (isStaticContent(requestURI)) {
					RequestDispatcher dispatcher = request.getRequestDispatcher(requestURI);
					dispatcher.forward(request, response);
					return true;
				}
				else return false;
			}
			else return false;
		}
		else return false;
	}
	private boolean isStaticContent(String requestURI) {
		if (requestURI == null) return false;
		if ("".equals(requestURI)) return false;
		if (requestURI.startsWith("js/")) return true;
		if (requestURI.startsWith("images/")) return true;
		if ("error.jsp".equals(requestURI)) return true;
		return false;
	}
}
