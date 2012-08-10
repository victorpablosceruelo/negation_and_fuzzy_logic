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
 * Servlet implementation class SearchServlet
 */
@WebServlet("/SearchServlet")
public class SearchServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(SearchServlet.class);

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		LOG.info("--- doGet invocation ---");
		doGetAndDoPost(request, response);
		LOG.info("--- doGet end ---");
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		LOG.info("--- doPost invocation ---");
		doGetAndDoPost(request, response);
		LOG.info("--- doPost end ---");	
	}
	
	private void doGetAndDoPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, java.io.IOException {
		try {
			selectDbOrQuery(request, response);
		} catch (Exception e) {
			LOG.error("Exception thrown: ");
			LOG.error(e);
			e.printStackTrace();
			ServletsAuxMethodsClass.goToAuthenticationSignout(request, response, LOG);
		}
	}

	private void selectDbOrQuery(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// Ask for the previously created session.
		HttpSession session = request.getSession(false);
		String database;
		
		if (ServletsAuxMethodsClass.client_session_is_not_authenticated(session)) {
			LOG.info("no session. logout.");
			ServletsAuxMethodsClass.goToAuthenticationSignout(request, response, LOG);
		}
		else {
			LOG.info("valid session. Session id: " + session.getId() + " Creation Time" + new Date(session.getCreationTime()) + " Time of Last Access" + new Date(session.getLastAccessedTime()));
			database = request.getParameter("database");
			if (database == null) {
				selectDb(request, response);
			}
			else {
				performQuery(request, response);
			}
		}
	}
	
	private void selectDb(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		ServletsAuxMethodsClass.goToSelectDatabasePage(request, response, LOG);
	}
	private void performQuery(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		ServletsAuxMethodsClass.goToPerformQueryPage(request, response, LOG);
	}

}
