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
import auxiliar.WorkingFolderClass;
import auxiliar.WorkingFolderClassException;

/**
 * Servlet implementation class SearchServlet
 */
@WebServlet("/DataBasesMenuServlet")
public class DataBasesMenuServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(DataBasesMenuServlet.class);

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
			dataBasesMenu(request, response);
		} catch (Exception e) {
			LOG.error("Exception thrown: ");
			LOG.error(e);
			e.printStackTrace();
			ServletsAuxMethodsClass.goToAuthenticationSignout(request, response, LOG);
		}
	}

	private void dataBasesMenu(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// Ask for the previously created session.
		HttpSession session = request.getSession(false);
		String database;
		String operation;
		
		if (ServletsAuxMethodsClass.client_session_is_not_authenticated(session)) {
			LOG.info("no session. logout.");
			ServletsAuxMethodsClass.goToAuthenticationSignout(request, response, LOG);
		}
		else {
			LOG.info("valid session. Session id: " + session.getId() + " Creation Time" + new Date(session.getCreationTime()) + " Time of Last Access" + new Date(session.getLastAccessedTime()));
			database = request.getParameter("database");
			operation = request.getParameter("op");
			if ((database == null) || (operation == null)) {
				dataBasesMenuAux(session, request, response);
			}
			else {
				if ((operation != null) && ("remove".equals(operation))) {
					removeDataBase(database, session, request, response);
				}
				else {
					dataBasesMenuAux(session, request, response);
				}
			}
		}
	}
	
	private void dataBasesMenuAux(HttpSession session, HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		ServletsAuxMethodsClass.goToDatabasesMenuPage(request, response, LOG);
	}

	/*
	private void performQuery(HttpSession session, HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		ServletsAuxMethodsClass.goToDataBaseQuery(request, response, LOG);
	}
	*/
	
	private void removeDataBase(String database, HttpSession session, HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		
		WorkingFolderClass workingFolder;
		try {
			workingFolder = new WorkingFolderClass();
			String userDisplayName = (String) session.getAttribute("user_display_name");
			workingFolder.removeDataBase(database, userDisplayName);
			request.setAttribute("msg1", "The database "+database+" has been removed. ");
		} catch (WorkingFolderClassException e) {
			LOG.info("Exception: " + e);
			// e.printStackTrace();
			request.setAttribute("msg1", "The database "+database+" could not be removed. ");
		}
		dataBasesMenuAux(session, request, response);
		
	}

}
