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

import CiaoJava.PLException;
import auxiliar.CiaoPrologConnectionClass;
import auxiliar.ServletsAuxMethodsClass;
import auxiliar.WorkingFolderClassException;

/**
 * Servlet implementation class DbQueryServlet
 */
@WebServlet("/DataBaseQueryServlet")
public class DataBaseQueryServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private static final Log LOG = LogFactory.getLog(DataBaseQueryServlet.class);
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public DataBaseQueryServlet() {
        super();
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		LOG.info("--- doGet invocation ---");
		dbQuery(request, response);
		LOG.info("--- doGet end ---");
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		LOG.info("--- doPost invocation ---");
		dbQuery(request, response);
		LOG.info("--- doPost end ---");
	}
	
	protected void dbQuery(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// Ask for the previously created session.
		HttpSession session = request.getSession(false);
		String database = null;
		
		if (ServletsAuxMethodsClass.client_session_is_not_authenticated(session)) {
			LOG.info("no session. logout.");
			ServletsAuxMethodsClass.goToAuthenticationSignout(request, response, LOG);
		}
		else {
			LOG.info("valid session. Session id: " + session.getId() + " Creation Time" + new Date(session.getCreationTime()) + " Time of Last Access" + new Date(session.getLastAccessedTime()));
			database = request.getParameter("database");
			if (database == null) {
				LOG.info("database is null.");
				ServletsAuxMethodsClass.goToDataBasesMenu(request, response, LOG);
			}
			else {
				LOG.info("database to be used is " + database + ".");
				dbQueryAux(session, request, response);
			}
		}
	}
	protected void dbQueryAux(HttpSession session, HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		CiaoPrologConnectionClass connection = null; 
		try {
			connection = new CiaoPrologConnectionClass();
		} catch (PLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			connection = null;
		} catch (WorkingFolderClassException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			connection = null;
		}
		String userDisplayName = (String) session.getAttribute("user_display_name");
		try {
			connection.changeCiaoPrologWorkingFolder(userDisplayName);
		} catch (WorkingFolderClassException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (PLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		ServletsAuxMethodsClass.goToDataBaseQueryPage(request, response, LOG);
	}
}
