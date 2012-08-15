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
				try {
					dbQueryAux(session, request, response);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
	}
	protected void dbQueryAux(HttpSession session, HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException, PLException, WorkingFolderClassException {
		
		CiaoPrologConnectionClass connection = null; 

		connection = new CiaoPrologConnectionClass();
		String userDisplayName = (String) session.getAttribute("userDisplayName");
		connection.changeCiaoPrologWorkingFolder(userDisplayName);
		ServletsAuxMethodsClass.forward_to("/WEB-INF/dataBaseQuery.jsp", request, response, LOG);
	}
}
