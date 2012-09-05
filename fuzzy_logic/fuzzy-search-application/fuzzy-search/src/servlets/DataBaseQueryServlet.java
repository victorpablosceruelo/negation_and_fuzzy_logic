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
import auxiliar.LocalUserNameFixesClassException;
import auxiliar.ServletsAuxMethodsClass;
import auxiliar.FoldersUtilsClassException;

/**
 * Servlet implementation class DbQueryServlet
 */
@WebServlet("/DataBaseQueryServlet")
public class DataBaseQueryServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private static final Log LOG = LogFactory.getLog(DataBaseQueryServlet.class);
	
	// static private FoldersUtilsClass FoldersUtilsObject = null;
	
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
	

	public void destroy(CiaoPrologConnectionClass connection) {
		LOG.info("Destroy any resoource associated with the Ciao Prolog connection.");
		if (connection != null) {
			connection.connectionTermination();
		}
	}
	
	private void dbQuery(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// Ask for the previously created session.
		HttpSession session = request.getSession(false);
		String database = null;
		String owner = null;
		
		if (ServletsAuxMethodsClass.client_session_is_not_authenticated(session)) {
			LOG.info("no session. logout.");
			ServletsAuxMethodsClass.redirect_to(ServletsAuxMethodsClass.AuthenticationSignout_Page, request, response, LOG);
		}
		else {
			LOG.info("valid session. Session id: " + session.getId() + " Creation Time" + new Date(session.getCreationTime()) + " Time of Last Access" + new Date(session.getLastAccessedTime()));
			database = request.getParameter("database");
			owner = request.getParameter("owner");
			if ((database == null) || (owner == null)) {
				LOG.info("database is null.");
				ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.DataBasesMenuServlet_Page, request, response, LOG);
			}
			else {
				LOG.info("Choosen database and owner are: " + database + " :: " + owner + ".");
				try {
					dbQueryAux(owner, database, session, request, response);
				} catch (Exception e) {
					e.printStackTrace();
					request.setAttribute("msg1", "Exception in dbQuery. Message: \n" + e.getMessage());
					ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.DataBasesMenuServlet_Page, request, response, LOG);
				}
			}
		}
	}
	
	//String localUserName = (String) session.getAttribute("localUserName");
	//connection.changeCiaoPrologWorkingFolder(localUserName);
	
	/**
	 * Connects to prolog server plserver, 
	 * changes the current folder to the one owned by the owner of the database,
	 * asks the database for the available predicates via introspection
	 * and shows the results in a web page.
	 * Offers to the jsp all the collected information.
	 */
	private void dbQueryAux(String owner, String database, 
			HttpSession session, HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException, PLException, FoldersUtilsClassException, LocalUserNameFixesClassException {
		
			CiaoPrologConnectionClass connection = (CiaoPrologConnectionClass) session.getAttribute("connection");
		
			if (connection == null) {
				connection = new CiaoPrologConnectionClass();
			}
			connection.selectDatabase(owner, database);

			// ArrayList<CiaoPrologProgramElementInfoClass> programInfo = 
			session.setAttribute("connection", connection);

			ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.DataBaseQuery_Page, request, response, LOG);
	}
}
