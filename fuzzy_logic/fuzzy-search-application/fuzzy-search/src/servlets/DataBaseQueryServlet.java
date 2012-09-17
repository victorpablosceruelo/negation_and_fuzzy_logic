package servlets;

import java.io.IOException;
import java.util.Date;
import java.util.Enumeration;

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
		String operation = null;
		
		if (ServletsAuxMethodsClass.client_session_is_not_authenticated(session)) {
			LOG.info("no session. logout.");
			ServletsAuxMethodsClass.redirect_to(ServletsAuxMethodsClass.AuthenticationSignout_Page, request, response, LOG);
		}
		else {
			LOG.info("valid session. Session id: " + session.getId() + " Creation Time" + new Date(session.getCreationTime()) + " Time of Last Access" + new Date(session.getLastAccessedTime()));
			database = request.getParameter("database");
			owner = request.getParameter("owner");
			operation = request.getParameter("op");
			if ((database == null) || (owner == null) || (operation == null) ||
					((! "query".equals(operation)) && (! "runquery".equals(operation)))) {
				LOG.info("database is null.");
				ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.DataBasesMenuServlet_Page, request, response, LOG);
			}
			else {
				LOG.info("Choosen database, owner and op are: " + database + " :: " + owner + " :: " + operation + " ");

				try {
					dbQueryAux(owner, database, operation, session, request, response);
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
	private void dbQueryAux(String owner, String database, String operation, 
			HttpSession session, HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException, PLException, FoldersUtilsClassException, LocalUserNameFixesClassException {
		
			// Aqui tendriamos que decidir si hay query o nos limitamos a ejecutar la query "databaseIntrospectionQuery"
			CiaoPrologConnectionClass connection = (CiaoPrologConnectionClass) session.getAttribute("connection");
		
			if (connection == null) {
				connection = new CiaoPrologConnectionClass();
			}
			
			if ("query".equals(operation)) {
				dbQueryAux_Introspection(owner, database, connection);
			}
			if ("runquery".equals(operation)) {
				build_and_execute_query(request, response);
			}

			// ArrayList<CiaoPrologProgramElementInfoClass> programInfo = 
			session.setAttribute("connection", connection);

			if ("query".equals(operation)) {
				ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.DataBaseQuery_Page, request, response, LOG);
			}
			if ("runquery".equals(operation)) {
				ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.DataBaseQueryResults_Page, request, response, LOG);
			}
	}
	
	private void dbQueryAux_Introspection(String owner, String database, CiaoPrologConnectionClass connection) 
			throws ServletException, IOException, PLException, FoldersUtilsClassException, LocalUserNameFixesClassException {
		
		connection.databaseIntrospectionQuery(owner, database);
		LOG.info("------");
		LOG.info("------");
		LOG.info("--------> testing query !!! <-----------");
		LOG.info("------");
		LOG.info("------");
		connection.testingQuery(owner, database);
	}
	
	
	private void build_and_execute_query(HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		int counter = 0;
		LOG.info("build_and_execute_query call.");
		while (request.getParameter("fuzzyRule[" + counter + "]") != null) {
			LOG.info("fuzzyRule[" + counter + "]:" + request.getParameter("fuzzyRule[" + counter + "]"));
			counter++;
		}
		
		String formParameters = " --- \n";
	    Enumeration<String> paramNames = request.getParameterNames();
	    while(paramNames.hasMoreElements()) {
	    	String paramName = (String)paramNames.nextElement();
	    	String[] paramValues = request.getParameterValues(paramName);
	    	for(int i=0; i<paramValues.length; i++) {
	    		formParameters += "paramName: " + paramName + " paramValue: " + paramValues[i] + " \n";
	    	}
	    }
	    LOG.info(formParameters);
	    
		/*
		String [][] query = new String [counter][];
		for (int i=0; i<counter; i++) {
			query[i] = new String[]
		}*/
		
	}
	
}








/////