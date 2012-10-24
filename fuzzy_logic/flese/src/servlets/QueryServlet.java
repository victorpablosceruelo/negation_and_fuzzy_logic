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
import CiaoJava.PLStructure;
import CiaoJava.PLVariable;
import auxiliar.CiaoPrologConnectionClass;
import auxiliar.LocalUserNameFixesClassException;
import auxiliar.QueryConversorClass;
import auxiliar.QueryConversorExceptionClass;
import auxiliar.ServletsAuxMethodsClass;
import auxiliar.FoldersUtilsClassException;

/**
 * Servlet implementation class DbQueryServlet
 */
@WebServlet("/QueryServlet")
public class QueryServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private static final Log LOG = LogFactory.getLog(QueryServlet.class);
	
	// static private FoldersUtilsClass FoldersUtilsObject = null;
	
    /**
     * @see HttpServlet#HttpServlet()
     */
    public QueryServlet() { 
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
	
	private void dbQuery(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// Ask for the previously created session.
		HttpSession session = request.getSession(false);
		String fileName = null;
		String fileOwner = null;
		String operation = null;
		
		if (ServletsAuxMethodsClass.clientSessionIsAuthenticated(request, response, LOG)) {
			LOG.info("valid session. Session id: " + session.getId() + " Creation Time" + new Date(session.getCreationTime()) + " Time of Last Access" + new Date(session.getLastAccessedTime()));
			fileName = request.getParameter("fileName");
			fileOwner = request.getParameter("fileOwner");
			operation = request.getParameter("op");
			if ((fileName == null) || (fileOwner == null) || (operation == null) ||
					((! "buildQuery".equals(operation)) && (! "runQuery".equals(operation)))) {
				LOG.info("operation: "+operation+" fileName: "+fileName+" fileOwner: "+fileOwner);
				ServletsAuxMethodsClass.addMessageToTheUser(request, "Incorrect arguments for request.", LOG);
				ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.FilesMgmtServlet, request, response, LOG);
			}
			else {
				LOG.info("Choosen fileName, fileOwner and op are: " + fileName + " :: " + fileOwner + " :: " + operation + " ");

				try {
					dbQueryAux(fileOwner, fileName, operation, session, request, response);
				} catch (Exception e) {
					e.printStackTrace();
					ServletsAuxMethodsClass.addMessageToTheUser(request, e.getMessage(), LOG);
					ServletsAuxMethodsClass.addMessageToTheUser(request, "Exception in dbQuery. Message: \n" + e.getMessage(), LOG);
					ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.FilesMgmtServlet, request, response, LOG);
				}
			}
		}
	}
	
	//String localUserName = (String) session.getAttribute("localUserName");
	//connection.changeCiaoPrologWorkingFolder(localUserName);
	
	/**
	 * Connects to prolog server plserver, 
	 * changes the current folder to the one owned by the fileOwner of the fileName,
	 * asks the fileName for the available predicates via introspection
	 * and shows the results in a web page.
	 * Offers to the jsp all the collected information.
	 * @throws QueryConversorExceptionClass 
	 */
	private void dbQueryAux(String fileOwner, String fileName, String operation, 
			HttpSession session, HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException, PLException, FoldersUtilsClassException, LocalUserNameFixesClassException, QueryConversorExceptionClass {
		
			// Aqui tendriamos que decidir si hay query o nos limitamos a ejecutar la query "fileNameIntrospectionQuery"
			CiaoPrologConnectionClass connection = (CiaoPrologConnectionClass) session.getAttribute("connection");
		
			if (connection == null) {
				connection = new CiaoPrologConnectionClass();
			}
			
			if ("buildQuery".equals(operation)) {
				dbQueryAux_Introspection(fileOwner, fileName, connection);
			}
			if ("runQuery".equals(operation)) {
				build_and_execute_query(fileOwner, fileName, connection, request);
			}

			// ArrayList<CiaoPrologProgramElementInfoClass> programInfo = 
			session.setAttribute("connection", connection);

			if ("buildQuery".equals(operation)) {
				ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.BuildQueryPage, request, response, LOG);
			}
			if ("runQuery".equals(operation)) {
				ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.RunQueryPage, request, response, LOG);
			}
	}
	
	private void dbQueryAux_Introspection(String fileOwner, String fileName, CiaoPrologConnectionClass connection) 
			throws ServletException, IOException, PLException, FoldersUtilsClassException, LocalUserNameFixesClassException {
		
		connection.programFileIntrospectionQuery(fileOwner, fileName);
		/*
		LOG.info("------");
		LOG.info("------");
		LOG.info("--------> testing query !!! <-----------");
		LOG.info("------");
		LOG.info("------");
		connection.testingQuery(fileOwner, fileName);
		*/
	}
	
	
	private void build_and_execute_query(String fileOwner, String fileName, CiaoPrologConnectionClass connection, HttpServletRequest request) 
			throws ServletException, IOException, PLException, FoldersUtilsClassException, LocalUserNameFixesClassException, QueryConversorExceptionClass {
		LOG.info("build_and_execute_query call.");
		
		String formParameters = " --- Parameters Names and Values --- \n";
	    Enumeration<String> paramNames = request.getParameterNames();
	    while(paramNames.hasMoreElements()) {
	    	String paramName = (String)paramNames.nextElement();
	    	String[] paramValues = request.getParameterValues(paramName);
	    	for(int i=0; i<paramValues.length; i++) {
	    		formParameters += "paramName: " + paramName + " paramValue: " + paramValues[i] + " \n";
	    	}
	    }
	    LOG.info(formParameters);
	    
	    int queryLinesCounter = Integer.parseInt(request.getParameter("queryLinesCounter"));
	    QueryConversorClass conversor = new QueryConversorClass(queryLinesCounter);
	    
	    int numOfArguments;
	    String [][] arguments;
	    String quantifier0, quantifier1, fuzzyPredicate;
	    for (int i=0; i<queryLinesCounter; i++) {
	    	quantifier0 = request.getParameter("fuzzyRuleQuantifier["+i+"][0]");
	    	quantifier1 = request.getParameter("fuzzyRuleQuantifier["+i+"][1]");
	    	fuzzyPredicate = request.getParameter("fuzzyRule["+i+"]");
	    
	    	numOfArguments = 0;
	    	while (request.getParameter("fuzzyRuleArgument["+i+"]["+numOfArguments+"]") != null) {
	    		numOfArguments++;
	    	}
	    	arguments = new String[numOfArguments][];
	    	for (int j=0; j<numOfArguments; j++) {
	    		arguments[j] = new String[2];
	    		if ("constant".equals(request.getParameter("fuzzyRuleArgument["+i+"]["+j+"]"))) {
	    			arguments[j][0] = "constant";
	    			arguments[j][1] = request.getParameter("fuzzyRuleArgumentConstant["+i+"]["+j+"]");
	    		}
	    		else {
	    			arguments[j][0] = "variable";
	    			arguments[j][1] = request.getParameter("fuzzyRuleArgument["+i+"]["+j+"]");
	    		}
	    	}
	    	
	    	conversor.addSubQuery(quantifier0, quantifier1, fuzzyPredicate, arguments);
	    }
	    
	    PLStructure query = conversor.getFinalQuery();
	    PLVariable [] variables = conversor.getFinalQueryVariables();

	    connection.performQuery(query, fileOwner, fileName, variables);
	    // performQuery(PLStructure query, String fileOwner, String fileName, PLVariable [] variables)
		
	}
	
}








/////