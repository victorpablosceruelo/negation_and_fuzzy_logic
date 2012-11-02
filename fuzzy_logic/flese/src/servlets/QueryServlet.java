package servlets;

import java.util.Date;
import java.util.Enumeration;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import CiaoJava.PLStructure;
import CiaoJava.PLVariable;
import auxiliar.AnswerTermInJavaClassException;
import auxiliar.CiaoPrologConnectionClass;
import auxiliar.QueryConversorClass;
import auxiliar.QueryConversorExceptionClass;
import auxiliar.ServletsAuxMethodsClass;

/**
 * Servlet implementation class DbQueryServlet
 */
@WebServlet("/QueryServlet")
public class QueryServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private static final Log LOG = LogFactory.getLog(QueryServlet.class);

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) {
		LOG.info("--- doGet invocation ---");
		doGetAndDoPost(request, response);
		LOG.info("--- doGet end ---");
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) {
		LOG.info("--- doPost invocation ---");
		doGetAndDoPost(request, response);
		LOG.info("--- doPost end ---");
	}
	
	private void doGetAndDoPost(HttpServletRequest request, HttpServletResponse response) {
		try {
			if (ServletsAuxMethodsClass.clientSessionIsAuthenticated(request, response, LOG)) {
				dbQuery(request, response);
			}
		} catch (Exception e) {
			ServletsAuxMethodsClass.actionOnException(e, request, response, LOG);
		}
	}
	
	private void dbQuery(HttpServletRequest request, HttpServletResponse response) throws Exception {
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
					LOG.info(e.getMessage());
					e.printStackTrace();
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
	 * @throws AnswerTermInJavaClassException 
	 */
	private void dbQueryAux(String fileOwner, String fileName, String operation, 
			HttpSession session, HttpServletRequest request, HttpServletResponse response) throws Exception {
		
			// Aqui tendriamos que decidir si hay query o nos limitamos a ejecutar la query "fileNameIntrospectionQuery"
			CiaoPrologConnectionClass connection = (CiaoPrologConnectionClass) session.getAttribute("connection");
			boolean justUpdatedIntrospection = false;
			
			if (connection == null) {
				connection = new CiaoPrologConnectionClass();
				dbQueryAux_Introspection(fileOwner, fileName, connection);
				justUpdatedIntrospection = true;
			}
			
			if (("buildQuery".equals(operation)) && (! justUpdatedIntrospection)) {
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
			throws Exception {
		
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
			throws Exception {
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
	    QueryConversorClass conversor = new QueryConversorClass(queryLinesCounter +1, connection);
	    String msg = "";
	    
	    // Parameters to be retrieved and saved:
	    // quantifier0, quantifier1, predicate, rfuzzyComputeOperator, rfuzzyComputeValue, aggregator;
	    
	    conversor.subqueryEndTestAndSave();
	    msg += conversor.subqueryRetrieveAndSaveSubpart("startupType", request, QueryConversorClass.initialPredicate);
	    msg += conversor.subqueryRetrieveAndSaveSubpart("queryLines.selectAggregator", request, QueryConversorClass.aggregator);
	    
	    for (int i=0; i<queryLinesCounter; i++) {
	    	conversor.subqueryEndTestAndSave();
	    
	    	msg += conversor.subqueryRetrieveAndSaveSubpart("queryLine["+i+"].selectQuantifier_0", request, QueryConversorClass.quantifier0);
	    	msg += conversor.subqueryRetrieveAndSaveSubpart("queryLine["+i+"].selectQuantifier_1", request, QueryConversorClass.quantifier1);
	    	msg += conversor.subqueryRetrieveAndSaveSubpart("queryLine["+i+"].selectPredicate", request, QueryConversorClass.predicate);
	    	msg += conversor.subqueryRetrieveAndSaveSubpart("queryLine["+i+"].selectRfuzzyComputeOperator", request, QueryConversorClass.rfuzzyComputeOperator);
	    	msg += conversor.subqueryRetrieveAndSaveSubpart("queryLine["+i+"].selectRfuzzyComputeValue", request, QueryConversorClass.rfuzzyComputeValue);
	    }
	    LOG.info(msg);
	    
	    conversor.subqueryEndTestAndSave();
	    PLStructure query = conversor.queryConvert();
	    PLVariable [] variables = conversor.getListOfVariables();
	    String [] variablesNames = conversor.getListOfNamesForVariables();
	    request.setAttribute("variablesNames", variablesNames);
	    
	    String querySimpleInfoString = conversor.getQuerySimpleInfoString();
	    String queryComplexInfoString = conversor.getQueryComplexInfoString();
	    request.setAttribute("querySimpleInfoString", querySimpleInfoString);
	    request.setAttribute("queryComplexInfoString", queryComplexInfoString);

	    connection.performQuery(query, fileOwner, fileName, variables);
	    // performQuery(PLStructure query, String fileOwner, String fileName, PLVariable [] variables)
		
	}
	
}








/////