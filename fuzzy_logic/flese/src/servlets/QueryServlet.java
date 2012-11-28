package servlets;

import java.util.Enumeration;

import javax.servlet.ServletContext;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import CiaoJava.PLStructure;
import CiaoJava.PLVariable;
import auxiliar.CiaoPrologConnectionClass;
import auxiliar.LocalUserNameClass;
import auxiliar.QueryConversorClass;
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
		doGetAndDoPost("doGet", request, response);
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) {
		doGetAndDoPost("doPost", request, response);
	}
	
	private void doGetAndDoPost(String doAction, HttpServletRequest request, HttpServletResponse response) {
		LOG.info("--- "+doAction+" invocation ---");
		try {
			dbQuery(request, response);
		} catch (Exception e) {
			ServletsAuxMethodsClass.actionOnException(ServletsAuxMethodsClass.FilesMgmtServlet, "", e, request, response, LOG);
		}
		LOG.info("--- "+doAction+" end ---");
	}
	
	private void dbQuery(HttpServletRequest request, HttpServletResponse response) throws Exception {
		
		LocalUserNameClass localUserName = new LocalUserNameClass(request, response);
		String request_op = request.getParameter("op");
		if (request_op == null) throw new Exception("op is null.");

		String fileName = request.getParameter("fileName");
		if (fileName == null) throw new Exception("fileName is null.");
		
		String fileOwner = request.getParameter("fileOwner");
		if (fileOwner == null) throw new Exception("fileOwner is null.");

		// Ask for the previously created session.
		HttpSession session = request.getSession(false);
		// Aqui tendriamos que decidir si hay query o nos limitamos a ejecutar la query "fileNameIntrospectionQuery"
		CiaoPrologConnectionClass connection = (CiaoPrologConnectionClass) session.getAttribute("connection");
		
		if (connection == null) {
			ServletContext servletContext = getServletConfig().getServletContext();
			connection = new CiaoPrologConnectionClass(servletContext);
		}
		
		// If the program contains errors this will thrown an exception in the highest level.
		buildAndExecuteQueryIntrospection(fileOwner, fileName, connection);
		
		try {
			LOG.info("Choosen fileName, fileOwner and op are: " + fileName + " :: " + fileOwner + " :: " + request_op + " ");
			if ("runQuery".equals(request_op)) {
				buildAndExecuteQueryGeneric(fileOwner, fileName, localUserName, connection, request);
			}
			
			// ArrayList<CiaoPrologProgramElementInfoClass> programInfo = 
			session.setAttribute("connection", connection);

			if ("buildQuery".equals(request_op)) {
				ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.BuildQueryPage, "", request, response, LOG);
			}
			if ("runQuery".equals(request_op)) {
				ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.RunQueryPage, "", request, response, LOG);
			}
		} catch (Exception e) {
			String additionalInfo="?op="+request_op+"&fileName="+fileName+"&fileOwner="+fileOwner;
			ServletsAuxMethodsClass.actionOnException(ServletsAuxMethodsClass.QueryServletBuildQuery, additionalInfo, e, request, response, LOG);
		}
	}
	
	private void buildAndExecuteQueryIntrospection(String fileOwner, String fileName, CiaoPrologConnectionClass connection) 
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
	
	
	private void buildAndExecuteQueryGeneric(String fileOwner, String fileName, LocalUserNameClass localUserName, CiaoPrologConnectionClass connection, HttpServletRequest request) 
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
	    QueryConversorClass conversor = new QueryConversorClass(connection, localUserName.getLocalUserName());
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