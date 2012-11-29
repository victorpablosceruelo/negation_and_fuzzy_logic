package servlets;

import java.io.IOException;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


import auxiliar.DispatchersClass;
import auxiliar.LocalUserNameClass;
import auxiliar.ServletsAuxMethodsClass;


/**
 * Servlet implementation class SearchServlet
 */
@WebServlet("/DispatcherServlet")
public class DispatcherServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(DispatcherServlet.class);
	
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		doGetAndDoPost("doGet", request, response);
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		doGetAndDoPost("doPost", request, response);
	}
	
	private void doGetAndDoPost(String doAction, HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		LOG.info("--- "+doAction+" invocation ---");
		
		try {
			// Dispatch the query.
			dispatchQuery(doAction, request, response);

		} catch (Exception e) {
			ServletsAuxMethodsClass.actionOnException(ServletsAuxMethodsClass.SignOutRequest, "", e, request, response, LOG);
		}
		LOG.info("--- "+doAction+" end ---");
	}

	private static final int EMPTY_REQUEST = 0;
	private static final int BUILD_QUERY = 1;
	private static final int UPLOAD_FILE = 2;
	private static final int DOWNLOAD_FILE = 3;
	private static final int REMOVE_FILE = 4;
	private static final int VIEW_FILE = 5;
	private static final int DB_INTROSPECTION_QUERY = 6;
	private static final int DB_GENERIC_QUERY = 7;
	
	
	private int dispatcherConversion(String request_op) throws Exception {
		if (request_op == null) throw new Exception("request_op is null");
		
		if ("".equals(request_op)) return EMPTY_REQUEST;
		if ("buildQuery".equals(request_op)) return BUILD_QUERY;
		if ("uploadFile".equals(request_op)) return UPLOAD_FILE;
		if ("downloadFile".equals(request_op)) return DOWNLOAD_FILE;
		if ("removeFile".equals(request_op)) return REMOVE_FILE;
		if ("viewFile".equals(request_op)) return VIEW_FILE;
		if ("dbIntrospectionQuery".equals(request_op)) return DB_INTROSPECTION_QUERY;
		if ("dbGenericQuery".equals(request_op)) return DB_GENERIC_QUERY;
		return EMPTY_REQUEST;
	}
	
	private void dispatchQuery(String doAction, HttpServletRequest request, HttpServletResponse response) throws Exception {
		
		// Ask for an existing session.
		HttpSession session = request.getSession(false);
		if (session == null) throw new Exception("Session is null");

		// Tests if we have logged in.
		LocalUserNameClass localUserName = new LocalUserNameClass(request, response);

		// Needed to work.
		ServletContext servletContext = getServletConfig().getServletContext();
		DispatchersClass dispatcherObject = new DispatchersClass(servletContext, doAction, localUserName, request, response);

		String request_op = request.getParameter("op");
		if (request_op == null) request_op = "default";

		int op = dispatcherConversion(request_op);
		
		switch (op) {
		case UPLOAD_FILE: dispatcherObject.uploadFile();
			break;
		case DOWNLOAD_FILE: dispatcherObject.downloadFile();
			break;
		case REMOVE_FILE: dispatcherObject.removeFile();
			break;
		case VIEW_FILE: dispatcherObject.viewFile();
			break;
		case DB_INTROSPECTION_QUERY: dispatcherObject.dbIntrospectionQuery();
			break;
		case DB_GENERIC_QUERY: dispatcherObject.dbGenericQuery();
			break;		
		case EMPTY_REQUEST:
		case BUILD_QUERY:
		default: dispatcherObject.emptyRequest();
			break;
		}
	}
}
	




