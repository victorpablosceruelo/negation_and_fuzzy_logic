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
			ServletsAuxMethodsClass.actionOnException(ServletsAuxMethodsClass.ExceptionAjaxPage, "", e, request, response, LOG);
		}
		LOG.info("--- "+doAction+" end ---");
	}	
	
	private void dispatchQuery(String doAction, HttpServletRequest request, HttpServletResponse response) throws Exception {

		// Ask for an existing session.
		HttpSession session = request.getSession(false);
		if (session == null) {
			ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.NullSessionAjaxPage, "", request, response, LOG);
		}
		else {
			dispatchQueryWithSession(doAction, request, response, session);
		}

	}
	private void dispatchQueryWithSession(String doAction, HttpServletRequest request, HttpServletResponse response, HttpSession session) throws Exception {
		
		if (session == null) throw new Exception("Session is null");
			
		// Tests if we have logged in.
		LocalUserNameClass localUserName = new LocalUserNameClass(request, response);

		// Needed to work.
		ServletContext servletContext = getServletConfig().getServletContext();
		DispatchersClass dispatcherObject = new DispatchersClass(servletContext, doAction, localUserName, request, response);

		String request_op = request.getParameter("op");
		if (request_op == null) request_op = "default";

		int op = ServletsAuxMethodsClass.uriNickNameForOpValue(request_op);
		
		switch (op) {
		case ServletsAuxMethodsClass.UserOptionsRequest :
			ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.UserOptionsAnswer, "", request, response, LOG);
			break;
		case ServletsAuxMethodsClass.ListProgramFuzzificationsRequest:
			dispatcherObject.listProgramFuzzifications();
			break;
		case ServletsAuxMethodsClass.SaveProgramFuzzificationRequest:
			dispatcherObject.saveProgramFuzzification();
			break;
		case ServletsAuxMethodsClass.FilesListRequest: dispatcherObject.filesList();
			break;
		case ServletsAuxMethodsClass.FileUploadRequest: dispatcherObject.uploadFile();
			break;
		case ServletsAuxMethodsClass.FileDownloadRequest: dispatcherObject.downloadFile();
			break;
		case ServletsAuxMethodsClass.FileRemoveRequest: dispatcherObject.removeFile();
			break;
		case ServletsAuxMethodsClass.FileViewRequest: dispatcherObject.viewFile();
			break;
		case ServletsAuxMethodsClass.ProgramFileIntrospectionRequest: dispatcherObject.dbIntrospectionQuery(true);
			break;
		case ServletsAuxMethodsClass.RunQueryRequest: dispatcherObject.dbGenericQuery();
			break;
		case ServletsAuxMethodsClass.TheSamePage:
		default: dispatcherObject.emptyRequest();
			break;
		}
	}
}
	




