package servlets;

import java.io.IOException;
import java.util.Iterator;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


import auxiliar.FileInfoClass;
import auxiliar.FilesMgmtClass;
import auxiliar.FilesMgmtClass;
import auxiliar.LocalUserNameClass;
import auxiliar.ServletsAuxMethodsClass;


/**
 * Servlet implementation class SearchServlet
 */
@WebServlet("/FilesMgmtServlet")
public class DispatcherServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(DispatcherServlet.class);
	private static FilesMgmtClass filesMgmtAux = null;
	
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
			// Ask for an existing session.
			HttpSession session = request.getSession(false);
			if (session == null) throw new Exception("Session is null");

			// Tests if we have logged in.
			LocalUserNameClass localUserName = new LocalUserNameClass(request, response);

			// Dispatch the query.
			dispatchQuery(doAction, request, response, session, localUserName);

		} catch (Exception e) {
			ServletsAuxMethodsClass.actionOnException(ServletsAuxMethodsClass.SocialAuthServletSignOut, "", e, request, response, LOG);
		}
		LOG.info("--- "+doAction+" end ---");
	}

	private static final int EMPTY_REQUEST = 0;
	private static final int BUILD_QUERY = 1;
	private static final int UPLOAD_FILE = 2;
	private static final int DOWNLOAD_FILE = 3;
	private static final int REMOVE_FILE = 4;
	private static final int VIEW_FILE = 5;
	
	
	private int dispatcherConversion(String request_op) throws Exception {
		if (request_op == null) throw new Exception("request_op is null");
		
		if ("".equals(request_op)) return EMPTY_REQUEST;
		if ("buildQuery".equals(request_op)) return BUILD_QUERY;
		if ("uploadFile".equals(request_op)) return UPLOAD_FILE;
		if ("downloadFile".equals(request_op)) return DOWNLOAD_FILE;
		if ("removeFile".equals(request_op)) return REMOVE_FILE;
		if ("viewFile".equals(request_op)) return VIEW_FILE;
		return EMPTY_REQUEST;
	}
	
	private void dispatchQuery(String doAction, HttpServletRequest request, HttpServletResponse response, HttpSession session, LocalUserNameClass localUserName) 
			throws Exception {
		
		if (filesMgmtAux == null) {
			ServletContext servletContext = getServletConfig().getServletContext();
			filesMgmtAux = new FilesMgmtClass(servletContext);
		}

		String request_op = request.getParameter("op");
		if (request_op == null) request_op = "default";

		int op = dispatcherConversion(request_op);
		
		switch (op) {
		case UPLOAD_FILE:
			filesMgmtAux.uploadFile(doAction, localUserName, request, response);
			break;
		case DOWNLOAD_FILE:
			filesMgmtAux.downloadFile(doAction, localUserName, request, response);
			break;
		case REMOVE_FILE:
			filesMgmtAux.removeFile(doAction, localUserName, request, response);
			break;
		case VIEW_FILE:
			filesMgmtAux.viewFile(doAction, localUserName, request, response);
			break;
		case EMPTY_REQUEST:
		case BUILD_QUERY:
		default:
			Iterator<FileInfoClass> filesIterator = filesMgmtAux.returnFilesIterator(localUserName.getLocalUserName());
			request.setAttribute("filesIterator", filesIterator);
			// Forward to the jsp page.
			ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.FilesMgmtIndexPage, "", request, response, LOG);
			break;
		}
	}
}
	




