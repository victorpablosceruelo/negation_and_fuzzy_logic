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


import auxiliar.FilesMgmtClass;
import auxiliar.ServletsAuxMethodsClass;


/**
 * Servlet implementation class SearchServlet
 */
@WebServlet("/FilesMgmtServlet")
public class FilesMgmtServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	final Log LOG = LogFactory.getLog(FilesMgmtServlet.class);
	private static FilesMgmtClass fileMgmtAux = null;
	
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		LOG.info("--- doGet invocation ---");
		doGetAndDoPost("doGet", request, response);
		LOG.info("--- doGet end ---");
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		LOG.info("--- doPost invocation ---");
		doGetAndDoPost("doPost", request, response);
		LOG.info("--- doPost end ---");	
	}
	
	private void doGetAndDoPost(String doMethod, HttpServletRequest request, HttpServletResponse response) 
			throws ServletException, IOException {
		try {
			if (fileMgmtAux == null) {
				ServletContext servletContext = getServletConfig().getServletContext();
				fileMgmtAux = new FilesMgmtClass(servletContext);
			}
			fileMgmtServlet(doMethod, request, response);
		} catch (Exception e) {
			ServletsAuxMethodsClass.actionOnException(e, request, response, LOG);
		}
	}

	private void fileMgmtServlet(String doMethod, HttpServletRequest request, HttpServletResponse response) throws Exception {
		// Ask for the previously created session.
		HttpSession session = request.getSession(false);
		
		if (ServletsAuxMethodsClass.clientSessionIsAuthenticated(request, response, LOG)) {
			String request_op = request.getParameter("op");
			LOG.info("op: " + request_op);
			if (request_op != null) {
				if ("upload".equals(request_op)) {
					fileMgmtAux.uploadFile(doMethod, session, request, response);
				}
				if ("download".equals(request_op)) {
					fileMgmtAux.downloadFile(doMethod, session, request, response);
				}
				if ("remove".equals(request_op)) {
					fileMgmtAux.removeFile(doMethod, session, request, response);
				}
				if ("view".equals(request_op)) {
					fileMgmtAux.viewFile(doMethod, session, request, response);
				}
				
				if ((! "upload".equals(request_op)) && (! ("download".equals(request_op))) &&
					     (! ("remove".equals(request_op))) && (! ("view".equals(request_op)))) {
					LOG.info("Strange op in request. op: " + request_op);
				}
			}

			if ((request_op == null) || ((! ("download".equals(request_op))) && (! ("view".equals(request_op))))) {
				ServletsAuxMethodsClass.forward_to(ServletsAuxMethodsClass.FilesMgmtIndexPage, request, response, LOG);
			}
		}
	}
}



